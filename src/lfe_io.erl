%% Copyright (c) 2008-2013 Robert Virding
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% File    : lfe_io.erl
%% Author  : Robert Virding
%% Purpose : Some basic i/o functions for Lisp Flavoured Erlang.
%%
%% The io functions have been split into the following modules:
%% lfe_io        - basic read and write functions
%% lfe_io_write  - basic write functions
%% lfe_io_pretty - basic print functions
%% lfe_io_format - formatted output

-module(lfe_io).

-export([parse_file/1,read_file/1,read/0,read/1,read_string/1]).
-export([print/1,print/2,print1/1,print1/2]).
-export([prettyprint/1,prettyprint/2,
         prettyprint1/1,prettyprint1/2,prettyprint1/3,prettyprint1/4]).
-export([format/2,format/3,fwrite/2,fwrite/3,
         format1/2,fwrite1/2]).

%% -compile(export_all).

-import(lists, [flatten/1,reverse/1,reverse/2,map/2,mapfoldl/3,all/2]).

%% Define IS_MAP/1 macro for is_map/1 bif.
-ifdef(HAS_MAPS).
-define(IS_MAP(T), is_map(T)).
-else.
-define(IS_MAP(T), false).
-endif.

%% parse_file(FileName) -> {ok,[{Sexpr,Line}]} | {error,Error}.
%%  Parse a file returning the raw sexprs (as it should be) and line
%%  numbers of start of each sexpr. Handle errors consistently.

parse_file(Name) ->
    with_token_file(Name, fun (Ts) -> parse_file1(Ts, [], []) end).

parse_file1([_|_]=Ts0, Pc0, Ss) ->
    case lfe_parse:sexpr(Pc0, Ts0) of
        {ok,L,S,Ts1} -> parse_file1(Ts1, [], [{S,L}|Ss]);
        {more,Pc1} ->
            %% Need more tokens but there are none, so call again to
            %% generate an error message.
            {error,E,_} = lfe_parse:sexpr(Pc1, {eof,99999}),
            {error,E};
        {error,E,_} -> {error,E}
    end;
parse_file1([], _, Ss) -> {ok,reverse(Ss)}.

%% read_file(FileName) -> {ok,[Sexpr]} | {error,Error}.
%%  Read a file returning the raw sexprs (as it should be).

read_file(Name) ->
    with_token_file(Name, fun (Ts) -> read_file1(Ts, []) end).

read_file1([_|_]=Ts0, Ss) ->
    case lfe_parse:sexpr(Ts0) of
        {ok,_,S,Ts1} -> read_file1(Ts1, [S|Ss]);
        {more,Pc1} ->
            %% Need more tokens but there are none, so call again to
            %% generate an error message.
            {error,E,_} = lfe_parse:sexpr(Pc1, {eof,99999}),
            {error,E};
        {error,E,_} -> {error,E}
    end;
read_file1([], Ss) -> {ok,reverse(Ss)}.

%% with_token_file(FileName, DoFunc)
%%  Open the file, scan all LFE tokens and apply DoFunc on them.

with_token_file(Name, Do) ->
    case file:open(Name, [read]) of
        {ok,F} ->
            Ret = case io:request(F, {get_until,'',lfe_scan,tokens,[1]}) of
                      {ok,Ts,_} -> Do(Ts);
                      {error,Error,_} -> {error,Error}
                  end,
            file:close(F),                      %Close the file
            Ret;                                % and return value
        {error,Error} -> {error,{none,file,Error}}
    end.

%% read() -> {ok,Sexpr} | {error,Error}.
%% read(Prompt) -> {ok,Sexpr} | {error,Error}.
%% read(IoDevice, Prompt) -> {ok,Sexpr} | {error,Error}.
%%  A very simple read function. Line oriented and cannot handle
%%  tokens over line-breaks but can handle multiple lines. Anything
%%  remaining on last line after a sexpr is lost. Signal errors.

%% read() -> read(standard_io, '').
%% read(Prompt) -> read(standard_io, Prompt).
%% read(Io, Prompt) ->
%%     scan_and_parse(Io, Prompt, [], 1).
read() -> read(standard_io).
read(Io) ->
    scan_and_parse(Io, '', [], 1).

scan_and_parse(Io, Prompt, Pc0, L) ->
    case io:get_line(Io, Prompt) of
        {error,E} -> {error,{L,lfe_parse,E}};
        eof ->
            %% No more so must take what we have.
            case lfe_parse:sexpr(Pc0, {eof,L}) of
                {ok,_,S,_} -> {ok,S};
                {error,E,_} -> {error,E}
            end;
        Cs ->
            case lfe_scan:string(Cs, L) of
                {ok,[],_} ->
                    %% Empty line (token free) just go on.
                    scan_and_parse(Io, Prompt, Pc0, L+1);
                {ok,Ts,_} ->
                    case lfe_parse:sexpr(Pc0, Ts) of
                        {ok,_,S,_} -> {ok,S};
                        {more,Pc1} -> scan_and_parse(Io, Prompt, Pc1, L+1);
                        {error,E,_} -> {error,E}
                    end;
                {error,E,_} -> {error,E}
            end
    end.

%% read_string(String) -> {ok,Sexpr} | {error,Error}.
%%  Read a string.

read_string(Cs) ->
    case lfe_scan:string(Cs, 1) of
        {ok,Ts,L} ->
            case lfe_parse:sexpr(Ts ++ {eof,L}) of
                {ok,_,S,_} -> {ok,S};
                {error,E,_} -> {error,E}
            end;
        {error,E,_} -> {error,E}
    end.

%% print([IoDevice], Sexpr) -> ok.
%% print1(Sexpr) -> [char()].
%% print1(Sexpr, Depth) -> [char()].
%%  A simple print function. Does not pretty-print but stops at Depth.

print(S) -> print(standard_io, S).
print(Io, S) -> io:put_chars(Io, print1(S)).

print1(S) -> print1(S, -1).                     %All the way
print1(S, D) -> lfe_io_write:term(S, D).

%% prettyprint([IoDevice], Sexpr) -> ok.
%% prettyprint1(Sexpr, Depth, Indentation, LineLength) -> [char()].
%%  External interface to the prettyprint functions.

prettyprint(S) -> prettyprint(standard_io, S).
prettyprint(Io, S) -> io:put_chars(Io, prettyprint1(S, -1)).

prettyprint1(S) -> lfe_io_pretty:term(S).
prettyprint1(S, D) -> lfe_io_pretty:term(S, D, 0, 80).
prettyprint1(S, D, I) -> lfe_io_pretty:term(S, D, I, 80).
prettyprint1(S, D, I, L) -> lfe_io_pretty:term(S, D, I, L).

%% format([IoDevice,] Format, Args) -> ok.
%% fwrite([IoDevice,] Format, Args) -> ok.
%% format1(Format, Args) -> [char()].
%% fwrite1(Format, Args) -> [char()].
%%  External interface to the formated output functions.

format(F, As) -> format(standard_io, F, As).
format(Io, F, As) -> io:put_chars(Io, format1(F, As)).

format1(F, As) -> fwrite1(F, As).

fwrite(F, As) -> fwrite(standard_io, F, As).
fwrite(Io, F, As) -> io:put_chars(Io, fwrite1(F, As)).

fwrite1(F, As) ->
    case catch lfe_io_format:fwrite1(F, As) of
        {'EXIT',_} ->                           %Something went wrong
            erlang:error(badarg, [F,As]);       %Signal from here
        Result -> Result
    end.
