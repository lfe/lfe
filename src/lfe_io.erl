%% Copyright (c) 2008-2016 Robert Virding
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

-export([parse_file/1,read_file/1]).
-export([read/0,read/1,read/2,read_line/0,read_line/1,read_line/2]).
-export([read_string/1]).
-export([scan_sexpr/2,scan_sexpr/3]).
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
            Ret = case io:request(F, {get_until,unicode,'',lfe_scan,tokens,[1]}) of
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
%%  A simple read function. It is not line oriented and stops as soon
%%  as it has consumed enough.

read() -> read(standard_io, '').
read(Prompt) -> read(standard_io, Prompt).
read(Io, Prompt) ->
    case io:request(Io, {get_until,unicode,Prompt,?MODULE,scan_sexpr,[1]}) of
        {ok,Sexpr,_} -> {ok,Sexpr};
        {error,E} -> {error,{1,io,E}};
        {error,Error,_} -> {error,Error};
        {eof,_} -> eof
    end.

%% read_line() -> {ok,Sexpr} | {error,Error}.
%% read_line(Prompt) -> {ok,Sexpr} | {error,Error}.
%% read_line(IoDevice, Prompt) -> {ok,Sexpr} | {error,Error}.
%%  A simple read function. It is line oriented and reads whole lines
%%  until it has consumed enough characters. Left-over characters in
%%  the last line are discarded.

read_line() -> read_line(standard_io, '').
read_line(Prompt) -> read_line(standard_io, Prompt).
read_line(Io, Prompt) ->
    %% We input lines and call scan_sexpr directly ourself.
    read_line_1(Io, Prompt, [], 1).

read_line_1(Io, P, C0, L0) ->
    case io:get_line(Io, P) of
        {error,Error} -> {error,{L0,io,Error}};
        Cs0 ->
            case scan_sexpr(C0, Cs0, L0) of
                {done,{ok,Ret,_L1},_Cs1} -> {ok,Ret};
                {done,{error,Error,_},_Cs1} -> {error,Error};
                {more,C1} ->
                    read_line_1(Io, P, C1, L0)
            end
    end.

%% scan_sexpr(Continuation, Chars) ->
%% scan_sexpr(Continuation, Chars, Line) ->
%%     {done,Ret,Rest} | {more,Continuation}.
%%  This function is a re-entrant call which scans tokens from the
%%  input and parses a sexpr. If there are enough characters then it
%%  returns {done,...} else {cont,Cont} if it needs more characters.
%%  This is continued until a sexpr has been scanned.

scan_sexpr([], Cs) ->
    scan_sexpr({[],[]}, Cs, 1).

scan_sexpr([], Cs, L) ->
    scan_sexpr({[],[]}, Cs, L);
scan_sexpr({Sc,Pc}, Cs, L) ->
    scan_sexpr_1(Sc, Pc, Cs, L).

scan_sexpr_1(Sc0, Pc0, Cs0, L0) ->
    case lfe_scan:token(Sc0, Cs0, L0) of
        {done,{error,_,_},_}=Error -> Error;
        {done,{ok,T,L1},Cs1} ->
            %% We have a token, now check if we have a sexpr.
            case lfe_parse:sexpr(Pc0, [T]) of
                {ok,L2,Sexpr,_} ->
                    {done,{ok,Sexpr,L2},Cs1};
                {error,Error,_} ->
                    {done,{error,Error,Cs1},Cs1};
                {more,Pc1} ->                   %Need more tokens
                    scan_sexpr_1([], Pc1, Cs1, L1)
            end;
        {done,{eof,_},_}=Eof -> Eof;
        {more,Sc1} ->
            {more,{Sc1,Pc0}}
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
