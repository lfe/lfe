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
%% lfe_io_pretty - sexpr prettyprinter
%% lfe_io_format - formatted output

-module(lfe_io).

-export([parse_file/1,read_file/1,read/0,read/1,read_string/1,
     print/1,print/2,print1/1,print1/2]).
-export([prettyprint/1,prettyprint/2,
     prettyprint1/1,prettyprint1/2,prettyprint1/3,prettyprint1/4]).
-export([format/2,format/3,fwrite/2,fwrite/3,
     format1/2,fwrite1/2]).

-export([print1_symb/1,print1_string/2,print1_bits/2]).

%% -compile(export_all).

-import(lists, [flatten/1,reverse/1,reverse/2,map/2,mapfoldl/3,all/2]).

%% Define IS_MAP/1 macro for is_map/1 bif.
-ifdef(HAS_MAPS).
-define(IS_MAP(T), is_map(T)).
-else.
-define(IS_MAP(T), false).
-endif.

%% parse_file(FileName) -> {ok,[{Sexpr,Line}]} | {error,Error}.
%% Parse a file returning the raw sexprs (as it should be) and line
%% numbers of start of each sexpr. Handle errors consistently.

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
%% Read a file returning the raw sexprs (as it should be).

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

%% read([IoDevice]) -> {ok,Sexpr} | {error,Error}.
%%  A very simple read function. Line oriented and cannot handle
%%  tokens over line-breaks but can handle multiple lines. Anything
%%  remaining on last line after a sexpr is lost. Signal errors.

read() -> read(standard_io).
read(Io) ->
    scan_and_parse(Io, [], 1).

scan_and_parse(Io, Pc0, L) ->
    case io:get_line(Io, '') of
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
                    scan_and_parse(Io, Pc0, L+1);
                {ok,Ts,_} ->
                    case lfe_parse:sexpr(Pc0, Ts) of
                        {ok,_,S,_} -> {ok,S};
                        {more,Pc1} -> scan_and_parse(Io, Pc1, L+1);
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

print1(_, 0) -> "...";
print1(Symb, _) when is_atom(Symb) -> print1_symb(Symb);
print1(Numb,_ ) when is_integer(Numb) -> integer_to_list(Numb);
print1(Numb, _) when is_float(Numb) -> io_lib_format:fwrite_g(Numb);
print1(List, D) when is_list(List) ->
    [$(,print1_list(List, D-1),$)];
print1({}, _) -> "#()";
print1(Vec, D) when is_tuple(Vec) ->
    Es = tuple_to_list(Vec),
    ["#(",print1_list(Es, D-1),")"];
print1(Bit, _) when is_bitstring(Bit) ->
    ["#B(",print1_bits(Bit),$)];
print1(Map, D) when ?IS_MAP(Map) -> print1_map(Map, D);
print1(Other, D) ->                             %Use standard Erlang for rest
    io_lib:write(Other, D).

%% print1_symb(Symbol) -> [char()].

print1_symb(Symb) ->
    Cs = atom_to_list(Symb),
    case quote_symbol(Symb, Cs) of
    true -> print1_string(Cs , $|);
    false -> Cs
    end.

%% print1_bits(Bitstring) -> [char()]
%% print1_bits(Bitstring, Depth) -> [char()]
%%  Print the bytes in a bitstring. Print bytes except for last which
%%  we add size field if not 8 bits big.

print1_bits(Bits) -> print1_bits(Bits, -1).     %Print them all

print1_bits(_, 0) -> "...";
print1_bits(<<B:8>>, _) -> integer_to_list(B);  %Catch last binary byte
print1_bits(<<B:8,Bits/bitstring>>, N) ->
    [integer_to_list(B),$\s|print1_bits(Bits, N-1)];
print1_bits(<<>>, _) -> [];
print1_bits(Bits, _) ->                         %0 < Size < 8
    N = bit_size(Bits),
    <<B:N>> = Bits,
    io_lib:format("(~w (size ~w))", [B,N]).

%% print1_list(List, Depth) -> Chars.
%% Print the elements in a list. We handle the empty list and depth=0.

print1_list([], _) -> [];
print1_list(_, 0) -> "...";
print1_list([Car|Cdr], D) ->
    [print1(Car, D)|print1_tail(Cdr, D-1)].

%% print1_tail(Tail, Depth)
%% Print the tail of a list decrasing the depth for each element. We
%% know about dotted pairs.

print1_tail([], _) -> "";
print1_tail(_, 0) -> [$\s|"..."];
print1_tail([S|Ss], D) ->
    [$\s,print1(S, D)|print1_tail(Ss, D-1)];
print1_tail(S, D) -> [" . "|print1(S, D)].

%% print1_map(Map, Depth)

print1_map(Map, D) ->
    [$#,$M,$(,print1_map_body(maps:to_list(Map), D), $)].

print1_map_body([], _) -> [];
print1_map_body(_, D) when D =:= 0; D =:= 1 -> "...";
print1_map_body([KV], D) -> print1_map_assoc(KV, D);
print1_map_body([KV|KVs], D) ->
    Massoc = print1_map_assoc(KV, D),
    [Massoc,$\s|print1_map_body(KVs, D-1)].

print1_map_assoc({K,V}, D) ->
    [print1(K, D-1),$\s,print1(V, D-1)].

%% quote_symbol(Symbol, SymbChars) -> bool().
%%  Check if symbol needs to be quoted when printed. If it can read as
%%  a number then it must be quoted.

quote_symbol('.', _) -> true;                   %Needs quoting
quote_symbol(_, [C|Cs]=Cs0) ->
    case catch {ok,list_to_float(Cs0)} of
    {ok,_} -> true;
    _ -> case catch {ok,list_to_integer(Cs0)} of
         {ok,_} -> true;
         _ -> not (start_symb_char(C) andalso symb_chars(Cs))
         end
    end;
quote_symbol(_, []) -> true.

symb_chars(Cs) -> all(fun symb_char/1, Cs).

start_symb_char($#) -> false;
start_symb_char($`) -> false;
start_symb_char($') -> false;                   %'
start_symb_char($,) -> false;
start_symb_char($|) -> false;                   %Symbol quote character
start_symb_char(C) -> symb_char(C).

symb_char($() -> false;
symb_char($)) -> false;
symb_char($[) -> false;
symb_char($]) -> false;
symb_char(${) -> false;
symb_char($}) -> false;
symb_char($") -> false;
symb_char($;) -> false;
symb_char(C) -> ((C > $\s) and (C =< $~)) orelse (C > $\240).

%% print1_string([Char], QuoteChar) -> [Char]
%%  Generate the list of characters needed to print a string.

print1_string(S, Q) ->
    [Q|print1_string1(S, Q)].

print1_string1([], Q) ->    [Q];
print1_string1([C|Cs], Q) ->
    string_char(C, Q, print1_string1(Cs, Q)).

string_char(Q, Q, Tail) -> [$\\,Q|Tail];        %Must check these first!
string_char($\\, _, Tail) -> [$\\,$\\|Tail];
string_char(C, _, Tail) when C >= $\s, C =< $~ ->
    [C|Tail];
string_char(C, _, Tail) when C >= $\240, C =< $\377 ->
    [C|Tail];
string_char($\n, _, Tail) -> [$\\,$n|Tail];     %\n = LF
string_char($\r, _, Tail) -> [$\\,$r|Tail];     %\r = CR
string_char($\t, _, Tail) -> [$\\,$t|Tail];     %\t = TAB
string_char($\v, _, Tail) -> [$\\,$v|Tail];     %\v = VT
string_char($\b, _, Tail) -> [$\\,$b|Tail];     %\b = BS
string_char($\f, _, Tail) -> [$\\,$f|Tail];     %\f = FF
string_char($\e, _, Tail) -> [$\\,$e|Tail];     %\e = ESC
string_char($\d, _, Tail) -> [$\\,$d|Tail];     %\d = DEL
string_char(C, _, Tail) ->
    %%Unicode and other control characters.
    "\\x" ++ erlang:integer_to_list(C, 16) ++ ";" ++ Tail.

%% prettyprint([IoDevice], Sexpr) -> ok.
%% prettyprint1(Sexpr, Depth, Indentation, LineLength) -> [char()].
%%  External interface to the prettyprint functions.

prettyprint(S) -> prettyprint(standard_io, S).
prettyprint(Io, S) -> io:put_chars(Io, prettyprint1(S, -1)).

prettyprint1(S) -> lfe_io_pretty:print1(S).
prettyprint1(S, D) -> lfe_io_pretty:print1(S, D, 0, 80).
prettyprint1(S, D, I) -> lfe_io_pretty:print1(S, D, I, 80).
prettyprint1(S, D, I, L) -> lfe_io_pretty:print1(S, D, I, L).

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
