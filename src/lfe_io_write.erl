%% Copyright (c) 2008-2015 Robert Virding
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

%% File    : lfe_io_write.erl
%% Author  : Robert Virding
%% Purpose : Basic write functions for Lisp Flavoured Erlang.

-module(lfe_io_write).

-export([term/1,term/2,symbol/1,string/2,bitstring/2]).

%% -compile(export_all).

%% Define IS_MAP/1 macro for is_map/1 bif.
-ifdef(HAS_MAPS).
-define(IS_MAP(T), is_map(T)).
-else.
-define(IS_MAP(T), false).
-endif.

%% print([IoDevice], Sexpr) -> ok.
%% print1(Sexpr) -> [char()].
%% print1(Sexpr, Depth) -> [char()].
%%  A simple print function. Does not pretty-print but stops at Depth.

term(S) -> term(S, -1).                         %All the way

term(_, 0) -> "...";
term(Symb, _) when is_atom(Symb) -> symbol(Symb);
term(Numb,_ ) when is_integer(Numb) -> integer_to_list(Numb);
term(Numb, _) when is_float(Numb) -> io_lib_format:fwrite_g(Numb);
term(List, D) when is_list(List) ->
    [$(,list(List, D-1),$)];
term({}, _) -> "#()";
term(Vec, D) when is_tuple(Vec) ->
    Es = tuple_to_list(Vec),
    ["#(",list(Es, D-1),")"];
term(Bit, _) when is_bitstring(Bit) ->
    bitstring(Bit);
term(Map, D) when ?IS_MAP(Map) -> map(Map, D);
term(Other, D) ->                               %Use standard Erlang for rest
    io_lib:write(Other, D).

%% symbol(Symbol) -> [char()].

symbol(Symb) ->
    Cs = atom_to_list(Symb),
    case quote_symbol(Symb, Cs) of
        true -> string(Cs , $|);
        false -> Cs
    end.

%% bitstring(Bitstring) -> [char()]
%% bitstring(Bitstring, Depth) -> [char()]
%%  Print the bytes in a bitstring. Print bytes except for last which
%%  we add size field if not 8 bits big.

bitstring(Bit) -> bitstring(Bit, -1).

bitstring(Bit, D) ->
    ["#B(",bytes(Bit, D),$)].

bytes(_, 0) -> "...";
bytes(<<B:8>>, _) -> integer_to_list(B);       %Catch last binary byte
bytes(<<B:8,Bs/bitstring>>, N) ->
    [integer_to_list(B),$\s|bytes(Bs, N-1)];
bytes(<<>>, _) -> [];
bytes(Bits, _) ->                               %0 < Size < 8
    N = bit_size(Bits),
    <<B:N>> = Bits,
    io_lib:format("(~w (size ~w))", [B,N]).

%% list(List, Depth) -> Chars.
%%  Print the elements in a list. We handle the empty list and depth=0.

list([], _) -> [];
list(_, 0) -> "...";
list([Car|Cdr], D) ->
    [term(Car, D)|list_tail(Cdr, D-1)].

%% list_tail(Tail, Depth)
%%  Print the tail of a list decrasing the depth for each element. We
%%  know about dotted pairs.

list_tail([], _) -> "";
list_tail(_, 0) -> [$\s|"..."];
list_tail([S|Ss], D) ->
    [$\s,term(S, D)|list_tail(Ss, D-1)];
list_tail(S, D) -> [" . "|term(S, D)].

%% map(Map, Depth)

map(Map, D) ->
    [$#,$M,$(,map_body(maps:to_list(Map), D), $)].

map_body([], _) -> [];
map_body(_, D) when D =:= 0; D =:= 1 -> "...";
map_body([KV], D) -> map_assoc(KV, D);
map_body([KV|KVs], D) ->
    Massoc = map_assoc(KV, D),
    [Massoc,$\s|map_body(KVs, D-1)].

map_assoc({K,V}, D) ->
    [term(K, D-1),$\s,term(V, D-1)].

%% quote_symbol(Symbol, SymbChars) -> bool().
%%  Check if symbol needs to be quoted when printed. If it can read as
%%  a number then it must be quoted.

quote_symbol('.', _) -> true;                   %Needs quoting
quote_symbol(_, [C|Cs]=Cs0) ->
    case catch {ok,list_to_float(Cs0)} of
        {ok,_} -> true;
        _ -> case catch {ok,list_to_integer(Cs0)} of
                 {ok,_} -> true;
                 _ -> not (lfe_scan:start_symbol_char(C) andalso
                           symbol_chars(Cs))
             end
    end;
quote_symbol(_, []) -> true.

symbol_chars(Cs) -> lists:all(fun lfe_scan:symbol_char/1, Cs).

%% string([Char], QuoteChar) -> [Char]
%%  Generate the list of characters needed to print a string.

string(S, Q) ->
    [Q,string_chars(S, Q)].

string_chars([], Q) -> [Q];
string_chars([C|Cs], Q) ->
    string_char(C, Q, string_chars(Cs, Q)).

string_char(Q, Q, Tail) -> [$\\,Q|Tail];        %Must check these first!
string_char($\\, _, Tail) -> [$\\,$\\|Tail];
string_char($\b, _, Tail) -> [$\\,$b|Tail];     %\b = BS
string_char($\t, _, Tail) -> [$\\,$t|Tail];     %\t = TAB
string_char($\n, _, Tail) -> [$\\,$n|Tail];     %\n = LF
string_char($\v, _, Tail) -> [$\\,$v|Tail];     %\v = VT
string_char($\f, _, Tail) -> [$\\,$f|Tail];     %\f = FF
string_char($\r, _, Tail) -> [$\\,$r|Tail];     %\r = CR
string_char($\e, _, Tail) -> [$\\,$e|Tail];     %\e = ESC
string_char($\d, _, Tail) -> [$\\,$d|Tail];     %\d = DEL
string_char(C, _, Tail) -> [C|Tail].
