%% Copyright (c) 2008-2014 Robert Virding
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

%% File    : lfe_io_pretty.erl
%% Author  : Robert Virding
%% Purpose : Pretty printer for Lisp Flavoured Erlang.

-module(lfe_io_pretty).

-export([print1/1,print1/2,print1/3,print1/4]).

-compile(export_all).

-import(lists, [reverse/1,reverse/2,flatlength/1]).

%% Define IS_MAP/1 macro for is_map/1 bif.
-ifdef(HAS_MAPS).
-define(IS_MAP(T), is_map(T)).
-else.
-define(IS_MAP(T), false).
-endif.

%% print1(Sexpr) -> [char()].
%% print1(Sexpr, Depth) -> [char()].
%% print1(Sexpr, Depth, Indentation, LineLength) -> [char()].
%%  A relatively simple pretty print function, but with some
%%  customisation. N.B. We know about the standard character macros
%%  and use them instead of their expanded forms.

print1(S) -> print1(S, -1, 0, 80).

print1(S, D) -> print1(S, D, 0, 80).

print1(S, D, I) -> print1(S, D, I, 80).

print1(_, 0, _, _) -> "...";
print1(Symb, _, _, _) when is_atom(Symb) -> lfe_io:print1_symb(Symb);
print1(Numb, _, _, _) when is_integer(Numb) -> integer_to_list(Numb);
print1(Numb, _, _, _) when is_float(Numb) -> io_lib_format:fwrite_g(Numb);
%% Handle some default special cases, standard character macros. These
%% don't increase depth as they really should.
print1([quote,E], D, I, L) -> ["'",print1(E, D, I+1, L)];
print1([backquote,E], D, I, L) -> ["`",print1(E, D, I+1, L)];
print1([unquote,E], D, I, L) -> [",",print1(E, D, I+1, L)];
print1(['unquote-splicing',E], D, I, L) -> [",@",print1(E, D, I+2, L)];
print1([Car|_]=List, D, I, L) ->
    %% Handle printable lists specially.
    case io_lib:printable_list(List) of
        true -> lfe_io:print1_string(List, $"); %"
        false ->
            case print1_list_max(List, D-1, I+1, L-1) of
                {yes,Print} -> ["(",Print,")"];
                no ->
                    %% Customise printing of lists.
                    case indent_type(Car) of
                        none ->                 %Normal lists.
                            ["(",print1_list(List, D-1, I+1, L-1),")"];
                        defun ->                %Special case for defuns
                            print1_defun(List, D, I, L);
                        N when is_integer(N) -> %Special N first elements
                            print1_type(List, D, I, L, N)
                    end
            end
    end;
print1([], _, _, _) -> "()";
print1({}, _, _, _) -> "#()";
print1(Tup, D, I, L) when is_tuple(Tup) ->
    Es = tuple_to_list(Tup),
    case print1_list_max(Es, D-1, I+2, L-1) of
        {yes,Print}  -> ["#(",Print,")"];
        no -> ["#(",print1_list(Es, D-1, I+2, L),")"]
    end;
print1(Bit, D, _, _) when is_bitstring(Bit) ->
    ["#B(",lfe_io:print1_bits(Bit, D),$)];      %First D bytes
print1(Map, D, I, L) when ?IS_MAP(Map) ->
    print1_map(Map, D, I, L);
print1(Other, _, _, _) ->
    lfe_io:print1(Other).                       %Use standard LFE for rest

%% print1_defun(List, Depth, Indentation, LineLength) -> [char()].
%% Print a defun depending on whether it is traditional or matching.

print1_defun([Def,Name,Args|Rest], D, I, L) when
      is_atom(Name), (D > 3) or (D < 0) ->
    Dcs = atom_to_list(Def),                    %Might not actually be defun
    Ncs = atom_to_list(Name),
    case lfe_lib:is_symb_list(Args) of
        true ->                                 %Traditional
            Acs = print1(Args, D-2, I + length(Dcs) + length(Ncs) + 3, L),
            Tcs = print1_tail(Rest, D-3, I+2, L),
            ["(",Dcs," ",Ncs," ",Acs,Tcs,")"];
        false ->                                %Matching
            Tcs = print1_tail([Args|Rest], D-2, I+2, L),
            ["(",Dcs," ",Ncs,Tcs,")"]
    end;
print1_defun(List, D, I, L) ->
    %% Too short to get worked up about, or not a "proper" defun or
    %% not enough depth.
    ["(",print1_list(List, D-1, I+1, L),")"].

%% print1_type(List, Depth, Indentation, LineLength, TypeCount) -> [char()].
%% Print a special type form indenting first TypeCount elements afer
%% type and rest indented 2 steps.

print1_type([Car|Cdr], D, I, L, N) when (D > 2) or (D < 0) ->
    %% Handle special lists, we KNOW Car is an atom.
    Cs = atom_to_list(Car),
    NewI = I + length(Cs) + 2,
    {Spec,Rest} = split(N, Cdr),
    Tcs = [print1_list(Spec, D-1, NewI, L),
           print1_tail(Rest, D-2, I+2, L)],
    ["(" ++ Cs," ",Tcs,")"];
print1_type(List, D, I, L, _) ->
    %% Too short to get worked up about or not enough depth.
    [$(,print1_list(List, D-1, I+1, L),$)].

%% split(N, List) -> {List1,List2}.
%%  Split a list into two lists, the first containing the first N
%%  elements and the second the rest. Be tolerant of too few elements.

split(0, L) -> {[],L};
split(_, []) -> {[],[]};
split(N, [H|T]) ->
    {H1,T1} = split(N-1, T),
    {[H|H1],T1}.

%% print1_list_max(List, Depth, Indentation, LineLength) -> {yes,Chars} | no.
%%  Maybe print a list on one line, but abort if it goes past
%%  LineLength.

print1_list_max([], _, _, _) -> {yes,[]};
print1_list_max(_, 0, _, _) -> {yes,"..."};
print1_list_max([Car|Cdr], D, I, L) ->
    Cs = print1(Car, D, 0, 99999),        %Never break the line
    print1_tail_max(Cdr, D-1, I + flatlength(Cs), L, [Cs]).

%% print1_tail_max(Tail, Depth, Indentation, LineLength) -> {yes,Chars} | no.
%%  Maybe print the tail of a list on one line, but abort if it goes
%%  past LineLength. We know about dotted pairs. When we reach depth 0
%%  we just quit as we know necessary "..." will have come from an
%%  earlier print1 at same depth.

print1_tail_max(_, _, I, L, _) when I >= L ->   %No more room
    no;
print1_tail_max([], _, _, _, Acc) -> {yes,reverse(Acc)};
print1_tail_max(_, 0, _, _, Acc) -> {yes,reverse(Acc, [" ..."])};
print1_tail_max([Car|Cdr], D, I, L, Acc) ->
    Cs = print1(Car, D, 0, 99999),              %Never break the line
    print1_tail_max(Cdr, D-1, I + flatlength(Cs) + 1, L, [Cs," "|Acc]);
print1_tail_max(S, D, I, L, Acc) ->
    Cs = print1(S, D, 0, 99999),                %Never break the line
    print1_tail_max([], D-1, I + flatlength(Cs) + 3, L, [Cs," . "|Acc]).

%% print1_list(List, Depth, Indentation, LineLength)
%%  Print a list, one element per line but print multiple atomic
%%  elements on one line. No leading/trailing ().

print1_list([], _, _, _) -> [];
print1_list(_, 0, _, _) -> "...";
print1_list([Car|Cdr], D, I, L) ->
    case print1_element(Car, I, D, I, L) of
        {join,Ccs,Cl} ->                        %Atomic that fits
            [Ccs|print1_tail(Cdr, I+Cl, D, I, L)];
        {break,Ccs,_} ->                        %Atomic that does not fit
            [Ccs|print1_tail(Cdr, L, D, I, L)];
        {break,Ccs} ->                          %Non-atomic
            %% Force a break after not an atomic.
            [Ccs|print1_tail(Cdr, L, D, I, L)]
    end.

%% print1_tail(Tail, Depth, Indentation, LineLength)
%% print1_tail(Tail, CurrentLength, Depth, Indentation, LineLength)
%%  Print the tail of a list decreasing the depth for each element. We
%%  print multiple atomic elements on one line and we know about
%%  dotted pairs.

print1_tail(Tail, D, I, L) ->
    print1_tail(Tail, L, D, I, L).              %Force a break

print1_tail([], _, _, _, _) -> "";
print1_tail(_, _, 0, _, _) -> " ...";
print1_tail([Car|Cdr], CurL, D, I, L) ->
    case print1_element(Car, CurL+1, D, I, L) of
        {join,Ccs,Cl} ->                        %Atomic that fits
            [$\s,Ccs,print1_tail(Cdr, CurL+1+Cl, D-1, I, L)];
        {break,Ccs,Cl} ->                       %Atomic that does not fit
            [newline(I, Ccs),print1_tail(Cdr, I+Cl, D-1, I, L)];
        {break,Ccs} ->                          %Non-atomic
            %% Force a break after not an atomic.
            [newline(I, Ccs),print1_tail(Cdr, L, D-1, I, L)]
    end;
print1_tail(Cdr, CurL, D, I, L) ->
    case print1_element(Cdr, CurL+3, D, I, L) of
        {join,Ccs,_} -> [" . "|Ccs];            %Atomic that fits
        {break,Ccs,_} ->                        %Atomic that does not fit
            [" .\n",blanks(I, Ccs)];
        {break,Ccs} ->                          %Non-atomic
            [" .\n",blanks(I, Ccs)]
    end.

print1_element(E, CurL, D, _, L) when is_number(E);
                                      is_atom(E);
                                      is_pid(E);
                                      is_reference(E);
                                      is_port(E);
                                      is_function(E);
                                      E =:= [] ->
    Ecs = lfe_io:print1(E, D),
    El = flatlength(Ecs),
    if CurL+El =< L - 10 -> {join,Ecs,El};      %Don't make the line too wide
       true -> {break,Ecs,El}
    end;
print1_element(E, _, D, I, L) ->
    {break,print1(E, D, I, L)}.

blanks(N, Tail) -> string:chars($\s, N, Tail).

newline(N) -> newline(N, []).

newline(N, Tail) ->
    [$\n|blanks(N, Tail)].

%% indent_type(Form) -> N | none.
%%  Defines special indentation. None means default, N is number of
%%  sexprs in list which are indented *after* Form while all following
%%  that end up at indent+2.

%% Old style forms.
indent_type('define') -> 1;
indent_type('define-module') -> 1;
indent_type('extend-module') -> 0;
indent_type('define-syntax') -> 1;
indent_type('define-record') -> 1;
indent_type('begin') -> 0;
indent_type('let-syntax') -> 1;
indent_type('syntax-rules') -> 0;
indent_type('macro') -> 0;
%% New style forms.
indent_type('defmodule') -> 1;
indent_type('defun') -> defun;
indent_type('defmacro') -> defun;
indent_type('defsyntax') -> 1;
indent_type('defrecord') -> 1;
%% Core forms.
indent_type('progn') -> 0;
indent_type('lambda') -> 1;
indent_type('match-lambda') -> 0;
indent_type('let') -> 1;
indent_type('let-function') -> 1;
indent_type('letrec-function') -> 1;
indent_type('let-macro') -> 1;
indent_type('if') -> 1;
indent_type('case') -> 1;
indent_type('receive') -> 0;
indent_type('catch') -> 0;
indent_type('try') -> 1;
indent_type('funcall') -> 1;
indent_type('call') -> 2;
indent_type('define-function') -> 1;
indent_type('define-macro') -> 1;
indent_type('eval-when-compile') -> 0;
%% Core macros.
indent_type(':') -> 2;
indent_type('cond') -> 999;            %All following forms
indent_type('let*') -> 1;
indent_type('flet') -> 1;
indent_type('flet*') -> 1;
indent_type('fletrec') -> 1;
indent_type(macrolet) -> 1;
indent_type(syntaxlet) -> 1;
indent_type('do') -> 2;
indent_type('lc') -> 1;                %List comprehensions
indent_type('bc') -> 1;                %Binary comprehensions
indent_type('match-spec') -> 0;
indent_type(_) -> none.

%% print1_map(Map, Depth, Indentation, LineLength).
%%  Print a map, one key value pair per line.

print1_map(Map, D, I, L) ->
    [$#,$M,$(,print1_map_body(maps:to_list(Map), I+3, D, I+3, L-1),$)].

print1_map_body([], _, _, _, _) -> [];
print1_map_body([{K,V}|KVs], CurL, D, I, L) ->
    case print1_map_assoc(K, V, CurL, D, I, L) of
        {join,KVcs,KVl} ->
            [KVcs,print1_map_rest(KVs, CurL+KVl, D-1, I, L)];
        {break,KVcs,KVl} ->
            [KVcs,print1_map_rest(KVs, I+KVl, D-1, I, L)];
        {break,KVcs} ->
            %% Force a break after K/V split.
            [KVcs,print1_map_rest(KVs, L, D-1, I, L)]
    end.

print1_map_rest([], _, _, _, _) -> "";
print1_map_rest(_, _, 0, _, _) -> " ...";
print1_map_rest([{K,V}|KVs], CurL, D, I, L) ->
    case print1_map_assoc(K, V, CurL+1, D, I, L) of
        {join,KVcs,KVl} ->
            [$\s,KVcs,print1_map_rest(KVs, CurL+KVl+1, D-1, I, L)];
        {break,KVcs,KVl} ->
            [newline(I, KVcs),print1_map_rest(KVs, I+KVl, D-1, I, L)];
        {break,KVcs} ->
            %% Force a break after K/V split.
            [newline(I, KVcs),print1_map_rest(KVs, L, D-1, I, L)]
    end.

print1_map_assoc(K, V, CurL, D, I, L) ->
    Kcs = print1(K, D, 0, 99999),               %Never break the line
    Kl = flatlength(Kcs),
    Vcs = print1(V, D, 0, 99999),               %Never break the line
    Vl = flatlength(Vcs),
    if CurL+Kl+Vl < L-10 ->                     %Both fit together here
            {join,[Kcs,$\s,Vcs],Kl+1+Vl};
       I+Kl+Vl < L-10 ->                        %Both fit on single line
            {break,[Kcs,$\s,Vcs],Kl+1+Vl};
       true ->                                  %On separate lines
            %% Try to reuse flat prints if they fit on one line.
            Ks = if I+Kl < L-10 -> Kcs;
                    true -> print1(K, D, I, L)
                 end,
            Vs = if I+Vl < L-10 -> Vcs;
                    true -> print1(V, D, I, L)
                 end,
            {break,[Ks,newline(I, Vs)]}
    end.

last_length(S) -> last_length(S, 0).

last_length([H|T], L0) when is_list(H) ->
    L1 = last_length(H, L0),                    %Must go left-to-right
    last_length(T, L1);
last_length([$\n|T], _) ->
    last_length(T, 0);
last_length([_|T], L) ->
    last_length(T, L+1);
last_length([], L) -> L.
