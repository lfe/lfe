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

%% File    : lfe_io_format.erl
%% Author  : Robert Virding
%% Purpose : Formatted io for Lisp Flavoured Erlang.

%% The formatting algorithms and code structure is the same as in
%% io_lib_format. I have no problems with this as I originally wrote
%% most of it.

-module(lfe_io_format).

-export([fwrite1/2]).

-import(lists, [reverse/1,foldl/3]).

%% -compile([export_all]).

fwrite1(Format, Data) ->
    Cs = scan(Format, Data),
    Pc = pcount(Cs),
    build(Cs, Pc, 0).

%% scan(Format, Args) -> FormatList.

scan(Format, Args) when is_binary(Format) ->
    scan(binary_to_list(Format), Args);
scan(Format, Args) ->
    collect(Format, Args).

%% collect(Format, Args) -> FormatList.

collect([$~|Fmt0], As0) ->
    {C,Fmt1,As1} = collect_cseq(Fmt0, As0),
    [C|collect(Fmt1, As1)];
collect([C|Fmt], Args) ->
    [C|collect(Fmt, Args)];
collect([], []) -> [].

%% collect_cseq(Format, Args) ->
%%      {{Control,ControlArgs,Field,Adjust,Precision,Pad},Format,Args}.

collect_cseq(Fmt0, As0) ->
    {F,Ad,Fmt1,As1} = field_width(Fmt0, As0),
    {P,Fmt2,As2} = precision(Fmt1, As1),
    {Pad,Fmt3,As3} = pad_char(Fmt2, As2),
    {C,As,Fmt4,As4} = collect_cc(Fmt3, As3),
    {{C,As,F,Ad,P,Pad},Fmt4,As4}.

%% field_width(Format, Args) -> {Field,Adjust,Format,Args}.
%% precision(Format, Args) -> {Precision,Format,Args}.
%% pad_char(Format, Args) -> {Field,Adjust,Format,Args}.
%%  Extract the field width/precision/pad char from the format

field_width([$-|Fmt0], Args0) ->
    {F,Fmt,Args} = field_value(Fmt0, Args0),
    field_width(-F, Fmt, Args);
field_width(Fmt0, Args0) ->
    {F,Fmt,Args} = field_value(Fmt0, Args0),
    field_width(F, Fmt, Args).

field_width(F, Fmt, Args) when F < 0 ->
    {-F,left,Fmt,Args};
field_width(F, Fmt, Args) when F >= 0 ->
    {F,right,Fmt,Args}.

precision([$.|Fmt], Args) -> field_value(Fmt, Args);
precision(Fmt, Args) -> {none,Fmt,Args}.

field_value([$*|Fmt], [A|Args]) when is_integer(A) ->
    {A,Fmt,Args};
field_value([C|Fmt], Args) when is_integer(C), C >= $0, C =< $9 ->
    field_value([C|Fmt], Args, 0);
field_value(Fmt, Args) ->
    {none,Fmt,Args}.

field_value([C|Fmt], Args, F) when is_integer(C), C >= $0, C =< $9 ->
    field_value(Fmt, Args, 10*F + (C - $0));
field_value(Fmt, Args, F) ->                    %Default case
    {F,Fmt,Args}.

pad_char([$.,$*|Fmt], [Pad|Args]) -> {Pad,Fmt,Args};
pad_char([$.,Pad|Fmt], Args) -> {Pad,Fmt,Args};
pad_char(Fmt, Args) -> {$\s,Fmt,Args}.

%% pcount([ControlC]) -> Count.
%%  Count the number of print requests.

pcount(Cs) ->
    foldl(fun ({$p,_,_,_,_,_}, Acc) -> Acc+1;
              ({$P,_,_,_,_,_}, Acc) -> Acc+1;
              (_, Acc) -> Acc
          end, 0, Cs).

%% build(FormatList, Pc, Indentation) -> [Char].
%%  Interpret the control structures. Count the number of print
%%  remaining and only calculate indentation when necessary. Must also
%%  be smart when calculating indentation for characters in format.

build([{C,As,F,Ad,P,Pad}|Cs], Pc0, I) ->
    S = control(C, As, F, Ad, P, Pad, I),
    Pc1 = decr_pc(C, Pc0),
    if
        Pc1 > 0 -> [S|build(Cs, Pc1, indentation(S, I))];
        true -> [S|build(Cs, Pc1, I)]
    end;
build([$\n|Cs], Pc, _I) -> [$\n|build(Cs, Pc, 0)];
build([$\t|Cs], Pc, I) -> [$\t|build(Cs, Pc, ((I + 8) div 8) * 8)];
build([C|Cs], Pc, I) -> [C|build(Cs, Pc, I+1)];
build([], _Pc, _I) -> [].

decr_pc($p, Pc) -> Pc - 1;
decr_pc($P, Pc) -> Pc - 1;
decr_pc(_, Pc) -> Pc.

%% indentation([Char], Indentation) -> Indentation.
%%  Calculate the indentation of the end of a string given its start
%%  indentation. We assume tabs at 8 cols.

indentation([$\n|Cs], _I) -> indentation(Cs, 0);
indentation([$\t|Cs], I) -> indentation(Cs, ((I + 8) div 8) * 8);
indentation([C|Cs], I) when is_integer(C) ->
    indentation(Cs, I+1);
indentation([C|Cs], I) ->
    indentation(Cs, indentation(C, I));
indentation([], I) -> I.

%% collect_cc([FormatChar], [Argument]) ->
%%    {Control,[ControlArg],[FormatChar],[Arg]}.
%%  Here we collect the argments for each control character.
%%  Be explicit to cause failure early.

collect_cc([$w|Fmt], [A|Args]) -> {$w,[A],Fmt,Args};
collect_cc([$p|Fmt], [A|Args]) -> {$p,[A],Fmt,Args};
collect_cc([$W|Fmt], [A,Depth|Args]) -> {$W,[A,Depth],Fmt,Args};
collect_cc([$P|Fmt], [A,Depth|Args]) -> {$P,[A,Depth],Fmt,Args};
collect_cc([$s|Fmt], [A|Args]) -> {$s,[A],Fmt,Args};
collect_cc([$e|Fmt], [A|Args]) -> {$e,[A],Fmt,Args};
collect_cc([$f|Fmt], [A|Args]) -> {$f,[A],Fmt,Args};
collect_cc([$g|Fmt], [A|Args]) -> {$g,[A],Fmt,Args};
collect_cc([$b|Fmt], [A|Args]) -> {$b,[A],Fmt,Args};
collect_cc([$B|Fmt], [A|Args]) -> {$B,[A],Fmt,Args};
collect_cc([$x|Fmt], [A,Prefix|Args]) -> {$x,[A,Prefix],Fmt,Args};
collect_cc([$X|Fmt], [A,Prefix|Args]) -> {$X,[A,Prefix],Fmt,Args};
collect_cc([$+|Fmt], [A|Args]) -> {$+,[A],Fmt,Args};
collect_cc([$#|Fmt], [A|Args]) -> {$#,[A],Fmt,Args};
collect_cc([$c|Fmt], [A|Args]) -> {$c,[A],Fmt,Args};
collect_cc([$~|Fmt], Args) when is_list(Args) -> {$~,[],Fmt,Args};
collect_cc([$n|Fmt], Args) when is_list(Args) -> {$n,[],Fmt,Args};
collect_cc([$i|Fmt], [A|Args]) -> {$i,[A],Fmt,Args}.

%% control(FormatChar, [Argument], FieldWidth, Adjust, Precision, PadChar,
%%       Indentation) ->
%%    [Char]
%%  This is the main dispatch function for the various formatting commands.
%%  Field widths and precisions have already been calculated.

control($w, [A], F, Adj, P, Pad, _) ->
    write(lfe_io:print1(A, -1), F, Adj, P, Pad);
control($W, [A,Depth], F, Adj, P, Pad, _I) when is_integer(Depth) ->
    write(lfe_io:print1(A, Depth), F, Adj, P, Pad);
control($p, [A], F, Adj, P, Pad, I) ->
    print(A, -1, F, Adj, P, Pad, I);
control($P, [A,Depth], F, Adj, P, Pad, I) when is_integer(Depth) ->
    print(A, Depth, F, Adj, P, Pad, I);
control($s, [A], F, Adj, P, Pad, _I) when is_atom(A) ->
    string(atom_to_list(A), F, Adj, P, Pad);
%% control($s, [L0], F, Adj, P, Pad, _) ->
%%     L = iolist_to_chars(L0),
%%     string(L, F, Adj, P, Pad);
control($s, [L0], F, Adj, P, Pad, _) ->
    L = unicode:characters_to_list(L0),
    string(L, F, Adj, P, Pad);
control($e, [A], F, Adj, P, Pad, _) when is_float(A) ->
    fwrite_e(A, F, Adj, P, Pad);
control($f, [A], F, Adj, P, Pad, _) when is_float(A) ->
    fwrite_f(A, F, Adj, P, Pad);
control($g, [A], F, Adj, P, Pad, _) when is_float(A) ->
    fwrite_g(A, F, Adj, P, Pad);
control($b, [A], F, Adj, P, Pad, _) when is_integer(A) ->
    unprefixed_integer(A, F, Adj, base(P), Pad, true);
control($B, [A], F, Adj, P, Pad, _) when is_integer(A) ->
    unprefixed_integer(A, F, Adj, base(P), Pad, false);
control($x, [A,Prefix], F, Adj, P, Pad, _) when is_integer(A),
                                                is_atom(Prefix) ->
    prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), true);
control($x, [A,Prefix], F, Adj, P, Pad, _) when is_integer(A) ->
    true = io_lib:deep_char_list(Prefix), %Check if Prefix a character list
    prefixed_integer(A, F, Adj, base(P), Pad, Prefix, true);
control($X, [A,Prefix], F, Adj, P, Pad, _) when is_integer(A),
                                                is_atom(Prefix) ->
    prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), false);
control($X, [A,Prefix], F, Adj, P, Pad, _) when is_integer(A) ->
    true = io_lib:deep_char_list(Prefix), %Check if Prefix a character list
    prefixed_integer(A, F, Adj, base(P), Pad, Prefix, false);
control($+, [A], F, Adj, P, Pad, _) when is_integer(A) ->
    Base = base(P),
    Prefix = base_prefix(Base, true),
    prefixed_integer(A, F, Adj, Base, Pad, Prefix, true);
control($#, [A], F, Adj, P, Pad, _) when is_integer(A) ->
    Base = base(P),
    Prefix = base_prefix(Base, false),
    prefixed_integer(A, F, Adj, Base, Pad, Prefix, false);
control($c, [A], F, Adj, P, Pad, _) when is_integer(A) ->
    char(A, F, Adj, P, Pad);
%% control($c, [A], F, Adj, P, Pad, _Enc, _I) when is_integer(A) ->
%%     char(A band 255, F, Adj, P, Pad);
control($~, [], F, Adj, P, Pad, _) -> char($~, F, Adj, P, Pad);
control($n, [], F, Adj, P, Pad, _) -> newline(F, Adj, P, Pad);
control($i, [_], _, _, _, _, _) -> [].

%% Default integer base
base(none) -> 10;
base(B) when is_integer(B) -> B.

%% write(CharList, Field, Adjust, Precision, PadChar)
%%  Write the characters of a term. Use Precision to trim length of
%%  output.  Adjust the characters within the field if length less
%%  than Max padding with PadChar.

write(T, none, _, none, _) -> T;
write(T, F, Adj, P, Pad) ->
    N = lists:flatlength(T),
    if P =:= none -> write1(T, F, Adj, N, Pad);
       P >= N -> write1(T, F, Adj, N, Pad);
       true -> write1(flat_trunc(T, P), F, Adj, P, Pad)
    end.

write1(T, none, _, _, _) -> T;
write1(T, F, Adj, N, Pad) ->
    if F < N -> chars($*, F);
       F == N -> T;
       true -> adjust(T, chars(Pad, F-N), Adj)
    end.

%% print(CharList, Depth, Field, Adjust, Precision, PadChar, Indentation)
%%  Pretty print the characters of a term, field width is maximum line
%%  length and precision is initial indentation.

print(T, D, none, Adj, P, Pad, I) -> print(T, D, 80, Adj, P, Pad, I);
print(T, D, F, Adj, none, Pad, I) -> print(T, D, F, Adj, I, Pad, I);
print(T, D, F, right, P, _, _) ->
    lfe_io_pretty:term(T, D, P, F).

%% fwrite_e(Float, Field, Adjust, Precision, PadChar)

fwrite_e(Fl, none, Adj, none, Pad) ->           %Default values
    fwrite_e(Fl, none, Adj, 6, Pad);
fwrite_e(Fl, none, _Adj, P, _Pad) when P >= 2 ->
    float_e(Fl, float_data(Fl), P);
fwrite_e(Fl, F, Adj, none, Pad) ->
    fwrite_e(Fl, F, Adj, 6, Pad);
fwrite_e(Fl, F, Adj, P, Pad) when P >= 2 ->
    write(float_e(Fl, float_data(Fl), P), F, Adj, F, Pad).

float_e(Fl, Fd, P) when Fl < 0.0 ->             %Negative numbers
    [$-|float_e(-Fl, Fd, P)];
float_e(_Fl, {Ds,E}, P) ->
    case float_man(Ds, 1, P-1) of
        {[$0|Fs],true} -> [[$1|Fs]|float_exp(E)];
        {Fs,false} -> [Fs|float_exp(E-1)]
    end.

%% float_man([Digit], Icount, Dcount) -> {[Chars],CarryFlag}.
%%  Generate the characters in the mantissa from the digits with Icount
%%  characters before the '.' and Dcount decimals. Handle carry and let
%%  caller decide what to do at top.

float_man(Ds, 0, Dc) ->
    {Cs,C} = float_man(Ds, Dc),
    {[$.|Cs],C};
float_man([D|Ds], I, Dc) ->
    case float_man(Ds, I-1, Dc) of
        {Cs,true} when D =:= $9 -> {[$0|Cs],true};
        {Cs,true} -> {[D+1|Cs],false};
        {Cs,false} -> {[D|Cs],false}
    end;
float_man([], I, Dc) ->                         %Pad with 0's
    {string:chars($0, I, [$.|string:chars($0, Dc)]),false}.

float_man([D|_], 0) when D >= $5 -> {[],true};
float_man([_|_], 0) -> {[],false};
float_man([D|Ds], Dc) ->
    case float_man(Ds, Dc-1) of
        {Cs,true} when D =:= $9 -> {[$0|Cs],true};
        {Cs,true} -> {[D+1|Cs],false};
        {Cs,false} -> {[D|Cs],false}
    end;
float_man([], Dc) -> {string:chars($0, Dc),false}. %Pad with 0's

%% float_exp(Exponent) -> [Char].
%%  Generate the exponent of a floating point number. Always include sign.

float_exp(E) when E >= 0 ->
    [$e,$+|integer_to_list(E)];
float_exp(E) ->
    [$e|integer_to_list(E)].

%% fwrite_f(FloatData, Field, Adjust, Precision, PadChar)

fwrite_f(Fl, none, Adj, none, Pad) ->           %Default values
    fwrite_f(Fl, none, Adj, 6, Pad);
fwrite_f(Fl, none, _Adj, P, _Pad) when P >= 1 ->
    float_f(Fl, float_data(Fl), P);
fwrite_f(Fl, F, Adj, none, Pad) ->
    fwrite_f(Fl, F, Adj, 6, Pad);
fwrite_f(Fl, F, Adj, P, Pad) when P >= 1 ->
    write(float_f(Fl, float_data(Fl), P), F, Adj, F, Pad).

float_f(Fl, Fd, P) when Fl < 0.0 ->
    [$-|float_f(-Fl, Fd, P)];
float_f(Fl, {Ds,E}, P) when E =< 0 ->
    float_f(Fl, {string:chars($0, -E+1, Ds),1}, P); %Prepend enough 0's
float_f(_Fl, {Ds,E}, P) ->
    case float_man(Ds, E, P) of
        {Fs,true} -> "1" ++ Fs;                 %Handle carry
        {Fs,false} -> Fs
    end.

%% float_data([FloatChar]) -> {[Digit],Exponent}

float_data(Fl) -> float_data(float_to_list(Fl), []).

float_data([$e|E], Ds) ->
    {reverse(Ds),list_to_integer(E)+1};
float_data([D|Cs], Ds) when D >= $0, D =< $9 ->
    float_data(Cs, [D|Ds]);
float_data([_|Cs], Ds) ->
    float_data(Cs, Ds).

%% fwrite_g(Float, Field, Adjust, Precision, PadChar)
%%  Use the f form if Float is >= 0.1 and < 1.0e4,
%%  and the prints correctly in the f form, else the e form.
%%  Precision always means the # of significant digits.

fwrite_g(Fl, F, Adj, none, Pad) ->
    fwrite_g(Fl, F, Adj, 6, Pad);
fwrite_g(Fl, F, Adj, P, Pad) when P >= 1 ->
    A = abs(Fl),
    E = if A < 1.0e-1 -> -2;
           A < 1.0e0  -> -1;
           A < 1.0e1  -> 0;
           A < 1.0e2  -> 1;
           A < 1.0e3  -> 2;
           A < 1.0e4  -> 3;
           true       -> fwrite_f
        end,
    if  P =< 1, E =:= -1;
        P-1 > E, E >= -1 ->
            fwrite_f(Fl, F, Adj, P-1-E, Pad);
        P =< 1 ->
            fwrite_e(Fl, F, Adj, 2, Pad);
        true ->
            fwrite_e(Fl, F, Adj, P, Pad)
    end.

%% string(StringList, Field, Adjust, Precision, PadChar)
%%  Output a string adjusted with PadChar.

string(S, none, _, none, _) -> S;
string(S, F, Adj, P, Pad) ->
    N = lists:flatlength(S),
    if P =:= none -> string1(S, F, Adj, N, Pad);
       P >= N -> string1(S, F, Adj, N, Pad);
       true -> string1(flat_trunc(S, P), F, Adj, P, Pad)
    end.

string1(S, none, _, _, _) -> S;
string1(S, F, Adj, N, Pad) ->
    if F < N -> flat_trunc(S, F);
       F == N -> S;
       true -> adjust(S, chars(Pad, F-N), Adj)
    end.

%% unprefixed_integer(Int, Field, Adjust, Base, PadChar, Lowercase) ->
%%      [Char].

unprefixed_integer(Int, F, Adj, Base, Pad, Lowercase)
  when Base >= 2, Base =< 1+$Z-$A+10 ->
    if Int < 0 ->
            S = cond_lowercase(erlang:integer_to_list(-Int, Base), Lowercase),
            write([$-|S], F, Adj, none, Pad);
       true ->
            S = cond_lowercase(erlang:integer_to_list(Int, Base), Lowercase),
            write(S, F, Adj, none, Pad)
    end.

%% prefixed_integer(Int, Field, Adjust, Base, PadChar, Prefix, Lowercase) ->
%%      [Char].

prefixed_integer(Int, F, Adj, Base, Pad, Prefix, Lowercase)
  when Base >= 2, Base =< 1+$Z-$A+10 ->
    if Int < 0 ->
            S = cond_lowercase(erlang:integer_to_list(-Int, Base), Lowercase),
            write([$-,Prefix|S], F, Adj, none, Pad);
       true ->
            S = cond_lowercase(erlang:integer_to_list(Int, Base), Lowercase),
            write([Prefix|S], F, Adj, none, Pad)
    end.

%% base_prefix(Base, Lowercase) -> [Char].
%% Special case prefixes for bases.

base_prefix(2, true) -> "#b";
base_prefix(2, false) -> "#B";
base_prefix(8, true) -> "#o";
base_prefix(8, false) -> "#O";
base_prefix(10, true) -> "#d";
base_prefix(10, false) -> "#D";
base_prefix(16, true) -> "#x";
base_prefix(16, false) -> "#X";
base_prefix(Base, true) -> [$#,integer_to_list(Base),$r];
base_prefix(Base, false) -> [$#,integer_to_list(Base),$R].

%% char(Char, Field, Adjust, Precision, PadChar) -> [Char].

char(C, none, _Adj, none, _Pad) -> [C];
char(C, F, _Adj, none, _Pad) -> chars(C, F);
char(C, none, _Adj, P, _Pad) -> chars(C, P);
char(C, F, Adj, P, Pad) when F >= P ->
    adjust(chars(C, P), chars(Pad, F - P), Adj).

%% newline(Field, Adjust, Precision, PadChar) -> [Char].

newline(none, _, _, _) -> "\n";
newline(F, _, _, _) -> chars($\n, F).

%%
%% Utilities
%%

adjust(Data, [], _) -> Data;
adjust(Data, Pad, left) -> [Data,Pad];
adjust(Data, Pad, right) -> [Pad,Data].

%% Flatten and truncate a deep list to at most N elements.

flat_trunc(List, N) when is_integer(N), N >= 0 ->
    flat_trunc(List, [], N).

flat_trunc(_, _, 0) -> [];
flat_trunc([H|T], S, N) when is_list(H) ->
    flat_trunc(H, [T|S], N);
flat_trunc([C|T], S, N) -> [C|flat_trunc(T, S, N-1)];
flat_trunc([], [H|T], N) -> flat_trunc(H, T, N);
flat_trunc([], [], _) -> [].            %All done

%% A deep version of string:chars/2,3

chars(_C, 0) -> [];
chars(C, 1) ->  [C];
chars(C, 2) ->  [C,C];
chars(C, 3) ->  [C,C,C];
chars(C, N) when is_integer(N), (N band 1) =:= 0 ->
    S = chars(C, N bsr 1),
    [S|S];
chars(C, N) when is_integer(N) ->
    S = chars(C, N bsr 1),
    [C,S|S].

%%chars(C, N, Tail) -> [chars(C, N)|Tail].

%% Lowercase conversion

cond_lowercase(String, true) -> lowercase(String);
cond_lowercase(String,false) -> String.

lowercase([H|T]) when is_integer(H), H >= $A, H =< $Z ->
    [(H-$A+$a)|lowercase(T)];
lowercase([H|T]) -> [H|lowercase(T)];
lowercase([]) -> [].
