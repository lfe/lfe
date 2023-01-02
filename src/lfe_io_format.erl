%% Copyright (c) 2008-2020 Robert Virding
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

-record(cstruct, {cchar,                        %Control character
                  args,                         %Control arguments
                  width,                        %Field width
                  adjust,                       %Adjust left/right
                  prec,                         %Precision
                  pad                           %Pad character
                }).

%% -compile([export_all]).

fwrite1(Format, Data) ->
    Cstructs = scan(Format, Data),
    Pc = pcount(Cstructs),
    build(Cstructs, Pc, 0).

%% scan(Format, Data) -> OutputChars.

scan(Format, Data) when is_binary(Format) ->
    scan(binary_to_list(Format), Data);
scan(Format, Data) ->
    collect(Format, Data).

%% collect(Format, Data) -> [ControlStruct].
%%  Collect all the control structures and characters built from the
%%  format and the data.

collect([$~|Fmt0], Data0) ->
    {Cstruct,Fmt1,Data1} = collect_cseq(Fmt0, Data0),
    [Cstruct|collect(Fmt1, Data1)];
collect([C|Fmt], Data) ->
    %% Just return the character in format.
    [C|collect(Fmt, Data)];
collect([], []) -> [].

%% collect_cseq(Format, Data) ->
%%      {ControlStruct,Format,Data}.

collect_cseq(Fmt0, Data0) ->
    {Width,Ad,Fmt1,Data1} = field_width(Fmt0, Data0),
    {P,Fmt2,Data2} = precision(Fmt1, Data1),
    {Pad,Fmt3,Data3} = pad_char(Fmt2, Data2),
    {C,Args,Fmt4,Data4} = collect_cc(Fmt3, Data3),
    {#cstruct{cchar=C,args=Args,width=Width,adjust=Ad,prec=P,pad=Pad},
     Fmt4,Data4}.

%% field_width(Format, Data) -> {Field,Adjust,Format,Data}.
%% precision(Format, Data) -> {Precision,Format,Data}.
%% pad_char(Format, Data) -> {Field,Adjust,Format,Data}.
%%  Extract the field width/precision/pad char from the format

field_width([$-|Fmt0], Data0) ->
    {F,Fmt,Data1} = field_value(Fmt0, Data0),
    field_width(-F, Fmt, Data1);
field_width(Fmt0, Data0) ->
    {F,Fmt,Data1} = field_value(Fmt0, Data0),
    field_width(F, Fmt, Data1).

field_width(F, Fmt, Data) when F < 0 ->
    {-F,left,Fmt,Data};
field_width(F, Fmt, Data) when F >= 0 ->
    {F,right,Fmt,Data}.

precision([$.|Fmt], Data) -> field_value(Fmt, Data);
precision(Fmt, Data) -> {none,Fmt,Data}.

field_value([$*|Fmt], [D|Data]) when is_integer(D) ->
    {D,Fmt,Data};
field_value([C|Fmt], Data) when is_integer(C), C >= $0, C =< $9 ->
    field_value([C|Fmt], Data, 0);
field_value(Fmt, Data) ->
    {none,Fmt,Data}.

field_value([C|Fmt], Data, F) when is_integer(C), C >= $0, C =< $9 ->
    field_value(Fmt, Data, 10*F + (C - $0));
field_value(Fmt, Data, F) ->                    %Default case
    {F,Fmt,Data}.

pad_char([$.,$*|Fmt], [Pad|Data]) -> {Pad,Fmt,Data};
pad_char([$.,Pad|Fmt], Data) -> {Pad,Fmt,Data};
pad_char(Fmt, Data) -> {$\s,Fmt,Data}.

%% pcount([ControlStructs]) -> Count.
%%  Count the number of print requests.

pcount(Cs) ->
    foldl(fun (#cstruct{cchar=$p}, Acc) -> Acc+1;
              (#cstruct{cchar=$P}, Acc) -> Acc+1;
              (_, Acc) -> Acc
          end, 0, Cs).

%% build([ControlStruct], PrintRequestCount, Indentation) -> [Char].
%%  Interpret the control structures. Count the number of print
%%  remaining and only calculate indentation when necessary. Must also
%%  be smart when calculating indentation for characters in format.

build([#cstruct{}=Cstruct|Cs], Pc0, I) ->
    #cstruct{cchar=C,args=As,width=Width,adjust=Ad,prec=P,pad=Pad} = Cstruct,
    S = control(C, As, Width, Ad, P, Pad, I),
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

%% collect_cc(Format, Data) ->
%%    {Control,[ControlArg],Format,Data}.
%%  Here we collect the argments for each control character.
%%  Be explicit to cause failure early.

collect_cc([$w|Fmt], [D|Data]) -> {$w,[D],Fmt,Data};
collect_cc([$p|Fmt], [D|Data]) -> {$p,[D],Fmt,Data};
collect_cc([$W|Fmt], [D,Depth|Data]) -> {$W,[D,Depth],Fmt,Data};
collect_cc([$P|Fmt], [D,Depth|Data]) -> {$P,[D,Depth],Fmt,Data};
collect_cc([$s|Fmt], [D|Data]) -> {$s,[D],Fmt,Data};
collect_cc([$e|Fmt], [D|Data]) -> {$e,[D],Fmt,Data};
collect_cc([$f|Fmt], [D|Data]) -> {$f,[D],Fmt,Data};
collect_cc([$g|Fmt], [D|Data]) -> {$g,[D],Fmt,Data};
collect_cc([$b|Fmt], [D|Data]) -> {$b,[D],Fmt,Data};
collect_cc([$B|Fmt], [D|Data]) -> {$B,[D],Fmt,Data};
collect_cc([$x|Fmt], [D,Prefix|Data]) -> {$x,[D,Prefix],Fmt,Data};
collect_cc([$X|Fmt], [D,Prefix|Data]) -> {$X,[D,Prefix],Fmt,Data};
collect_cc([$+|Fmt], [D|Data]) -> {$+,[D],Fmt,Data};
collect_cc([$#|Fmt], [D|Data]) -> {$#,[D],Fmt,Data};
collect_cc([$c|Fmt], [D|Data]) -> {$c,[D],Fmt,Data};
collect_cc([$~|Fmt], Data) when is_list(Data) -> {$~,[],Fmt,Data};
collect_cc([$n|Fmt], Data) when is_list(Data) -> {$n,[],Fmt,Data};
collect_cc([$i|Fmt], [D|Data]) -> {$i,[D],Fmt,Data}.

%% control(FormatChar, [Argument], FieldWidth, Adjust, Precision, PadChar,
%%       Indentation) ->
%%    [Char]
%%  This is the main dispatch function for the various formatting commands.
%%  Field widths and precisions have already been calculated.

control($w, [A], F, Adj, P, Pad, _I) ->
    write(lfe_io:print1(A, -1), F, Adj, P, Pad);
control($W, [A,Depth], F, Adj, P, Pad, _I) when is_integer(Depth) ->
    write(lfe_io:print1(A, Depth), F, Adj, P, Pad);
control($p, [A], F, Adj, P, Pad, I) ->
    print(A, -1, F, Adj, P, Pad, I);
control($P, [A,Depth], F, Adj, P, Pad, I) when is_integer(Depth) ->
    print(A, Depth, F, Adj, P, Pad, I);
control($s, [A], F, Adj, P, Pad, _I) when is_atom(A) ->
    string(atom_to_list(A), F, Adj, P, Pad);
%% control($s, [L0], F, Adj, P, Pad, _I) ->
%%     L = iolist_to_chars(L0),
%%     string(L, F, Adj, P, Pad);
control($s, [L0], F, Adj, P, Pad, _I) ->
    L = unicode:characters_to_list(L0),
    string(L, F, Adj, P, Pad);
control($e, [A], F, Adj, P, Pad, _I) when is_float(A) ->
    fwrite_e(A, F, Adj, P, Pad);
control($f, [A], F, Adj, P, Pad, _I) when is_float(A) ->
    fwrite_f(A, F, Adj, P, Pad);
control($g, [A], F, Adj, P, Pad, _I) when is_float(A) ->
    fwrite_g(A, F, Adj, P, Pad);
control($b, [A], F, Adj, P, Pad, _I) when is_integer(A) ->
    unprefixed_integer(A, F, Adj, base(P), Pad, true);
control($B, [A], F, Adj, P, Pad, _I) when is_integer(A) ->
    unprefixed_integer(A, F, Adj, base(P), Pad, false);
control($x, [A,Prefix], F, Adj, P, Pad, _I) when is_integer(A),
                                                is_atom(Prefix) ->
    prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), true);
control($x, [A,Prefix], F, Adj, P, Pad, _I) when is_integer(A) ->
    true = io_lib:deep_char_list(Prefix), %Check if Prefix a character list
    prefixed_integer(A, F, Adj, base(P), Pad, Prefix, true);
control($X, [A,Prefix], F, Adj, P, Pad, _I) when is_integer(A),
                                                is_atom(Prefix) ->
    prefixed_integer(A, F, Adj, base(P), Pad, atom_to_list(Prefix), false);
control($X, [A,Prefix], F, Adj, P, Pad, _I) when is_integer(A) ->
    true = io_lib:deep_char_list(Prefix), %Check if Prefix a character list
    prefixed_integer(A, F, Adj, base(P), Pad, Prefix, false);
control($+, [A], F, Adj, P, Pad, _I) when is_integer(A) ->
    Base = base(P),
    Prefix = base_prefix(Base, true),
    prefixed_integer(A, F, Adj, Base, Pad, Prefix, true);
control($#, [A], F, Adj, P, Pad, _I) when is_integer(A) ->
    Base = base(P),
    Prefix = base_prefix(Base, false),
    prefixed_integer(A, F, Adj, Base, Pad, Prefix, false);
control($c, [A], F, Adj, P, Pad, _I) when is_integer(A) ->
    char(A, F, Adj, P, Pad);
control($~, [], F, Adj, P, Pad, _I) -> char($~, F, Adj, P, Pad);
control($n, [], F, Adj, P, Pad, _I) -> newline(F, Adj, P, Pad);
control($i, [_], _F, _Adj, _P, _Pad, _I) -> [].

%% Default integer base
base(none) -> 10;
base(B) when is_integer(B) -> B.

%% write(CharList, FieldWidth, Adjust, Precision, PadChar)
%%  Write the characters of a term. Use Precision to trim length of
%%  output.  Adjust the characters within the field if length less
%%  than Max padding with PadChar.

write(T, none, _Adj, none, _Pad) -> T;
write(T, W, Adj, P, Pad) ->
    N = lists:flatlength(T),
    if P =:= none -> write1(T, W, Adj, N, Pad);
       P >= N -> write1(T, W, Adj, N, Pad);
       true -> write1(flat_trunc(T, P), W, Adj, P, Pad)
    end.

write1(T, none, _Adj, _P, _Pad) -> T;
write1(T, W, Adj, N, Pad) ->
    if W < N -> chars($*, W);
       W == N -> T;
       true -> adjust(T, chars(Pad, W-N), Adj)
    end.

%% print(CharList, Depth, FieldWidth, Adjust, Precision, PadChar, Indentation)
%%  Pretty print the characters of a term, field width is maximum line
%%  length and precision is initial indentation.

print(T, D, none, Adj, P, Pad, I) -> print(T, D, 80, Adj, P, Pad, I);
print(T, D, W, Adj, none, Pad, I) -> print(T, D, W, Adj, I, Pad, I);
print(T, D, FW, right, P, _Pad, _I) ->
    lfe_io_pretty:term(T, D, P, FW).

%% fwrite_e(Float, FieldWidth, Adjust, Precision, PadChar)

fwrite_e(Fl, none, Adj, none, Pad) ->           %Default values
    fwrite_e(Fl, none, Adj, 6, Pad);
fwrite_e(Fl, none, _Adj, P, _Pad) when P >= 2 ->
    float_e(Fl, float_data(Fl), P);
fwrite_e(Fl, W, Adj, none, Pad) ->
    fwrite_e(Fl, W, Adj, 6, Pad);
fwrite_e(Fl, W, Adj, P, Pad) when P >= 2 ->
    write(float_e(Fl, float_data(Fl), P), W, Adj, W, Pad).

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

%% fwrite_f(FloatData, FieldWidth, Adjust, Precision, PadChar)

fwrite_f(Fl, none, Adj, none, Pad) ->           %Default values
    fwrite_f(Fl, none, Adj, 6, Pad);
fwrite_f(Fl, none, _Adj, P, _Pad) when P >= 1 ->
    float_f(Fl, float_data(Fl), P);
fwrite_f(Fl, W, Adj, none, Pad) ->
    fwrite_f(Fl, W, Adj, 6, Pad);
fwrite_f(Fl, W, Adj, P, Pad) when P >= 1 ->
    write(float_f(Fl, float_data(Fl), P), W, Adj, W, Pad).

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

%% fwrite_g(Float, FieldWidth, Adjust, Precision, PadChar)
%%  Use the f form if Float is >= 0.1 and < 1.0e4,
%%  and the prints correctly in the f form, else the e form.
%%  Precision always means the # of significant digits.

fwrite_g(Fl, W, Adj, none, Pad) ->
    fwrite_g(Fl, W, Adj, 6, Pad);
fwrite_g(Fl, W, Adj, P, Pad) when P >= 1 ->
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
            fwrite_f(Fl, W, Adj, P-1-E, Pad);
        P =< 1 ->
            fwrite_e(Fl, W, Adj, 2, Pad);
        true ->
            fwrite_e(Fl, W, Adj, P, Pad)
    end.

%% string(StringList, FieldWidth, Adjust, Precision, PadChar)
%%  Output a string adjusted with PadChar.

string(S, none, _, none, _) -> S;
string(S, W, Adj, P, Pad) ->
    N = lists:flatlength(S),
    if P =:= none -> string1(S, W, Adj, N, Pad);
       P >= N -> string1(S, W, Adj, N, Pad);
       true -> string1(flat_trunc(S, P), W, Adj, P, Pad)
    end.

string1(S, none, _Adj, _N, _Pad) -> S;
string1(S, W, Adj, N, Pad) ->
    if W < N -> flat_trunc(S, W);
       W == N -> S;
       true -> adjust(S, chars(Pad, W-N), Adj)
    end.

%% unprefixed_integer(Int, Field, Adjust, Base, PadChar, Lowercase) ->
%%      [Char].

unprefixed_integer(Int, W, Adj, Base, Pad, Lowercase)
  when Base >= 2, Base =< 1+$Z-$A+10 ->
    if Int < 0 ->
            S = cond_lowercase(erlang:integer_to_list(-Int, Base), Lowercase),
            write([$-|S], W, Adj, none, Pad);
       true ->
            S = cond_lowercase(erlang:integer_to_list(Int, Base), Lowercase),
            write(S, W, Adj, none, Pad)
    end.

%% prefixed_integer(Int, FieldWidth, Adjust, Base, PadChar, Prefix, Lowercase) ->
%%      [Char].

prefixed_integer(Int, W, Adj, Base, Pad, Prefix, Lowercase)
  when Base >= 2, Base =< 1+$Z-$A+10 ->
    if Int < 0 ->
            S = cond_lowercase(erlang:integer_to_list(-Int, Base), Lowercase),
            write([$-,Prefix|S], W, Adj, none, Pad);
       true ->
            S = cond_lowercase(erlang:integer_to_list(Int, Base), Lowercase),
            write([Prefix|S], W, Adj, none, Pad)
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

%% char(Char, FieldWidth, Adjust, Precision, PadChar) -> [Char].

char(C, none, _Adj, none, _Pad) -> [C];
char(C, W, _Adj, none, _Pad) -> chars(C, W);
char(C, none, _Adj, P, _Pad) -> chars(C, P);
char(C, W, Adj, P, Pad) when W >= P ->
    adjust(chars(C, P), chars(Pad, W - P), Adj).

%% newline(FieldWidth, Adjust, Precision, PadChar) -> [Char].

newline(none, _Adj, _P, _Pad) -> "\n";
newline(W, _Adj, _P, _Pad) -> chars($\n, W).

%%
%% Utilities
%%

adjust(Data, [], _Adj) -> Data;
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
