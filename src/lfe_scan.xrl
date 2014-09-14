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

%% File    : lfe_scan.xrl
%% Author  : Robert Virding
%% Purpose : Token definitions for Lisp Flavoured Erlang.

Definitions.
B    = [01]
O    = [0-7]
D    = [0-9]
H    = [0-9a-fA-F]
B36    = [0-9a-zA-Z]
U    = [A-Z]
L    = [a-z]
A    = ({U}|{L})
DEL    = [][()}{";\000-\s]
SYM    = [^][()}{";\000-\s]
SSYM    = [^][()}{|";#`',\000-\s]
WS    = ([\000-\s]|;[^\n]*)

Rules.
%% Bracketed Comments using #| foo |#
#\|[^\|]*\|+([^#\|][^\|]*\|+)*# : block_comment(string:substr(TokenChars, 3)).
%% Separators
#[bB]\(         :    {token,{'#B(',TokenLine}}.
#[mM]\(         :    {token,{'#M(',TokenLine}}.
#\(             :    {token,{'#(',TokenLine}}.
#`              :    {token,{'#`',TokenLine}}.
#;              :    {token,{'#;',TokenLine}}.
#,              :    {token,{'#,',TokenLine}}.
#,@             :    {token,{'#,@',TokenLine}}.
'               :    {token,{'\'',TokenLine}}.
`               :    {token,{'`',TokenLine}}.
,               :    {token,{',',TokenLine}}.
,@              :    {token,{',@',TokenLine}}.
\.              :    {token,{'.',TokenLine}}.
[][()}{]        :    {token,{list_to_atom(TokenChars),TokenLine}}.
%% Characters
#\\(x{H}+|.)    :    char_token(string:substr(TokenChars, 3), TokenLine).
%% String
"(\\x{H}+;|\\.|[^"])*" :
            %% Strip quotes.
            S = string:substr(TokenChars, 2, TokenLen - 2),
            {token,{string,TokenLine,chars(S)}}.
%% Symbols
\|(\\x{H}+;|\\.|[^|])*\| :
            %% Strip quotes.
            S = string:substr(TokenChars, 2, TokenLen - 2),
            symbol_token(chars(S), TokenLine).
%% Funs
#'{SSYM}{SYM}*/{D}+    :
            %% Strip sharpsign single-quote.
            FunStr = string:substr(TokenChars,3),
            {token,{'#\'',TokenLine,FunStr}}.
%% Based numbers
#[bB]{B}+    :    base_token(string:substr(TokenChars, 3), 2, TokenLine).
#[oO]{O}+    :    base_token(string:substr(TokenChars, 3), 8, TokenLine).
#[dD]{D}+    :    base_token(string:substr(TokenChars, 3), 10, TokenLine).
#[xX]{H}+    :    base_token(string:substr(TokenChars, 3), 16, TokenLine).
#([0]?[2-9]|[12][0-9]|3[0-6])[rR]{B36}+ :
    %% Have to scan all possible digit chars and fail if wrong.
    {Base,[_|Ds]} = base1(string:substr(TokenChars, 2), 10, 0),
    base_token(Ds, Base, TokenLine).

%% Atoms
[+-]?{D}+        :
    case catch {ok,list_to_integer(TokenChars)} of
        {ok,I} -> {token,{number,TokenLine,I}};
        _ -> {error,"illegal integer"}
    end.
[+-]?{D}+\.{D}+([eE][+-]?{D}+)? :
    case catch {ok,list_to_float(TokenChars)} of
        {ok,F} -> {token,{number,TokenLine,F}};
        _ -> {error,"illegal float"}
    end.
{SSYM}{SYM}*    :
    symbol_token(TokenChars, TokenLine).
{WS}+        :    skip_token.

Erlang code.
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

%% File    : lfe_scan.erl
%% Author  : Robert Virding
%% Purpose : Token definitions for Lisp Flavoured Erlang.

-import(string, [substr/2,substr/3]).

%% symbol_token(Chars, Line) -> {token,{symbol,Line,Symbol}} | {error,E}.
%% Build a symbol from list of legal characters, else error.

symbol_token(Cs, L) ->
    case catch {ok,list_to_atom(Cs)} of
    {ok,S} -> {token,{symbol,L,S}};
    _ -> {error,"illegal symbol"}
    end.

%% base_token(Chars, Base, Line) -> Integer.
%% Convert a string of Base characters into a number. We know that
%% the strings only contain the correct character.

base_token(Cs, B, L) ->
    case base1(Cs, B, 0) of
    {N,[]} -> {token,{number,L,N}};
    {_,_} -> {error,"illegal based number"}
    end.

base1([C|Cs], Base, SoFar) when C >= $0, C =< $9, C < Base + $0 ->
    Next = SoFar * Base + (C - $0),
    base1(Cs, Base, Next);
base1([C|Cs], Base, SoFar) when C >= $a, C =< $z, C < Base + $a - 10 ->
    Next = SoFar * Base + (C - $a + 10),
    base1(Cs, Base, Next);

base1([C|Cs], Base, SoFar) when C >= $A, C =< $Z, C < Base + $A - 10 ->
    Next = SoFar * Base + (C - $A + 10),
    base1(Cs, Base, Next);
base1([C|Cs], _Base, SoFar) -> {SoFar,[C|Cs]};
base1([], _Base, N) -> {N,[]}.

%% char_token(InputChars, Line) -> {token,{number,L,N}} | {error,E}.
%% Convert an input string into the corresponding character. We know
%% that the input string is correct.

char_token([$x,C|Cs], L) ->
    case base1([C|Cs], 16, 0) of
    {N,[]} -> {token,{number,L,N}};
    _ -> {error,"illegal character"}
    end;
char_token([C], L) -> {token,{number,L,C}}.

%% chars(InputChars) -> Chars.
%% Convert an input string into the corresponding string
%% characters. We know that the input string is correct.

chars([$\\,$x,C|Cs0]) ->
    case hex_char(C) of
    true ->
        case base1([C|Cs0], 16, 0) of
        {N,[$;|Cs1]} -> [N|chars(Cs1)];
        _Other -> [escape_char($x)|chars([C|Cs0])]
        end;
    false -> [escape_char($x)|chars([C|Cs0])]
    end;
chars([$\\,C|Cs]) -> [escape_char(C)|chars(Cs)];
chars([C|Cs]) -> [C|chars(Cs)];
chars([]) -> [].

%% Block Comment:
%% Provide a sensible error when people attempt to include nested
%% comments because currently the parser cannot process them without
%% a rebuild. But simply exploding on a '#|' is not going to be
%% that helpful.
block_comment(TokenChars) ->
  %% Check we're not opening another comment block.
  case string:str(TokenChars, "#|") of
    0 -> skip_token; %% No nesting found
    _ -> {error, "illegal nested block comment"}
  end.

hex_char(C) when C >= $0, C =< $9 -> true;
hex_char(C) when C >= $a, C =< $f -> true;
hex_char(C) when C >= $A, C =< $F -> true;
hex_char(_) -> false.

escape_char($n) -> $\n;                %\n = LF
escape_char($r) -> $\r;                %\r = CR
escape_char($t) -> $\t;                %\t = TAB
escape_char($v) -> $\v;                %\v = VT
escape_char($b) -> $\b;                %\b = BS
escape_char($f) -> $\f;                %\f = FF
escape_char($e) -> $\e;                %\e = ESC
escape_char($s) -> $\s;                %\s = SPC
escape_char($d) -> $\d;                %\d = DEL
escape_char(C) -> C.
