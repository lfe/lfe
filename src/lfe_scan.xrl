%% Copyright (c) 2008-2011 Robert Virding. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%% File    : lfe_scan.xrl
%% Author  : Robert Virding
%% Purpose : Token definitions for Lisp Flavoured Erlang.

Definitions.
B	= [01]
O	= [0-7]
D	= [0-9]
H	= [0-9a-fA-F]
B36	= [0-9a-zA-Z]
U	= [A-Z]
L	= [a-z]
A	= ({U}|{L})
DEL	= [][()}{";\000-\s]
SYM	= [^][()}{";\000-\s]
SSYM	= [^][()}{|";#`',\000-\s]
WS	= ([\000-\s]|;[^\n]*)

Rules.
%% Separaters
#[bB]\(		:	{token,{'#B(',TokenLine}}.
#\(		:	{token,{'#(',TokenLine}}.
#'		:	{token,{'#\'',TokenLine}}.
#`		:	{token,{'#`',TokenLine}}.
#;		:	{token,{'#;',TokenLine}}.
#,		:	{token,{'#,',TokenLine}}.
#,@		:	{token,{'#,@',TokenLine}}.
'		:	{token,{'\'',TokenLine}}.
`		:	{token,{'`',TokenLine}}.
,		:	{token,{',',TokenLine}}.
,@		:	{token,{',@',TokenLine}}.
\.		:	{token,{'.',TokenLine}}.
[][()}{]	:	{token,{list_to_atom(TokenChars),TokenLine}}.
%% Characters
#\\(x{H}+|.)	:	char_token(string:substr(TokenChars, 3), TokenLine).
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
%% Based numbers
#[bB]{B}+	:	base_token(string:substr(TokenChars, 3), 2, TokenLine).
#[oO]{O}+	:	base_token(string:substr(TokenChars, 3), 8, TokenLine).
#[dD]{D}+	:	base_token(string:substr(TokenChars, 3), 10, TokenLine).
#[xX]{H}+	:	base_token(string:substr(TokenChars, 3), 16, TokenLine).
#{D}+[rR]{B36}+ :
	%% Have to scan all possible digit chars and fail if wrong.
	{Base,[_|Ds]} = base1(string:substr(TokenChars, 2), 10, 0),
	base_token(Ds, Base, TokenLine).
%% Atoms
[+-]?{D}+		:
	case catch {ok,list_to_integer(TokenChars)} of
	    {ok,I} -> {token,{number,TokenLine,I}};
	    _ -> {error,"illegal integer"}
	end.
[+-]?{D}+\.{D}+([eE][+-]?{D}+)? :
	case catch {ok,list_to_float(TokenChars)} of
	    {ok,F} -> {token,{number,TokenLine,F}};
	    _ -> {error,"illegal float"}
	end.
{SSYM}{SYM}*	:
	symbol_token(TokenChars, TokenLine).
{WS}+		:	skip_token.

Erlang code.
%% Copyright (c) 2008-2011 Robert Virding. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

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
base1([C|Cs], Base, SoFar) when C >= $a, C =< $f, C < Base + $a - 10 ->
    Next = SoFar * Base + (C - $a + 10),
    base1(Cs, Base, Next);
base1([C|Cs], Base, SoFar) when C >= $A, C =< $F, C < Base + $A - 10 ->
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

hex_char(C) when C >= $0, C =< $9 -> true;
hex_char(C) when C >= $a, C =< $f -> true;
hex_char(C) when C >= $A, C =< $F -> true;
hex_char(_) -> false.

escape_char($n) -> $\n;				%\n = LF
escape_char($r) -> $\r;				%\r = CR
escape_char($t) -> $\t;				%\t = TAB
escape_char($v) -> $\v;				%\v = VT
escape_char($b) -> $\b;				%\b = BS
escape_char($f) -> $\f;				%\f = FF
escape_char($e) -> $\e;				%\e = ESC
escape_char($s) -> $\s;				%\s = SPC
escape_char($d) -> $\d;				%\d = DEL
escape_char(C) -> C.
