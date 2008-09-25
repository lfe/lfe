%% Copyright (c) 2008 Robert Virding. All rights reserved.
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

%% File    : lfe_io.erl
%% Purpose : Some basic i/o functions for Lisp Flavoured Erlang.
%%
%% Very primitive versions.

-module(lfe_io).

-export([parse_file/1,read_file/1,read/0,read/1,print/1,print/2,print1/1,
	prettyprint/1,prettyprint/2,prettyprint1/2]).

%% -compile(export_all).

-import(lists, [flatten/1,reverse/1,reverse/2,map/2,mapfoldl/3]).

%% parse_file(FileName) -> {ok,[{Sexpr,Line}]} | {error,Error}.
%% Parse a file returning the raw sexprs (as it should be) and line
%% numbers of start of each sexpr.

parse_file(Name) ->
    %% io:format("~p\n", [Name]),
    {ok,F} = file:open(Name, [read]),
    {ok,Ts,_} = io:request(F, {get_until,'',lfe_scan,tokens,[1]}),
    parse_file1(Ts, []).

parse_file1(Ts0, Ss) when Ts0 /= [] ->
    case lfe_parse:parse(Ts0) of
	{ok,L,S,Ts1} -> parse_file1(Ts1, [{S,L}|Ss]);
	{error,E,_} -> {error,E}
    end;
parse_file1([], Ss) -> {ok,reverse(Ss)}.

%% read_file(FileName) -> [Sexpr].
%% Read a file returning the raw sexprs (as it should be).

read_file(Name) ->
    %% io:format("~p\n", [Name]),
    {ok,F} = file:open(Name, [read]),
    {ok,Ts,_} = io:request(F, {get_until,'',lfe_scan,tokens,[1]}),
    read_file1(Ts).

read_file1(Ts0) when Ts0 /= [] ->
    case lfe_parse:parse(Ts0) of
	{ok,_,S,Ts1} -> [S|read_file1(Ts1)];
	{error,E,_} -> exit({error,E})
    end;
read_file1([]) -> [].

%% read([IoDevice]) -> Sexpr.
%%  A very simple read function. Line oriented but can handle multiple
%%  lines. Anything remaining on last line after a sexpr is lost.
%%  Signal errors.

read() -> read(standard_io).
read(Io) ->
    scan_and_parse(Io, []).

scan_and_parse(Io, Ts0) ->
    case io:get_line(Io, '') of
	eof ->
	    %% No more so must take what we have.
	    case lfe_parse:parse(Ts0) of
		{ok,_,S,_} -> S;
		{error,E,_} -> exit({error,E})
	    end;
	Cs ->
	    case lfe_scan:string(Cs) of
		{ok,[],_} ->
		    %% Empty line (token free) just go on.
		    scan_and_parse(Io, Ts0);
		{ok,More,_} ->
		    Ts1 = Ts0 ++ More,
		    case lfe_parse:parse(Ts1) of
			{ok,_,S,_} -> S;
			{error,{_,_,{missing,_}},_} ->
			    scan_and_parse(Io, Ts1);
			{error,E,_} -> exit({error,E})
		    end;
		E -> exit(E)
	    end
    end.
    
%% print([IoDevice], Sexpr) -> [char()].
%% A very simple print function. Does not pretty-print.

print(S) -> print(standard_io, S).
print(Io, S) -> io:put_chars(Io, print1(S)).

print1(Symb) when is_atom(Symb) -> print1_symb(Symb);
print1(Numb) when is_integer(Numb) -> integer_to_list(Numb);
print1(Numb) when is_float(Numb) -> float_to_list(Numb);
%% Handle some default special cases.
print1([quote,E]) -> [$'|print1(E)];
print1([quasiquote,E]) -> [$`|print1(E)];
print1([unquote,E]) -> [$,|print1(E)];
print1(['unquote-splicing',E]) -> [",@"|print1(E)];
%%print1([binary|Es]) -> ["#B"|print1(Es)];
print1([E|Es]) ->
    [$(,print1(E),print1_tail(Es),$)];
print1([]) -> "()";
print1(Vec) when is_tuple(Vec) ->
    case tuple_to_list(Vec) of
	[E|Es] -> ["#(",print1(E),print1_tail(Es),$)];
	[] -> "#()"
    end;
print1(Bit) when is_bitstring(Bit) ->
    ["#B(",print1_bits(Bit),$)].

%% print1_symb(Symbol) -> [char()].

print1_symb(Symb) ->
    Cs = atom_to_list(Symb),
    case quote_symbol(Symb, Cs) of
	true -> print_string(Cs , $|);
	false -> Cs
    end.

%% print1_bits(Bitstring)
%% Print the bytes in a bitstring. Print bytes except for last which
%% we print as bitstring segement if not 8 bits big.

print1_bits(<<B:8>>) -> integer_to_list(B);	%Catch last binary byte
print1_bits(<<B:8,Bits/bitstring>>) ->
    [integer_to_list(B),$\s|print1_bits(Bits)];
print1_bits(<<>>) -> [];
print1_bits(Bits) ->				%0 < Size < 8
    N = bit_size(Bits),
    <<B:N>> = Bits,
    io_lib:format("(~w bitstring (size ~w))", [B,N]).

%% print1_tail(Tail)
%% Print the tail of a list. We know about dotted pairs.

print1_tail([S|Ss]) ->
    [$\s,print1(S)|print1_tail(Ss)];
print1_tail([]) -> [];
print1_tail(S) -> [" . "|print1(S)].

%% quote_symbol(Symbol, SymbChars) -> bool().
%% Check if symbol needs to be quoted when printed. If it can read as
%% a number then it must be quoted.

quote_symbol(_, [C|Cs]=Cs0) ->
    case catch {ok,list_to_float(Cs0)} of
	{ok,_} -> true;
	_ -> case catch {ok,list_to_integer(Cs0)} of
		 {ok,_} -> true;
		 _ -> not (start_symb_char(C) andalso symb_chars(Cs))
	     end
    end;
quote_symbol(_, []) -> true.

symb_chars([C|Cs]) ->
    case symb_char(C) of
	true -> symb_chars(Cs);
	false -> false
    end;
symb_chars([]) -> true.

start_symb_char($#) -> false;
start_symb_char($`) -> false;
start_symb_char($') -> false;
start_symb_char($,) -> false;
start_symb_char($|) -> false;			%Symbol quote character
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

%% print_string([Char], QuoteChar) -> [Char]
%%  Generate the list of characters needed to print a string.

print_string(S, Q) ->
    [Q|print_string1(S, Q)].

print_string1([], Q) ->    [Q];
print_string1([C|Cs], Q) ->
    string_char(C, Q, print_string1(Cs, Q)).

string_char(Q, Q, Tail) -> [$\\,Q|Tail];	%Must check these first!
string_char($\\, _, Tail) -> [$\\,$\\|Tail];
string_char(C, _, Tail) when C >= $\s, C =< $~ ->
    [C|Tail];
string_char(C, _, Tail) when C >= $\240, C =< $\377 ->
    [C|Tail];
string_char($\n, _, Tail) -> [$\\,$n|Tail];	%\n = LF
string_char($\r, _, Tail) -> [$\\,$r|Tail];	%\r = CR
string_char($\t, _, Tail) -> [$\\,$t|Tail];	%\t = TAB
string_char($\v, _, Tail) -> [$\\,$v|Tail];	%\v = VT
string_char($\b, _, Tail) -> [$\\,$b|Tail];	%\b = BS
string_char($\f, _, Tail) -> [$\\,$f|Tail];	%\f = FF
string_char($\e, _, Tail) -> [$\\,$e|Tail];	%\e = ESC
string_char($\d, _, Tail) -> [$\\,$d|Tail];	%\d = DEL
string_char(C, _, Tail) ->			%Other control characters.
    C1 = hex(C bsr 4),
    C2 = hex(C band 15),
    [$\\,$x,C1,C2,$;|Tail].

hex(C) when C >= 0, C < 10 -> C + $0;
hex(C) when C >= 10, C < 16 -> C + $a.

%% prettyprint([IoDevice], Sexpr) -> [char()].
%%  An extremely simple pretty print function, but with some
%%  customisation.

prettyprint(S) -> prettyprint(standard_io, S).
prettyprint(Io, S) -> io:put_chars(Io, prettyprint1(S, 0)).

prettyprint1(Symb, _) when is_atom(Symb) -> print1_symb(Symb);
prettyprint1(Numb, _) when is_integer(Numb) -> integer_to_list(Numb);
prettyprint1(Numb, _) when is_float(Numb) -> float_to_list(Numb);
prettyprint1([quote,E], I) ->
    ["'",prettyprint1(E, I+1)];
prettyprint1([quasiquote,E], I) ->
    ["`",prettyprint1(E, I+1)];
prettyprint1([unquote,E], I) ->
    [",",prettyprint1(E, I+1)];
prettyprint1(['unquote-splicing',E], I) ->
    [",@",prettyprint1(E, I+1)];
prettyprint1([Car|Cdr]=List, I) ->
    %% Customise printing lists.
    case indent_type(Car) of
	none ->
	    %% Handle printable lists specially.
	    case io_lib:printable_list(List) of
		true -> print_string(List, 34);	%"
		false ->
		    %% Raw printing.
		    Flat = flatten(print1(List)),
		    if length(Flat) + I < 80 -> Flat;
		       true ->
			    ["(",prettyprint1(Car, I+1),pp_tail(Cdr, I+1),")"]
		    end
	    end;
	N when is_integer(N) ->
	    %% Handle special lists.
	    Cs = atom_to_list(Car),		%WE KNOW Car in an atom.
	    NewI = I + length(Cs) + 2,
	    Tcs = case split(N, Cdr) of
		      {[S|Ss],Rest} ->
			  [prettyprint1(S, NewI),pp_tail(Ss, NewI),
			   pp_tail(Rest, I+2)];
		      {[],Rest} -> [pp_tail(Rest, I+2)]
		  end,
	    ["(" ++ Cs," ",Tcs,")"]
    end;
prettyprint1([], _) -> "()";
prettyprint1({}, _) -> "#()";
prettyprint1(Tup, I) when is_tuple(Tup) ->
    Flat = flatten(print1(Tup)),
    if length(Flat) + I < 80 -> Flat;
       true ->
	    List = tuple_to_list(Tup),
	    ["#(",prettyprint1(hd(List), I+2),pp_tail(tl(List), I+2),")"]
    end;
prettyprint1(Bit, _) when is_bitstring(Bit) ->
    ["#B(",print1_bits(Bit),$)].

%% split(N, List) -> {List1,List2}.
%%  Split a list into two lists, the first containing the first N
%%  elements and the second the rest. Be tolerant of too few elements.

split(0, L) -> {[],L};
split(_, []) -> {[],[]};
split(N, [H|T]) ->
    {H1,T1} = split(N-1, T),
    {[H|H1],T1}.

pp_tail([Car|Cdr], I) ->
    ["\n",blanks(I, []),prettyprint1(Car, I),pp_tail(Cdr, I)];
pp_tail([], _) -> [];
pp_tail(S, I) ->
    [" .\n",blanks(I, prettyprint1(S, I))].

blanks(N, Tail) -> string:chars($\s, N, Tail).

%% indent_type(Form) -> N | none.
%%  Defines special indentation. None means default, N is number of
%%  sexprs in list which are indented *after* Form while all following
%%  that end up at indent+2.

%% Old style functions.
indent_type('define') -> 1;
indent_type('define-module') -> 1;
indent_type('define-syntax') -> 1;
indent_type('define-record') -> 1;
indent_type('begin') -> 0;
indent_type('let-syntax') -> 1;
indent_type('syntax-rules') -> 0;
indent_type('macro') -> 0;
%% New style functions.
indent_type('define-function') -> 1;
indent_type('define-macro') -> 1;
indent_type('defun') -> 1;
indent_type('defmacro') -> 1;
indent_type('defsyntax') -> 1;
indent_type('defrecord') -> 1;
%% Core forms.
indent_type('progn') -> 0;
indent_type('lambda') -> 1;
indent_type('match-lambda') -> 0;
indent_type('let') -> 1;
indent_type('let-function') -> 1;
indent_type('letrec-function') -> 1;
indent_type('if') -> 1;
indent_type('case') -> 1;
indent_type('receive') -> 0;
indent_type('cond') -> 999;			%All following forms
indent_type('catch') -> 0;
indent_type('try') -> 1;
indent_type('call') -> 2;
%% Core macros.
indent_type('let*') -> 1;
indent_type('flet') -> 1;
indent_type('flet*') -> 1;
indent_type('fletrec') -> 1;
indent_type('lc') -> 1;				%List comprehensions
indent_type('bc') -> 1;				%Binary comprehensions
indent_type(':') -> 2;
indent_type(_) -> none.
