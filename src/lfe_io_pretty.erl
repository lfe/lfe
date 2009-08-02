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

%% File    : lfe_io_pretty.erl
%% Purpose : Pretty printer for Lisp Flavoured Erlang.

-module(lfe_io_pretty).

-export([print1/1,print1/2,print1/3,print1/4]).

%% -compile(export_all).

-import(lists, [flatten/1]).

%% print1(Sexpr) -> [char()].
%% print1(Sexpr, Depth) -> [char()].
%% print1(Sexpr, Depth, Indentation, LineLength) -> [char()].
%%  An relatively simple pretty print function, but with some
%%  customisation.

print1(S) -> print1(S, -1, 0, 80).

print1(S, D) -> print1(S, D, 0, 80).

print1(S, D, I) -> print1(S, D, I, 80).

print1(_, 0, _, _) -> "...";
print1(Symb, _, _, _) when is_atom(Symb) -> lfe_io:print1_symb(Symb);
print1(Numb, _, _, _) when is_integer(Numb) -> integer_to_list(Numb);
print1(Numb, _, _, _) when is_float(Numb) -> float_to_list(Numb);
%% Handle some default special cases, standard character macros. These
%% don't increase depth as they really should.
print1([quote,E], D, I, L) -> ["'",print1(E, D, I+1, L)];
print1([backquote,E], D, I, L) -> ["`",print1(E, D, I+1, L)];
print1([unquote,E], D, I, L) -> [",",print1(E, D, I+1, L)];
print1(['unquote-splicing',E], D, I, L) -> [",@",print1(E, D, I+2, L)];
print1([Car|Cdr]=List, D, I, L) ->
    %% Customise printing lists.
    case indent_type(Car) of
	none ->
	    %% Handle printable lists specially.
	    case io_lib:printable_list(List) of
		true -> lfe_io:print1_string(List, 34);	%"
		false ->
		    %% Raw printing.
		    Flat = flatten(lfe_io:print1(List, D)),
		    if length(Flat) + I < L -> Flat;
		       true ->
			    ["(",print1(Car, D-1, I+1, L),
			     print1_tail(Cdr, D-1, I+1, L),")"]
		    end
	    end;
	N when is_integer(N) ->
	    %% Handle special lists.
	    Cs = atom_to_list(Car),		%We KNOW Car in an atom.
	    NewI = I + length(Cs) + 2,
	    Tcs = case split(N, Cdr) of
		      {[S|Ss],Rest} ->
			  [print1(S, D-1, NewI, L),
			   print1_tail(Ss, D-1, NewI, L),
			   print1_tail(Rest, D-1, I+2, L)];
		      {[],Rest} -> [print1_tail(Rest, D-1, I+2, L)]
		  end,
	    ["(" ++ Cs," ",Tcs,")"]
    end;
print1([], _, _, _) -> "()";
print1({}, _, _, _) -> "#()";
print1(Tup, D, I, L) when is_tuple(Tup) ->
    Flat = flatten(lfe_io:print1(Tup, D)),
    if length(Flat) + I < L -> Flat;
       true ->
	    [E|Es] = tuple_to_list(Tup),
	    ["#(",print1(E, D-1, I+2, L),print1_tail(Es, D-1, I+2, L),")"]
    end;
print1(Bit, _, _, _) when is_bitstring(Bit) ->
    ["#B(",lfe_io:print1_bits(Bit, 30),$)];	%First 30 bytes
print1(Other, _, _, _) ->			%Use standard Erlang for rest
    io_lib:write(Other).

%% split(N, List) -> {List1,List2}.
%%  Split a list into two lists, the first containing the first N
%%  elements and the second the rest. Be tolerant of too few elements.

split(0, L) -> {[],L};
split(_, []) -> {[],[]};
split(N, [H|T]) ->
    {H1,T1} = split(N-1, T),
    {[H|H1],T1}.

%% print1_tail(Tail, Depth, Indentation, LineLength)
%% Print the tail of a list. We know about dotted pairs.

print1_tail([], _, _, _) -> "";
print1_tail(_, 1, _, _) -> " ...";
print1_tail([Car|Cdr], D, I, L) ->
    ["\n",blanks(I, []),print1(Car, D-1, I, L),print1_tail(Cdr, D-1, I, L)];
print1_tail(S, D, I, L) ->
    [" .\n",blanks(I, print1(S, D-1, I, L))].

blanks(N, Tail) -> string:chars($\s, N, Tail).

%% indent_type(Form) -> N | none.
%%  Defines special indentation. None means default, N is number of
%%  sexprs in list which are indented *after* Form while all following
%%  that end up at indent+2.

%% Old style forms.
indent_type('define') -> 1;
indent_type('define-module') -> 1;
indent_type('define-syntax') -> 1;
indent_type('define-record') -> 1;
indent_type('begin') -> 0;
indent_type('let-syntax') -> 1;
indent_type('syntax-rules') -> 0;
indent_type('macro') -> 0;
%% New style forms.
indent_type('defmodule') -> 1;
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
indent_type('let-macro') -> 1;
indent_type('if') -> 1;
indent_type('case') -> 1;
indent_type('receive') -> 0;
indent_type('catch') -> 0;
indent_type('try') -> 1;
indent_type('call') -> 2;
indent_type('define-function') -> 1;
indent_type('define-macro') -> 1;
indent_type('eval-when-compile') -> 0;
%% Core macros.
indent_type(':') -> 2;
indent_type('cond') -> 999;			%All following forms
indent_type('let*') -> 1;
indent_type('flet') -> 1;
indent_type('flet*') -> 1;
indent_type('fletrec') -> 1;
indent_type(macrolet) -> 1;
indent_type(syntaxlet) -> 1;
indent_type('do') -> 2;
indent_type('lc') -> 1;				%List comprehensions
indent_type('bc') -> 1;				%Binary comprehensions
indent_type(_) -> none.
