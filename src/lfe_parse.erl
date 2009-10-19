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

%% File    : lfe_parse.erl
%% Author  : Robert Virding
%% Purpose : A simple Sexpr parser.
%% A simple sexpr parser. It is not re-entrant but does return excess tokens.

-module(lfe_parse).

-export([sexpr/1,format_error/1]).

-import(lists, [reverse/1,reverse/2]).

%% format_error(Error) -> String.
%%  Format errors to printable string.

format_error({missing,Tok}) ->
    io_lib:fwrite("missing ~p", [Tok]);
format_error({illegal,What}) ->
    io_lib:fwrite("illegal ~p", [What]).

%% sexpr(Tokens) -> {ok,Line,Sexpr,RestTokens} | {error,Error,RestTokens}.

sexpr([T|_]=Ts) ->
    L = line(T),
    try
	{Sexpr,R} = sexpr1(Ts),
	{ok,L,Sexpr,R}
    catch
	throw: {error,E,Rest} ->
	    {error,{L,lfe_parse,E},Rest}
    end.

%% Atoms.
sexpr1([{symbol,_,S}|Ts]) -> {S,Ts};
sexpr1([{number,_,N}|Ts]) -> {N,Ts};
sexpr1([{string,_,S}|Ts]) -> {S,Ts};
%% Lists.
sexpr1([{'(',_},{')',_}|Ts]) -> {[],Ts};
sexpr1([{'(',_}|Ts0]) ->
    {S,Ts1} = sexpr1(Ts0),
    case list_tail(Ts1, ')') of
	{Tail,[{')',_}|Ts2]} -> {[S|Tail],Ts2};
	{_,Ts2} -> throw({error,{missing,')'},Ts2})
    end;
sexpr1([{'[',_},{']',_}|Ts]) -> {[],Ts};
sexpr1([{'[',_}|Ts0]) ->
    {S,Ts1} = sexpr1(Ts0),
    case list_tail(Ts1, ']') of
	{Tail,[{']',_}|Ts2]} -> {[S|Tail],Ts2};
	{_,Ts2} -> throw({error,{missing,']'},Ts2})
    end;
%% Tuple constants (using vector constant syntax).
sexpr1([{'#(',_}|Ts0]) ->
    case proper_list(Ts0) of
	{List,[{')',_}|Ts1]} -> {list_to_tuple(List),Ts1};
	{_,Ts1} -> throw({error,{missing,')'},Ts1})
    end;
%% Binaries and bitstrings constants (our own special syntax).
sexpr1([{'#B(',_}|Ts0]) ->
    case proper_list(Ts0) of
	{List,[{')',_}|Ts1]} ->
	    %% Build and eval a binary sexpr.
	    case catch lfe_eval:expr([binary|List]) of
		Bin when is_bitstring(Bin) -> {Bin,Ts1};
		_ -> throw({error,{illegal,binary},Ts1})
	    end;
	{_,Ts1} -> throw({error,{missing,')'},Ts1})
    end;
%% Quotes.
sexpr1([{'\'',_}|Ts0]) ->			%Quote
    {S,Ts1} = sexpr1(Ts0),
    {[quote,S],Ts1};
sexpr1([{'`',_}|Ts0]) ->			%Backquote
    {S,Ts1} = sexpr1(Ts0),
    {[backquote,S],Ts1};
sexpr1([{',',_}|Ts0]) ->			%Unquote
    {S,Ts1} = sexpr1(Ts0),
    {[unquote,S],Ts1};
sexpr1([{',@',_}|Ts0]) ->			%Unquote splicing
    {S,Ts1} = sexpr1(Ts0),
    {['unquote-splicing',S],Ts1};
%% Error cases.
sexpr1([T|_]) ->
    throw({error,{illegal,op(T)},[]});
sexpr1([]) ->
    throw({error,{missing,token},[]}).

%% list_tail(Tokens, EndToken) -> {List,Tokens}.
%%  Parse tail of list allowing dotted pair.

list_tail(Ts, End) -> list_tail(Ts, End, []).

list_tail([{End,_}|_]=Ts, End, Es) -> {reverse(Es),Ts};
list_tail([{'.',_}|Ts0], _, Es) ->
    {T,Ts1} = sexpr1(Ts0),
    {reverse(Es, T),Ts1};
list_tail(Ts0, End, Es) ->
    {E,Ts1} = sexpr1(Ts0),
    list_tail(Ts1, End, [E|Es]).

%% proper_list(Tokens) -> {List,Tokens}.
%%  Parse a proper list.

proper_list(Ts) -> proper_list(Ts, []).

proper_list([{')',_}|_]=Ts, Es) -> {reverse(Es),Ts};
proper_list(Ts0, Es) ->
    {E,Ts1} = sexpr1(Ts0),
    proper_list(Ts1, [E|Es]).

%% Utilities.
op(T) -> element(1, T).
line(T) -> element(2, T).
val(T) -> element(3, T).
