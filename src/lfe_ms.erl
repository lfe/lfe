%% Copyright (c) 2011 Robert Virding. All rights reserved.
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

%%% File    : lfe_ms.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang match specification expander.

%%% Expand match specification into vanilla compatible data
%%% structure. We assume that all macros in the match spec have
%%% already been expanded. These functions are intended to be used
%%% within macros so they return code which when evaluated return the
%%% match-spec.

-module(lfe_ms).

-export([expand/1,format_error/1]).

-import(lists, [mapfoldl/3]).

%% ets:test_ms/2.

%% format_error(Error) -> ErrorString.

format_error(E) -> erlang:error({unknown_error,E}). %Remove later!

-define(Q(E), [quote,E]).			%We do a lot of quoting!

-record(ms, {dc=0,				%Dollar variable count
	     bs=[]				%Variable/$var bindings
	    }).

%% expand(MSBody) -> Expansion.
%% Expand the match spec body.

expand(Cls) ->
    {Exp,_} = clauses(Cls, #ms{}),
    Exp.

%% clauses(MSClauses, State) -> {Patterns,State}.

clauses([Cl0|Cls0], St0) ->
    {Cl1,St1} = clause(Cl0, St0),
    {Cls1,St2} = clauses(Cls0, St1),
    {[cons,Cl1,Cls1],St2};
clauses([], St) -> {[],St}.

%% clause(ClauseBody, State) -> {{Head,Guard,Body},State}.

clause([H0,['when'|G0]|B0], St0) ->
    St1 = St0#ms{dc=0,bs=[]},			%Reset clause local data
    {H1,St2} = head(H0, St1),
    {G1,St3} = guard(G0, St2),
    {B1,St4} = body(B0, St3),
    {[tuple,H1,G1,B1],St4};
clause([H0|B0], St0) ->
    {H1,St1} = head(H0, St0),
    {B1,St2} = body(B0, St1),
    {[tuple,H1,[],B1],St2}.

%% head(Patterns, State) -> {Pattern,State}.
%% Expand a head which can only consist of one argument. Only allow
%% aliasing at the top-level and only to a variable.

head([['=',S,Pat]], St0) when is_atom(S) ->
    St1 = new_binding(S, '$_', St0),
    pattern(Pat, St1);
head([['=',Pat,S]], St0) when is_atom(S) ->
    St1 = new_binding(S, '$_', St0),
    pattern(Pat, St1);
%% head([Pat], St0) when is_atom(Pat) ->	%WRONG!!
%%     St1 = new_binding(Pat, '$_', St0),
%%     pattern(Pat, St1);
head([Pat], St) ->
    pattern(Pat, St).

pattern('_', St) -> {?Q('_'),St};
pattern(Symb, St0) when is_atom(Symb) ->
    {Dv,St1} = pat_binding(Symb, St0),
    {?Q(Dv),St1};
pattern([quote,_]=E, St) -> {E,St};
pattern([cons,H0,T0], St0) ->
    {H1,St1} = pattern(H0, St0),
    {T1,St2} = pattern(T0, St1),
    {[cons,H1,T1],St2};
pattern([list|Ps0], St0) ->
    {Ps1,St1} = pat_list(Ps0, St0),
    {[list|Ps1],St1};
pattern([tuple|Ps0], St0) ->
    {Ps1,St1} = pat_list(Ps0, St0),
    {[tuple|Ps1],St1};
%% Support old no constructor style list forms.
pattern([H0|T0], St0) ->
    {H1,St1} = pattern(H0, St0),
    {T1,St2} = pattern(T0, St1),
    {[H1,T1],St2};
pattern(E, St) -> {E,St}.			%Atomic

pat_list(Ps, St) -> mapfoldl(fun pattern/2, St, Ps).

%% pat_binding(Var, Status) -> {DVar,Status}.
%% Get dollar var for variable, creating a new one if neccessary.

pat_binding(Var, St0) ->
    case find_binding(Var, St0) of
	{ok,Dv} -> {Dv,St0};
	error ->
	    {Dv,St1} = new_dollar(St0),
	    {Dv,new_binding(Var, Dv, St1)}
    end.

guard([T0|Ts0], St0) -> 
    {T1,St1} = test(T0, St0),
    {Ts1,St2} = guard(Ts0, St1),
    {[cons,T1,Ts1],St2};
guard([], St) -> {[],St}.

test(T, St) -> expr(T, St).			%Nothing special here yet

body([E0|Es0], St0) ->
    {E1,St1} = expr(E0, St0),
    {Es1,St2} = body(Es0, St1),
    {[cons,E1,Es1],St2};
body([], St) -> {[],St}.

expr(S, St) when is_atom(S) ->
    case find_binding(S, St) of
	{ok,Dv}  -> {?Q(Dv),St};
	error -> {[tuple,?Q(const),?Q(S)],St}
    end;
expr([quote,_]=E, St) -> {E,St};
expr([cons,H0,T0], St0) ->
    {H1,St1} = expr(H0, St0),
    {T1,St2} = expr(T0, St1),
    {[cons,H1,T1],St2};
expr([list|Es0], St0) ->
    {Es1,St1} = expr_list(Es0, St0),
    {[list|Es1],St1};
expr([tuple|Es0], St0) ->
    {Es1,St1} = expr_list(Es0, St0),
    {[tuple,[tuple|Es1]],St1};			%Yes this is what it is
%% Special match spec calls.
expr([bindings], St) -> {?Q('$*'),St};		%Special calls
expr([object], St) -> {?Q('$_'),St};
%% General function calls.
expr([call,?Q(erlang),?Q(Op)|Es], St) when is_atom(Op) ->
    expr([Op|Es], St);				%No checking!
expr([Op|Es0], St0) when is_atom(Op) ->
    {Es1,St1} = expr_list(Es0, St0),		%No checking!
    {[tuple,?Q(Op)|Es1],St1};
expr(T, St) when is_tuple(T) ->			%Constant
    {[tuple,T],St};				%???
expr(E, St) -> {E,St}.				%Atomic

expr_list(Es, St) -> mapfoldl(fun expr/2, St, Es).

%% new_binding(Name, Value, State) -> State.
%% find_binding(Name, State) -> {ok,Value} | error.
%% fetch_binding(Name, State) -> Value.

new_binding(Var, Val, #ms{bs=Bs}=St) ->
    St#ms{bs=orddict:store(Var, Val, Bs)}.

find_binding(Var, #ms{bs=Bs}) ->
    orddict:find(Var, Bs).

fetch_binding(Var, #ms{bs=Bs}) ->
    orddict:fetch(Var, Bs).

new_dollar(St) ->
    C = St#ms.dc,
    {list_to_atom("$" ++ integer_to_list(C)),St#ms{dc=C+1}}.
