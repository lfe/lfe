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

%%% File    : lfe_trans.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang translator.

%%% Translate LFE code to/from vanilla Erlang AST.

-module(lfe_trans).

-export([from_vanilla/1,to_vanilla/2]).

-import(lists, [map/2,foldr/3,splitwith/2]).

-define(Q(E), [quote,E]).			%We do a lot of quoting

%% from_vanilla(AST) -> Sexpr.

from_vanilla(AST) -> from_expr(AST).

from_expr({var,_,V}) -> V;
from_expr({nil,_}) -> [];
from_expr({integer,_,I}) -> I;
from_expr({float,_,F}) -> F;
from_expr({atom,_,A}) -> ?Q(A);			%Must quote here
from_expr({string,_,S}) -> ?Q(S);		%Must quote here
from_expr({cons,_,H,T}) ->
    [cons,from_expr(H),from_expr(T)];
from_expr({tuple,_,Es}) ->
    [tuple|from_expr_list(Es)];
from_expr({bin,_,Segs}) ->
    [binary|from_bitsegs(Segs)];
%% Core closure special forms.
from_expr({'fun',_,{clauses,Cls}}) ->
    ['match-lambda'|from_fun_cls(Cls)];		%Don't bother using lambda
from_expr({'fun',_,{function,F,A}}) ->
    ['fun',F,A];				%Return macros here?
from_expr({'fun',_,{function,M,F,A}}) ->
    ['fun',M,F,A];
%% Core control special forms.
from_expr({block,_,Es}) -> [progn|from_body(Es)];
from_expr({'case',_,E,Cls}) ->
    ['case',from_expr(E)|from_icrt_cls(Cls)];
from_expr({'if',_,Cls}) ->
    ['case',[]|from_icrt_cls(Cls)];
from_expr({'receive',_,Cls}) ->
    ['receive'|from_icrt_cls(Cls)];
from_expr({'receive',_,Cls,Timeout,Body}) ->
    ['receive'|from_icrt_cls(Cls) ++
	 [['after',from_expr(Timeout)|from_body(Body)]]];
from_expr({op,_,Op,A}) -> [Op,from_expr(A)];
from_expr({op,_,Op,L,R}) -> [Op,from_expr(L),from_expr(R)];
%% Function calls.
from_expr({call,_,{remote,_,M,F},As}) ->	%Remote function call
    [call,from_expr(M),from_expr(F)|from_expr_list(As)];
from_expr({call,_,{atom,_,F},As}) ->		%Local function call
    [F|from_expr_list(As)];
from_expr({call,_,F,As}) ->			%F not an atom or remote
    [funcall,from_expr(F)|from_expr_list(As)].

%% from_body(Expressions) -> Body.
%%  Handle '=' specially here and translate into let containing rest
%%  of body.

from_body([{match,_,P,E}|Es]) ->
    [['let',[[from_pat(P),from_expr(E)]]|from_body(Es)]];
from_body([E|Es]) ->
    [from_expr(E)|from_body(Es)];
from_body([]) -> [].

from_body_list(Bs) -> [ from_body(B) || B <- Bs ].

from_expr_list(Es) -> [ from_expr(E) || E <- Es ].

%% from_icrt_cls(Clauses) -> Clauses.
%% from_icrt_cl(Clause) -> Clause.
%%  If/case/receive/try clauses.
%%  No ; in guards!

from_icrt_cls(Cls) ->
    map(fun (Cl) -> from_icrt_cl(Cl) end, Cls).

from_icrt_cl({clause,_,[],[G],B}) ->		%If clause
    ['_',['when'|from_body(G)]|from_body(B)];
from_icrt_cl({clause,_,H,[],B}) ->
    [Lh] = from_pat_list(H),			%List of one
    [Lh|from_body(B)];
from_icrt_cl({clause,_,H,[G],B}) ->
    [Lh] = from_pat_list(H),			%List of one
    [Lh,['when'|from_body(G)]|from_body(B)].

%% from_fun_cls(Clauses) -> Clauses.
%% from_fun_cl(Clause) -> Clause.
%%  Function clauses.

from_fun_cls(Cls) ->
    map(fun (Cl) -> from_fun_cl(Cl) end, Cls).

from_fun_cl({clause,_,H,[],B}) -> [from_pat_list(H)|from_body(B)];
from_fun_cl({clause,_,H,[G],B}) ->
    [from_pat_list(H),['when'|from_body(G)]|from_body(B)].

from_bitsegs([{bin_element,_,Seg,Size,Type}|Ss]) ->
    [from_bitseg(Seg, Size, Type)|from_bitsegs(Ss)];
from_bitsegs([]) -> [].

from_bitseg_size(default) -> [];
from_bitseg_size(Size) -> [[size,from_expr(Size)]].

from_bitseg_type(_) -> [].

from_bitseg({integer,_,I}, default, default) -> I;
from_bitseg({integer,_,I}, Size, Type) ->
    [I|from_bitseg_type(Type) ++ from_bitseg_size(Size)];
from_bitseg({float,_,F}, Size, Type) ->
    [F|from_bitseg_type(Type) ++ from_bitseg_size(Size)];
from_bitseg({string,_,Cs}, default, default) -> Cs;
from_bitseg(E, Size, Type) ->
    [from_expr(E)|from_bitseg_type(Type) ++ from_bitseg_size(Size)].

from_pat({var,_,V}) -> V;
from_pat({nil,_}) -> [];
from_pat({integer,_,I}) -> I;
from_pat({float,_,F}) -> F;
from_pat({atom,_,A}) -> ?Q(A);			%Must quote here
from_pat({cons,_,H,T}) ->
    [cons,from_pat(H),from_pat(T)];
from_pat({tuple,_,Es}) ->
    [tuple|from_pat_list(Es)].

from_pat_list(Ps) -> [ from_pat(P) || P <- Ps ].

%% to_vanilla(Sexp, LineNumber) -> AST.

to_vanilla(S, L) ->
    %%lfe_io:format("~p\n", [S]),
    to_expr(S, L).

%% to_expr(Expr, LineNumber) -> Expr.

to_expr([], L) -> {nil,L};
to_expr(I, L) when is_integer(I) -> {integer,L,I};
to_expr(F, L) when is_float(F) -> {float,L,F};
to_expr(V, L) when is_atom(V) -> {var,L,V};	%Unquoted atom
to_expr(T, L) when is_tuple(T) ->
    {tuple,L,to_expr_list(tuple_to_list(T), L)};
to_expr(?Q(V), L) -> to_lit(V, L);
to_expr([cons,H,T], L) ->
    {cons,L,to_expr(H, L),to_expr(T, L)};
to_expr([car,E], L) -> {call,L,{atom,L,hd},[to_expr(E, L)]};
to_expr([cdr,E], L) -> {call,L,{atom,L,tl},[to_expr(E, L)]};
to_expr([list|Es], L) ->
    foldr(fun (E, T) -> {cons,L,to_expr(E, L),T} end, {nil,L}, Es);
to_expr(['list*'|Es], L) ->			%Macro
    to_list_s(Es, L, fun to_expr/2);
to_expr([tuple|Es], L) ->
    {tuple,L,to_expr_list(Es, L)};
to_expr([binary|Segs], L) ->
    {bin,L,to_bitsegs(Segs, L)};
%% Core closure special forms.
to_expr([lambda,As|B], L) ->
    {'fun',L,{clauses,[to_fun_cl([As|B], L)]}};
to_expr(['match-lambda'|Cls], L) ->
    {'fun',L,{clauses,to_fun_cls(Cls, L)}};
to_expr(['fun',F,A], L) -> {'fun',L,{function,F,A}};
to_expr(['fun',M,F,A], L) -> {'fun',L,{function,M,F,A}};
to_expr(['let',Lbs0|B0], L) ->
    Lbs1 = to_let_bindings(Lbs0, L),
    B1 = to_body(B0, L),
    {block,L,Lbs1 ++ B1};
%% Core control special forms.
to_expr([progn|B], L) -> to_block(B, L);
to_expr(['if',Test,True], L) ->
    to_expr(['if',Test,True,?Q(false)], L);
to_expr(['if',Test,True,False], L) ->
    {'case',L,to_expr(Test, L),
     to_icrt_cls([[?Q(true),True],[?Q(false),False]], L)};
to_expr(['case',E|Cls], L) ->
    {'case',L,to_expr(E, L),to_icrt_cls(Cls, L)};
to_expr(['receive'|Cls0], L) ->
    %% Get the right receive form depending on whether there is an after.
    {Cls1,A} = splitwith(fun (['after'|_]) -> false; (_) -> true end, Cls0),
    case A of
	[['after',T|B]] ->
	    {'receive',L,to_icrt_cls(Cls1, L),to_expr(T, L),to_body(B, L)};
	[] ->
	    {'receive',L,to_icrt_cls(Cls1, L)}
    end;
%% Special known macros.
to_expr([lc,Qs0|Es], L) ->
    Qs1 = to_lc_quals(Qs0, L),
    {lc,L,to_block(Es, L),Qs1};
%% General function calls.
to_expr([call,?Q(erlang),?Q(F)|As], L) ->
    %% This is semantically the same but some tools behave differently
    %% (qlc_pt).
    case is_erl_op(F, length(As)) of
	true -> list_to_tuple([op,L,F|to_expr_list(As, L)]);
	false ->
	    {call,L,{remote,{atom,L,erlang},{atom,L,F}},to_expr_list(As, L)}
    end;
to_expr([call,M,F|As], L) ->
    {call,L,{remote,L,to_expr(M, L),to_expr(F, L)},to_expr_list(As, L)};
to_expr([F|As], L) when is_atom(F) ->		%General function call
    case is_erl_op(F, length(As)) of
	true -> list_to_tuple([op,L,F|to_expr_list(As, L)]);
	false -> {call,L,{atom,L,F},to_expr_list(As, L)}
    end;
to_expr(List, L) ->
    case is_integer_list(List) of
	true -> {string,L,List};
	false ->
	    io:format("BOOM:~p\n", [List]),
	    {integer,L,4711}			%Not right!
    end.

%% is_erl_op(Op, Arity) -> bool().
%% Is Op/Arity one of the known Erlang operators?

is_erl_op(Op, Ar) ->
    erl_internal:arith_op(Op, Ar)
	orelse erl_internal:bool_op(Op, Ar)
	orelse erl_internal:comp_op(Op, Ar)
	orelse erl_internal:list_op(Op, Ar)
	orelse erl_internal:send_op(Op, Ar).

%% to_lc_quals(Qualifiers, Line) -> Qualifiers

to_lc_quals([['<-',P,E]|Qs], L) ->
    [{generate,L,to_pat(P, L),to_expr(E, L)}|to_lc_quals(Qs, L)];
to_lc_quals([['<-',P,['when'|G],E]|Qs], L) ->
    to_lc_quals([['<-',P,E]|G ++ Qs], L);	%Move guards to tests
to_lc_quals([T|Qs], L) ->
    [to_expr(T, L)|to_lc_quals(Qs, L)];
to_lc_quals([], _) -> [].

to_body(Es, L) -> [ to_expr(E, L) || E <- Es ].

to_expr_list(Es, L) -> [ to_expr(E, L) || E <- Es ].

to_list_s([E], L, F) -> F(E, L);
to_list_s([E|Es], L, F) ->
    {cons,L,F(E, L),to_list_s(Es, L, F)};
to_list_s([], L, _) -> {nil,L}.

%% to_block(Expressions, LineNumber) -> Block.
%% Don't generate {block,...} if only one expression, though
%% semantically the same some tools can't handle it (qlc_pt).

to_block(Es0, L) ->
    case to_expr_list(Es0, L) of
	[E] -> E;				%No need to wrap
	Es1 -> {block,L,Es1}			%Must wrap
    end.

%% to_let_bindings(Bindings, LineNumber) -> Block.

to_let_bindings(Lbs, L) ->
    map(fun ([P,V]) -> {match,L,to_pat(P, L),to_expr(V, L)} end, Lbs).

%% to_icrt_cls(Clauses, LineNumber) -> Clauses.
%% to_icrt_cl(Clause, LineNumber) -> Clause.
%%  If/case/receive/try clauses.

to_icrt_cls(Cls, L) ->
    lists:map(fun (Cl) -> to_icrt_cl(Cl, L) end, Cls).

to_icrt_cl([P,['when'|G]|B], L) ->
    {clause,L,[to_pat(P, L)],to_body(G, L),to_body(B, L)};
to_icrt_cl([P|B], L) ->
    {clause,L,[to_pat(P, L)],[],to_body(B, L)}.

%% to_fun_cls(Clauses, LineNumber) -> Clauses.
%% to_fun_cl(Clause, LineNumber) -> Clause.
%%  Function clauses.

to_fun_cls(Cls, L) ->
    lists:map(fun (Cl) -> to_fun_cl(Cl, L) end, Cls).

to_fun_cl([As,['when'|G]|B], L) ->
    {clause,L,to_pat_list(As, L),to_body(G, L),to_body(B, L)};
to_fun_cl([As|B], L) ->
    {clause,L,to_pat_list(As, L),[],to_body(B, L)}.

to_bitsegs(Ss, L) -> map(fun (S) -> to_bitseg(S, L) end, Ss).

to_bitseg([Val|Specs]=F, L) ->
    case is_integer_list(F) of
	true -> {bitelement,L,{string,L,F},default,default};
	false ->
	    {bitelement,L,to_expr(Val, L),default,default}
    end;
to_bitseg(Val, L) ->
    {bitelement,L,to_expr(Val, L),default,default}.

%% to_pat(Pattern, LineNumber) -> Pattern.

to_pat([], L) -> {nil,L};
to_pat(I, L) when is_integer(I) -> {integer,L,I};
to_pat(F, L) when is_float(F) -> {float,L,F};
to_pat(V, L) when is_atom(V) -> {var,L,V};	%Unquoted atom
to_pat(?Q(P), L) -> to_lit(P, L);		%Everything quoted here
to_pat([cons,H,T], L) ->
    {cons,L,to_pat(H, L),to_pat(T, L)};
to_pat([list|Es], L) ->
    foldr(fun (E, T) -> {cons,L,to_pat(E, L),T} end, {nil,L}, Es);
to_pat(['list*'|Es], L) ->
    to_list_s(Es, L, fun to_pat/2);
to_pat([tuple|Es], L) ->
    {tuple,L,to_pat_list(Es, L)};
to_pat(T, L) when is_tuple(T) ->
    {tuple,L,to_lit_list(tuple_to_list(T), L)}.

to_pat_list(Ps, L) -> [ to_pat(P, L) || P <- Ps ].

to_lit([], L) -> {nil,L};
to_lit(I, L) when is_integer(I) -> {integer,L,I};
to_lit(F, L) when is_float(F) -> {float,L,F};
to_lit(V, L) when is_atom(V) -> {atom,L,V};	%Quoted atom here!
to_lit([H|T], L) ->
    {cons,L,to_lit(H, L),to_lit(T, L)};
to_lit(T, L) when is_tuple(T) ->
    {tuple,L,to_lit_list(tuple_to_list(T), L)}.

to_lit_list(Ps, L) -> [ to_lit(P, L) || P <- Ps ].

is_integer_list([I|Is]) when is_integer(I) ->
    is_integer_list(Is);
is_integer_list([]) -> true;
is_integer_list(_) -> false.
