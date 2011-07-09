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

-import(lists, [map/2,foldl/3,foldr/3,mapfoldl/3,mapfoldr/3,splitwith/2]).

-import(orddict, [new/0,find/2,store/3]).

-define(Q(E), [quote,E]).			%We do a lot of quoting

%% from_vanilla(AST) -> Sexpr.

from_vanilla(AST) ->
    %% Be very LOUD for the time being.
    case catch {ok,from_expr(AST)} of
	{ok,S} -> S;
	Other ->
	    io:format("BOOM: ~p\n", [Other]),
	    error(Other)
    end.

from_expr({var,_,V}) -> V;
from_expr({nil,_}) -> [];
from_expr({integer,_,I}) -> I;
from_expr({float,_,F}) -> F;
from_expr({atom,_,A}) -> ?Q(A);			%Must quote here
from_expr({string,_,S}) -> ?Q(S);		%Must quote here
from_expr({cons,_,H0,T0}) ->
    H = from_expr(H0),
    case from_expr(T0) of			%Make it more elegant
	[list|T] -> [list,H|T];
	[] -> [list,H];
	T -> [cons,H,T]
    end;
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
%% More complex special forms. These become LFE macros.
from_expr({lc,_,E0,Qs0}) ->
    Qs1 = from_lc_quals(Qs0),
    E1 = from_expr(E0),
    [lc,Qs1,E1];
from_expr({record,_,R,Fs}) ->			%Create a record
    MR = list_to_atom("make-" ++ atom_to_list(R)),
    [MR|from_rec_fields(Fs)];
from_expr({record,_,E,R,Fs}) ->			%Set fields in record
    SR = list_to_atom("set-" ++ atom_to_list(R)),
    [SR,from_expr(E)|from_rec_fields(Fs)];
from_expr({record_field,_,E,R,{atom,_,F}}) ->	%We KNOW!
    RF = list_to_atom(atom_to_list(R) ++ "-" ++ atom_to_list(F)),
    [RF,from_expr(E)];
%% Function calls.
from_expr({op,_,Op,A}) -> [Op,from_expr(A)];
from_expr({op,_,Op,L,R}) -> [Op,from_expr(L),from_expr(R)];
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

%% from_lc_quals(Qualifiers) -> Qualifiers.

from_lc_quals([{generate,_,P,E}|Qs]) ->
    [['<-',from_pat(P),from_expr(E)]|from_lc_quals(Qs)];
from_lc_quals([T|Qs]) ->
    [from_expr(T)|from_lc_quals(Qs)];
from_lc_quals([]) -> [].

%% from_rec_fields(Recfields) -> Recfields.

from_rec_fields([{record_field,_,{atom,_,F},E}|Fs]) ->
    [F,from_expr(E)|from_rec_fields(Fs)];
from_rec_fields([{record_field,_,{var,_,F},E}|Fs]) -> %Hmm, special case
    [F,from_expr(E)|from_rec_fields(Fs)];
from_rec_fields([]) -> [].

%% from_bitsegs(Segs) -> Segs.

from_bitsegs([{bin_element,_,Seg,Size,Type}|Ss]) ->
    [from_bitseg(Seg, Size, Type)|from_bitsegs(Ss)];
from_bitsegs([]) -> [].

from_bitseg_size(default) -> [];
from_bitseg_size(Size) -> [[size,from_expr(Size)]].

from_bitseg_type(default) -> [];
from_bitseg_type(Ts) ->
    map(fun ({unit,U}) -> [unit,U]; (T) -> T end, Ts).

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
    [tuple|from_pat_list(Es)];
from_pat({record,_,R,Fs}) ->			%Match a record
    MR = list_to_atom("match-" ++ atom_to_list(R)),
    [MR|from_rec_fields(Fs)];
from_pat({match,_,P1,P2}) ->			%Aliases
    ['=',from_pat(P1),from_pat(P2)].

from_pat_list(Ps) -> [ from_pat(P) || P <- Ps ].

%% to_vanilla(Sexp, LineNumber) -> AST.
%%  Convert an LFE sexpr to an vanilla AST. The only difficulty is the
%%  handling of variables in (let ...). We assume that all macros have
%%  been expanded EXCEPT lc which we need for QLC's.

to_vanilla(S, L) ->
    %% Be very LOUD for the time being.
    case catch {ok,to_expr(S, L, new())} of
	{ok,AST} -> AST;
	Other ->
	    io:format("BOOM: ~p\n", [Other]),
	    error(Other)
    end.

%% to_expr(Expr, LineNumber, Vars) -> Expr.

to_expr([], L, _) -> {nil,L};
to_expr(I, L, _) when is_integer(I) -> {integer,L,I};
to_expr(F, L, _) when is_float(F) -> {float,L,F};
to_expr(V, L, Vs) when is_atom(V) ->		%Unquoted atom
    {var,L,var_name(V, Vs)};
to_expr(T, L, Vs) when is_tuple(T) ->
    {tuple,L,to_expr_list(tuple_to_list(T), L, Vs)};
to_expr(?Q(V), L, _) -> to_lit(V, L);
to_expr([cons,H,T], L, Vs) ->
    {cons,L,to_expr(H, L, Vs),to_expr(T, L, Vs)};
to_expr([car,E], L, Vs) -> {call,L,{atom,L,hd},[to_expr(E, L, Vs)]};
to_expr([cdr,E], L, Vs) -> {call,L,{atom,L,tl},[to_expr(E, L, Vs)]};
to_expr([list|Es], L, Vs) ->
    foldr(fun (E, T) -> {cons,L,to_expr(E, L, Vs),T} end, {nil,L}, Es);
to_expr([tuple|Es], L, Vs) ->
    {tuple,L,to_expr_list(Es, L, Vs)};
to_expr([binary|Segs], L, Vs) ->
    {bin,L,to_bitsegs(Segs, L, Vs)};
%% Core closure special forms.
to_expr([lambda,As|B], L, Vs) ->
    {'fun',L,{clauses,[to_fun_cl([As|B], L, Vs)]}};
to_expr(['match-lambda'|Cls], L, Vs) ->
    {'fun',L,{clauses,to_fun_cls(Cls, L, Vs)}};
to_expr(['fun',F,A], L, _) -> {'fun',L,{function,F,A}};
to_expr(['fun',M,F,A], L, _) -> {'fun',L,{function,M,F,A}};
to_expr(['let',Lbs0|B0], L, Vs0) ->
    {Lbs1,Vs1} = to_let_bindings(Lbs0, L, Vs0),
    B1 = to_body(B0, L, Vs1),
    {block,L,Lbs1 ++ B1};
%% Core control special forms.
to_expr([progn|B], L, Vs) -> to_block(B, L, Vs);
to_expr(['if',Test,True], L, Vs) ->
    to_expr(['if',Test,True,?Q(false)], L, Vs);
to_expr(['if',Test,True,False], L, Vs) ->
    {'case',L,to_expr(Test, L, Vs),
     to_icrt_cls([[?Q(true),True],[?Q(false),False]], L, Vs)};
to_expr(['case',E|Cls], L, Vs) ->
    {'case',L,to_expr(E, L, Vs),to_icrt_cls(Cls, L, Vs)};
to_expr(['receive'|Cls0], L, Vs) ->
    %% Get the right receive form depending on whether there is an after.
    {Cls1,A} = splitwith(fun (['after'|_]) -> false; (_) -> true end, Cls0),
    case A of
	[['after',T|B]] ->
	    {'receive',L,to_icrt_cls(Cls1, L, Vs),
	     to_expr(T, L, Vs),to_body(B, L, Vs)};
	[] ->
	    {'receive',L,to_icrt_cls(Cls1, L, Vs)}
    end;
%% Special known macros.
%% No record stuff here as they are macros which have been expanded.
%% Only lc for QLC.
to_expr([lc,Qs0|Es], L, Vs0) ->
    {Qs1,Vs1} = to_lc_quals(Qs0, L, Vs0),
    {lc,L,to_block(Es, L, Vs1),Qs1};
%% General function calls.
to_expr([call,?Q(erlang),?Q(F)|As], L, Vs) ->
    %% This is semantically the same but some tools behave differently
    %% (qlc_pt).
    case is_erl_op(F, length(As)) of
	true -> list_to_tuple([op,L,F|to_expr_list(As, L, Vs)]);
	false ->
	    {call,L,{remote,{atom,L,erlang},{atom,L,F}},
	     to_expr_list(As, L, Vs)}
    end;
to_expr([call,M,F|As], L, Vs) ->
    {call,L,{remote,L,to_expr(M, L, Vs),to_expr(F, L, Vs)},
     to_expr_list(As, L, Vs)};
to_expr([F|As], L, Vs) when is_atom(F) ->	%General function call
    case is_erl_op(F, length(As)) of
	true -> list_to_tuple([op,L,F|to_expr_list(As, L, Vs)]);
	false -> {call,L,{atom,L,F},to_expr_list(As, L, Vs)}
    end;
to_expr(List, L, _) ->
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

to_body(Es, L, Vs) -> [ to_expr(E, L, Vs) || E <- Es ].

to_expr_list(Es, L, Vs) -> [ to_expr(E, L, Vs) || E <- Es ].

to_list_s([E], L, Vs, F) -> F(E, L, Vs);
to_list_s([E|Es], L, Vs, F) ->
    {cons,L,F(E, L, Vs),to_list_s(Es, L, Vs, F)};
to_list_s([], L, _, _) -> {nil,L}.

%% to_block(Expressions, LineNumber, Vars) -> Block.
%% Don't generate {block,...} if only one expression, though
%% semantically the same some tools can't handle it (qlc_pt).

to_block(Es0, L, Vs) ->
    case to_expr_list(Es0, L, Vs) of
	[E] -> E;				%No need to wrap
	Es1 -> {block,L,Es1}			%Must wrap
    end.

%% to_let_bindings(Bindings, LineNumber, Vars) -> {Block,Vars}.

to_let_bindings(Lbs, L, Vs) ->
    F = fun ([P0,V0], Vs0) ->
		{P1,Vs1} = to_pat(P0, L, Vs0),
		%% The expression doesn't see new bindings.
		{{match,L,P1,to_expr(V0, L, Vs0)},Vs1}
	end,
    mapfoldl(F, Vs, Lbs).

%% to_icrt_cls(Clauses, LineNumber, Vars) -> Clauses.
%% to_icrt_cl(Clause, LineNumber, Vars) -> Clause.
%%  If/case/receive/try clauses.

to_icrt_cls(Cls, L, Vs) ->
    lists:map(fun (Cl) -> to_icrt_cl(Cl, L, Vs) end, Cls).

to_icrt_cl([P0,['when'|G]|B], L, Vs0) ->
    {P1,Vs1} = to_pat(P0, L, Vs0),
    {clause,L,[P1],to_body(G, L, Vs1),to_body(B, L, Vs1)};
to_icrt_cl([P0|B], L, Vs0) ->
    {P1,Vs1} = to_pat(P0, L, Vs0),
    {clause,L,[P1],[],to_body(B, L, Vs1)}.

%% to_fun_cls(Clauses, LineNumber, Vars) -> Clauses.
%% to_fun_cl(Clause, LineNumber, Vars) -> Clause.
%%  Function clauses.

to_fun_cls(Cls, L, Vs) ->
    lists:map(fun (Cl) -> to_fun_cl(Cl, L, Vs) end, Cls).

to_fun_cl([As0,['when'|G]|B], L, Vs0) ->
    {As1,Vs1} = to_pat_list(As0, L, Vs0),
    {clause,L,As1,to_body(G, L, Vs1),to_body(B, L, Vs1)};
to_fun_cl([As0|B], L, Vs0) ->
    {As1,Vs1} = to_pat_list(As0, L, Vs0),
    {clause,L,As1,[],to_body(B, L, Vs1)}.

%% to_lc_quals(Qualifiers, LineNumber, Vars) -> {Qualifiers,Vars}

to_lc_quals([['<-',P0,E]|Qs0], L, Vs0) ->
    {P1,Vs1} = to_pat(P0, L, Vs0),
    G = {generate,L,P1,to_expr(E, L, Vs0)},
    {Qs1,Vs2} = to_lc_quals(Qs0, L, Vs1),
    {[G|Qs1],Vs2};
to_lc_quals([['<-',P,['when'|G],E]|Qs], L, Vs) ->
    to_lc_quals([['<-',P,E]|G ++ Qs], L, Vs);	%Move guards to tests
to_lc_quals([T|Qs0], L, Vs0) ->
    {Qs1,Vs1} = to_lc_quals(Qs0, L, Vs0),
    {[to_expr(T, L, Vs0)|Qs1],Vs1};
to_lc_quals([], _, Vs) -> {[],Vs}.

%% to_bitsegs(Segs, LineNumber, Vars) -> Segs.
%% This gives a verbose value, but it is correct.

to_bitsegs(Ss, L, Vs) -> map(fun (S) -> to_bitseg(S, L, Vs) end, Ss).

to_bitseg([Val|Specs]=F, L, Vs) ->
    case is_integer_list(F) of
	true ->
	    {Size,Type} = to_bitspecs([]),
	    to_bin_element({string,L,F},to_expr(Size, L, Vs), Type, L);
	false ->
	    {Size,Type} = to_bitspecs(Specs),
	    to_bin_element(to_expr(Val, L, Vs),to_expr(Size, L, Vs), Type, L)
    end;
to_bitseg(Val, L, Vs) ->
    {Size,Type} = to_bitspecs([]),
    to_bin_element(to_expr(Val, L, Vs),to_expr(Size, L, Vs), Type, L).

to_bin_element(Val, Size, {Type,Unit,Sign,End}, L) ->
    {bin_element,L,Val,Size,[Type,{unit,Unit},Sign,End]}.

%% to_bitspec(Specs) -> {Size,Type}.
%%  Get the error handling as we want it.

to_bitspecs(Ss) ->
    case lfe_bits:get_bitspecs(Ss) of
	{ok,Sz,Ty} -> {Sz,Ty};
	{error,Error} -> erlang:error(Error)
    end.

%% to_pat(Pattern, LineNumber, Vars) -> {Pattern,Vars}.

to_pat([], L, Vs) -> {{nil,L},Vs};
to_pat(I, L, Vs) when is_integer(I) -> {{integer,L,I},Vs};
to_pat(F, L, Vs) when is_float(F) -> {{float,L,F},Vs};
to_pat(V0, L, Vs0) when is_atom(V0) ->		%Unquoted atom
    {V1,Vs1} = new_var_name(V0, Vs0),
    {{var,L,V1},Vs1};
to_pat(T, L, _) when is_tuple(T) ->		%Tuple literal
    {tuple,L,to_lit_list(tuple_to_list(T), L)};
to_pat(?Q(P), L, Vs) -> {to_lit(P, L),Vs};	%Everything quoted here
to_pat([cons,H0,T0], L, Vs0) ->
    {H1,Vs1} = to_pat(H0, L, Vs0),
    {T1,Vs2} = to_pat(T0, L, Vs1),
    {{cons,L,H1,T1},Vs2};
to_pat([list|Ps], L, Vs) ->
    F = fun (P0, {T,Vs0}) ->
		{P1,Vs1} = to_pat(P0, L, Vs0),
		{{cons,L,P1,T},Vs1}
	end,
    foldr(F, {{nil,L},Vs}, Ps);
to_pat([tuple|Ps0], L, Vs0) ->
    {Ps1,Vs1} = to_pat_list(Ps0, L, Vs0),
    {{tuple,L,Ps1},Vs1};
to_pat(['=',P1,P2], L, Vs0) ->			%Alias
    {P11,Vs1} = to_pat(P1, L, Vs0),
    {P21,Vs2} = to_pat(P2, L, Vs1),
    {{match,L,P11,P21},Vs2}.

to_pat_list(Ps, L, Vs) ->
    mapfoldl(fun (P, Vs0) -> to_pat(P, L, Vs0) end, Vs, Ps).

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

%% var_name(Var, Vars) -> Var.
%% new_var_name(Var, Vars) -> {Var,Vars}.
%%  New_var_name is used in patterns and always generates a new unused
%%  var name. This so we don't have to know all vars which we may be
%%  shadowing.

var_name(V, Vs) ->
    case find(V, Vs) of
	{ok,V1} -> V1;
	error -> V
    end.

new_var_name(Base, Vs) ->
    case find(Base, Vs) of
	{ok,V} ->
	    V1 = list_to_atom(lists:concat([V,'_'])),
	    {V1,store(Base, V1, Vs)};
	error ->
	    V1 = list_to_atom(lists:concat(['_',Base,'_'])),
	    {V1,store(Base, V1, Vs)}
    end.
