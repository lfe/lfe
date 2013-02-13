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

%% File    : lfe_trans.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang translator.

%%% Translate LFE code to/from vanilla Erlang AST.

-module(lfe_trans).

-export([from_vanilla/1,to_vanilla/2]).

-import(lists, [map/2,foldl/3,foldr/3,splitwith/2]).

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

%% So it won't get confused with strings.
from_bitseg({integer,_,I}, default, default) -> I;
from_bitseg({integer,_,I}, Size, Type) ->
    [I|from_bitseg_type(Type) ++ from_bitseg_size(Size)];
from_bitseg({float,_,F}, Size, Type) ->
    [F|from_bitseg_type(Type) ++ from_bitseg_size(Size)];
from_bitseg({string,_,Cs}, Size, Type) ->
    [Cs|from_bitseg_type(Type) ++ from_bitseg_size(Size)];
from_bitseg(E, Size, Type) ->
    [from_expr(E)|from_bitseg_type(Type) ++ from_bitseg_size(Size)].

from_bitseg_size(default) -> [];
from_bitseg_size(Size) -> [[size,from_expr(Size)]].

from_bitseg_type(default) -> [];
from_bitseg_type(Ts) ->
    map(fun ({unit,U}) -> [unit,U]; (T) -> T end, Ts).

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

to_vanilla(S, L) ->
    %% Be very LOUD for the time being.
    case catch {ok,to_expr(S, L)} of
	{ok,AST} -> AST;
	Other ->
	    io:format("BOOM: ~p\n", [Other]),
	    error(Other)
    end.

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
%% No record stuff here as they are macros which have been expanded.
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

%% to_lc_quals(Qualifiers, LineNumber) -> Qualifiers

to_lc_quals([['<-',P,E]|Qs], L) ->
    [{generate,L,to_pat(P, L),to_expr(E, L)}|to_lc_quals(Qs, L)];
to_lc_quals([['<-',P,['when'|G],E]|Qs], L) ->
    to_lc_quals([['<-',P,E]|G ++ Qs], L);	%Move guards to tests
to_lc_quals([T|Qs], L) ->
    [to_expr(T, L)|to_lc_quals(Qs, L)];
to_lc_quals([], _) -> [].

%% to_bitsegs(Segs, LineNumber) -> Segs.
%% This gives a verbose value, but it is correct.

to_bitsegs(Ss, L) -> map(fun (S) -> to_bitseg(S, L) end, Ss).

to_bitseg([Val|Specs]=F, L) ->
    case is_integer_list(F) of
	true ->
	    {Size,Type} = to_bitspecs([]),
	    to_bin_element({string,L,F},to_expr(Size, L), Type, L);
	false ->
	    {Size,Type} = to_bitspecs(Specs),
	    to_bin_element(to_expr(Val, L),to_expr(Size, L), Type, L)
    end;
to_bitseg(Val, L) ->
    {Size,Type} = to_bitspecs([]),
    to_bin_element(to_expr(Val, L),to_expr(Size, L), Type, L).

to_bin_element(Val, Size, {Type,Unit,Sign,End}, L) ->
    {bin_element,L,Val,Size,[Type,{unit,Unit},Sign,End]}.

%% to_bitspec(Specs) -> {Size,Type}.
%%  Get the error handling as we want it.

to_bitspecs(Ss) ->
    case lfe_bits:get_bitspecs(Ss) of
	{ok,Sz,Ty} -> {Sz,Ty};
	{error,Error} -> erlang:error(Error)
    end.

%% to_pat(Pattern, LineNumber) -> Pattern.

to_pat([], L) -> {nil,L};
to_pat(I, L) when is_integer(I) -> {integer,L,I};
to_pat(F, L) when is_float(F) -> {float,L,F};
to_pat(V, L) when is_atom(V) -> {var,L,V};	%Unquoted atom
to_pat(T, L) when is_tuple(T) ->		%Tuple literal
    {tuple,L,to_lit_list(tuple_to_list(T), L)};
to_pat(?Q(P), L) -> to_lit(P, L);		%Everything quoted here
to_pat([cons,H,T], L) ->
    {cons,L,to_pat(H, L),to_pat(T, L)};
to_pat([list|Es], L) ->
    foldr(fun (E, T) -> {cons,L,to_pat(E, L),T} end, {nil,L}, Es);
to_pat(['list*'|Es], L) ->
    to_list_s(Es, L, fun to_pat/2);
to_pat([tuple|Es], L) ->
    {tuple,L,to_pat_list(Es, L)};
to_pat(['=',P1,P2], L) ->			%Alias
    {match,L,to_pat(P1, L),to_pat(P2, L)}.

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
