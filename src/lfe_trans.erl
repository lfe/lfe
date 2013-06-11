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

-export([from_vanilla/1,from_vanilla/2,from_expr/1,from_expr/2,
	 from_body/1,from_body/2,from_lit/1,
	 to_vanilla/2,to_expr/2,to_lit/2]).

-import(lists, [map/2,foldl/3,mapfoldl/3,foldr/3,splitwith/2]).

-import(ordsets, [add_element/2,is_element/2,intersection/2]).

-define(Q(E), [quote,E]).			%We do a lot of quoting

-record(fstate, {vc=0				%Variable counter
		}).

%% from_vanilla(AST [, Variables]) -> Sexpr.

from_vanilla(AST) -> from_vanilla(AST, []).

from_vanilla(AST, Vs) ->
    %% Be very LOUD for the time being.
    St = #fstate{},
    case catch {ok,from_expr(AST, Vs, St)} of
	{ok,{S,_,_}} -> S;
	Other ->
	    io:format("BOOM: ~p\n", [Other]),
	    error(Other)
    end.

from_expr(E) -> from_expr(E, []).

from_expr(E, Vs) ->
    {S,_,_} = from_expr(E, Vs, #fstate{}),
    S.

%% from_expr(AST, VarTable, State) -> {Sexpr,VarTable,State}.

from_expr({var,_,V}, Vt, St) -> {V,Vt,St};		%Unquoted atom
from_expr({nil,_}, Vt, St) -> {[],Vt,St};
from_expr({integer,_,I}, Vt, St) -> {I,Vt,St};
from_expr({float,_,F}, Vt, St) -> {F,Vt,St};
from_expr({atom,_,A}, Vt, St) -> {?Q(A),Vt,St};		%Quoted atom
from_expr({string,_,S}, Vt, St) -> {?Q(S),Vt,St};	%Quoted string
from_expr({cons,_,H,T}, Vt0, St0) ->
    {Car,Vt1,St1} = from_expr(H, Vt0, St0),
    {Cdr,Vt2,St2} = from_expr(T, Vt1, St1),
    {from_cons(Car, Cdr),Vt2,St2};
%%    {[cons,Car,Cdr],Vt2,St2};
from_expr({tuple,_,Es}, Vt0, St0) ->
    {Ss,Vt1,St1} = from_expr_list(Es, Vt0, St0),
    {[tuple|Ss],Vt1,St1};
from_expr({bin,_,Segs}, Vt0, St0) ->
    {Ss,Vt1,St1} = from_bitsegs(Segs, Vt0, St0),
    {[binary|Ss],Vt1,St1};
%% Core closure special forms.
from_expr({'fun',_,{clauses,Cls}}, Vt0, St0) ->
    {Lcls,Vt1,St1} = from_fun_cls(Cls, Vt0, St0),
    {['match-lambda'|Lcls],Vt1,St1};		%Don't bother using lambda
from_expr({'fun',_,{function,F,A}}, Vt, St) ->
    {['fun',F,A],Vt,St};			%Return macros here?
from_expr({'fun',_,{function,M,F,A}}, Vt, St) ->
    {['fun',M,F,A],Vt,St};
%% Core control special forms.
from_expr({block,_,Es}, Vt0, St0) ->
    {Les,Vt1,St1} = from_body(Es, Vt0, St0),
    {[progn|Les],Vt1,St1};
from_expr({'case',_,E,Cls}, Vt0, St0) ->
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {Lcls,Vt2,St2} = from_icrt_cls(Cls, Vt1, St1),
    {['case',Le|Lcls],Vt2,St2};
from_expr({'if',_,Cls}, Vt0, St0) ->
    {Lcls,Vt1,St1} = from_icrt_cls(Cls, Vt0, St0),
    {['case',[]|Lcls],Vt1,St1};
from_expr({'receive',_,Cls}, Vt0, St0) ->
    {Lcls,Vt1,St1} = from_icrt_cls(Cls, Vt0, St0),
    {['receive'|Lcls],Vt1,St1};
from_expr({'receive',_,Cls,Timeout,Body}, Vt0, St0) ->
    {Lcls,Vt1,St1} = from_icrt_cls(Cls, Vt0, St0),
    {Lt,Vt2,St2} = from_expr(Timeout, Vt1, St1),
    {Lb,Vt3,St3} = from_body(Body, Vt2, St2),
    {['receive'|Lcls ++ [['after',Lt|Lb]]],Vt3,St3};
%% More complex special forms. These become LFE macros.
from_expr({lc,_,E,Qs}, Vt0, St0) ->
    {Lqs,Vt1,St1} = from_lc_quals(Qs, Vt0, St0),
    {Le,Vt2,St2} = from_expr(E, Vt1, St1),
    {[lc,Lqs,Le],Vt2,St2};
from_expr({record,_,R,Fs}, Vt0, St0) ->			%Create a record
    MR = list_to_atom("make-" ++ atom_to_list(R)),
    {Lfs,Vt1,St1} = from_rec_fields(Fs, Vt0, St0),
    {[MR|Lfs],Vt1,St1};
from_expr({record,_,E,R,Fs}, Vt0, St0) ->		%Set fields in record
    SR = list_to_atom("set-" ++ atom_to_list(R)),
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {Lfs,Vt2,St2} = from_rec_fields(Fs, Vt1, St1),
    {[SR,Le|Lfs],Vt2,St2};
from_expr({record_field,_,E,R,{atom,_,F}}, Vt0, St0) ->	%We KNOW!
    RF = list_to_atom(atom_to_list(R) ++ "-" ++ atom_to_list(F)),
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {[RF,Le],Vt1,St1};
%% Function calls.
from_expr({call,_,{remote,_,M,F},As}, Vt0, St0) ->	%Remote function call
    {Lm,Vt1,St1} = from_expr(M, Vt0, St0),
    {Lf,Vt2,St2} = from_expr(F, Vt1, St1),
    {Las,Vt3,St3} = from_expr_list(As, Vt2, St2),
    {[call,Lm,Lf|Las],Vt3,St3};
from_expr({call,_,{atom,_,F},As}, Vt0, St0) ->	%Local function call
    {Las,Vt1,St1} = from_expr_list(As, Vt0, St0),
    {[F|Las],Vt1,St1};
from_expr({call,_,F,As}, Vt0, St0) ->		%F not an atom or remote
    {Lf,Vt1,St1} = from_expr(F, Vt0, St0),
    {Las,Vt2,St2} = from_expr_list(As, Vt1, St1),
    {[funcall,Lf|Las],Vt2,St2};
from_expr({'try',_,Es,Scs,Ccs,As}, Vt, St0) ->
    %% Currently erl_lint does not allow any exports!
    {Les,_,St1} = from_body(Es, Vt, St0),
    %% These maybe empty.
    {Lscs,_,St2} = if Scs =:= [] -> {[],[],St1};
		      true -> from_icrt_cls(Scs, Vt, St1)
		   end,
    {Lccs,_,St3} = if Ccs =:= [] -> {[],[],St2};
		      true -> from_icrt_cls(Ccs, Vt, St2)
		   end,
    {Las,_,St4} = from_body(As, Vt, St3),
    {['try',[progn|Les]|
      from_maybe('case', Lscs) ++
	  from_maybe('catch', Lccs) ++
	  from_maybe('after', Las)],Vt,St4};
from_expr({'catch',_,E}, Vt0, St0) ->
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {['catch',Le],Vt1,St1};
from_expr({match,_,P,E}, Vt0, St0) ->
    {Lp,Eqt,Vt1,St1} = from_pat(P, Vt0, St0),	%The pattern
    {Le,Vt2,St2} = from_expr(E, Vt1, St1),	%The expression
    {Lb,Vt3,St3} = from_expr(P, Vt2, St2),	%Pattern as value expression
    Leg = from_eq_tests(Eqt),	%Implicit guard tests
    {['let',[[Lp,['when'|Leg],Le]],Lb],Vt3,St3};
from_expr({op,_,Op,A}, Vt0, St0) ->
    {La,Vt1,St1} = from_expr(A, Vt0, St0),
    {[Op,La],Vt1,St1};
from_expr({op,_,Op,L,R}, Vt0, St0) ->
    {Ll,Vt1,St1} = from_expr(L, Vt0, St0),
    {Lr,Vt2,St2} = from_expr(R, Vt1, St1),
    {[Op,Ll,Lr],Vt2,St2}.

from_maybe(_, []) -> [];
from_maybe(Tag, Es) -> [[Tag|Es]].

from_cons(Car, [list|Es]) -> [list,Car|Es];
from_cons(Car, []) -> [list,Car];
from_cons(Car, Cdr) -> [cons,Car,Cdr].

%% from_body(Expressions, VarTable, State) -> {Body,VarTable,State}.
%%  Handle '=' specially here and translate into let containing rest
%%  of body.

from_body(Es) -> from_body(Es, []).

from_body(Es, Vs) ->
    {Les,_,_} = from_body(Es, Vs, #fstate{}),
    Les.

from_body([{match,_,_,_}=Match], Vt0,St0) ->	%Last match
    {Lm,Vt1,St1} = from_expr(Match, Vt0, St0),	%Must return pattern as value
    {[Lm],Vt1,St1};
from_body([{match,_,P,E}|Es], Vt0, St0) ->
    {Lp,Eqt,Vt1,St1} = from_pat(P, Vt0, St0),
    {Le,Vt2,St2} = from_expr(E, Vt1, St1),
    {Les,Vt3,St4} = from_body(Es, Vt2, St2),
    Leg = from_eq_tests(Eqt),			%Implicit guard tests
    {[['let',[[Lp,['when'|Leg],Le]]|Les]],Vt3,St4};
from_body([E|Es], Vt0, St0) ->
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {Les,Vt2,St2} = from_body(Es, Vt1, St1),
    {[Le|Les],Vt2,St2};
from_body([], Vt, St) -> {[],Vt,St}.

from_expr_list(Es, Vt, St) -> mapfoldl2(fun from_expr/3, Vt, St, Es).

%% from_icrt_cls(Clauses, VarTable, State) -> {Clauses,VarTable,State}.
%% from_fun_cls(Clauses, VarTable, State) -> {Clauses,VarTable,State}.
%% from_icrt_cl(Clause, VarTable, State) -> {Clause,VarTable,State}.
%%  If/case/receive/try clauses.
%%  No ; in guards, so no guard sequence only one list of guard tests.

from_icrt_cls(Cls, Vt, St) -> from_cls(fun from_icrt_cl/3, Vt, St, Cls).

from_icrt_cl({clause,_,[],[G],B}, Vt0, St0) ->	%If clause
    {Lg,Vt1,St1} = from_body(G, Vt0, St0),
    {Lb,Vt2,St2} = from_body(B, Vt1, St1),
    {['_',['when'|Lg]|Lb],Vt2,St2};
from_icrt_cl({clause,_,H,[],B}, Vt0, St0) ->
    {[Lh],Eqt,Vt1,St1} = from_pat_list(H, Vt0, St0), %List of one
    {Lb,Vt2,St2} = from_body(B, Vt1, St1),
    Leg = from_eq_tests(Eqt),
    {[Lh,['when'|Leg]|Lb],Vt2,St2};
from_icrt_cl({clause,_,H,[G],B}, Vt0, St0) ->
    {[Lh],Eqt,Vt1,St1} = from_pat_list(H, Vt0, St0),		%List of one
    {Lg,Vt2,St2} = from_body(G, Vt1, St1),
    {Lb,Vt3,St3} = from_body(B, Vt2, St2),
    Leg = from_eq_tests(Eqt),
    {[Lh,['when'|Leg ++ Lg]|Lb],Vt3,St3}.

%% from_fun_cls(Clauses, VarTable, State) -> {Clauses,VarTableState}.
%% from_fun_cl(Clause, VarTable, State) -> {Clause,VarTable,State}.
%%  Function clauses.

from_fun_cls(Cls, Vt, St) -> from_cls(fun from_fun_cl/3, Vt, St, Cls).

from_fun_cl({clause,_,H,[],B}, Vt0, St0) ->
    {Lh,Eqt,Vt1,St1} = from_pat_list(H, Vt0, St0),
    {Lb,Vt2,St2} = from_body(B, Vt1, St1),
    Leg = from_eq_tests(Eqt),
    {[Lh,['when'|Leg]|Lb],Vt2,St2};
from_fun_cl({clause,_,H,[G],B}, Vt0, St0) ->
    {Lh,Eqt,Vt1,St1} = from_pat_list(H, Vt0, St0),
    {Lg,Vt2,St2} = from_body(G, Vt1, St1),
    {Lb,Vt3,St3} = from_body(B, Vt2, St2),
    Leg = from_eq_tests(Eqt),
    {[Lh,['when'|Leg ++ Lg]|Lb],Vt3,St3}.

from_cls(Fun, Vt0, St0, [C]) ->
    {Lc,Vt1,St1} = Fun(C, Vt0, St0),
    {[Lc],Vt1,St1};
from_cls(Fun, Vt0, St0, [C|Cs]) ->
    {Lc,Vtc,St1} = Fun(C, Vt0, St0),
    {Lcs,Vtcs,St2} = from_cls(Fun, Vt0, St1, Cs),
    {[Lc|Lcs],intersection(Vtc, Vtcs),St2}.

from_eq_tests(Gs) -> [ ['=:=',V,V1] || {V,V1} <- Gs ].

%% from_lc_quals(Qualifiers, VarTable, State) -> {Qualifiers,VarTable,State}.

from_lc_quals([{generate,_,P,E}|Qs], Vt0, St0) ->
    {Lp,Eqt,Vt1,St1} = from_pat(P, Vt0, St0),
    {Le,Vt2,St2} = from_expr(E, Vt1, St1),
    {Lqs,Vt3,St3} = from_lc_quals(Qs, Vt2, St2),
    Leg = from_eq_tests(Eqt),
    {[['<-',Lp,['when'|Leg]|Le]|Lqs],Vt3,St3};
from_lc_quals([T|Qs], Vt0, St0) ->
    {Lt,Vt1,St1} = from_expr(T, Vt0, St0),
    {Lqs,Vt2,St2} = from_lc_quals(Qs, Vt1, St1),
    {[Lt|Lqs],Vt2,St2};
from_lc_quals([], Vt, St) -> {[],Vt,St}.

%% from_rec_fields(Recfields, VarTable, State) -> {Recfields,VarTable,State}.

from_rec_fields([{record_field,_,{atom,_,F},E}|Fs], Vt0, St0) ->
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {Lfs,Vt2,St2} = from_rec_fields(Fs, Vt1, St1),
    {[F,Le|Lfs],Vt2,St2};
from_rec_fields([{record_field,_,{var,_,F},E}|Fs], Vt0, St0) -> %special case!!
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {Lfs,Vt2,St2} = from_rec_fields(Fs, Vt1, St1),
    {[F,Le|Lfs],Vt2,St2};
from_rec_fields([], Vt, St) -> {[],Vt,St}.

%% from_bitsegs(Segs, VarTable, State) -> {Segs,VarTable,State}.

from_bitsegs([{bin_element,_,Seg,Size,Type}|Segs], Vt0, St0) ->
    {S,Vt1,St1} = from_bitseg(Seg, Size, Type, Vt0, St0),
    {Ss,Vt2,St2} = from_bitsegs(Segs, Vt1, St1),
    {[S|Ss],Vt2,St2};
from_bitsegs([], Vt, St) -> {[],Vt,St}.

%% So it won't get confused with strings.
from_bitseg({integer,_,I}, default, default, Vt, St) -> {I,Vt,St};
from_bitseg({integer,_,I}, Size, Type, Vt0, St0) ->
    {Lsize,Vt1,St1} = from_bitseg_size(Size, Vt0, St0),
    {[I|from_bitseg_type(Type) ++ Lsize],Vt1,St1};
from_bitseg({float,_,F}, Size, Type, Vt0, St0) ->
    {Lsize,Vt1,St1} = from_bitseg_size(Size, Vt0, St0),
    {[F|from_bitseg_type(Type) ++ Lsize],Vt1,St1};
from_bitseg({string,_,S}, Size, Type, Vt0, St0) ->
    {Lsize,Vt1,St1} = from_bitseg_size(Size, Vt0, St0),
    {[S|from_bitseg_type(Type) ++ Lsize],Vt1,St1};
from_bitseg(E, Size, Type, Vt0, St0) ->
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {Lsize,Vt2,St2} = from_bitseg_size(Size, Vt1, St1),
    {[Le|from_bitseg_type(Type) ++ Lsize],Vt2,St2}.

from_bitseg_size(default, Vt, St) -> {[],Vt,St};
from_bitseg_size(Size, Vt0, St0) ->
    {Ssize,Vt1,St1} = from_expr(Size, Vt0, St0),
    {[[size,Ssize]],Vt1,St1}.

from_bitseg_type(default) -> [];
from_bitseg_type(Ts) ->
    map(fun ({unit,U}) -> [unit,U]; (T) -> T end, Ts).

new_var(#fstate{vc=C}=St) ->
    V = list_to_atom(lists:concat(["---",C,"---"])),
    {V,St#fstate{vc=C+1}}.

from_pat({var,_,'_'}, Vt, St) -> {'_',[],Vt,St};	%Special case _
from_pat({var,_,V}, Vt, St0) ->			%Unquoted atom
    case is_element(V, Vt) of			%Is variable bound?
	true ->
	    {V1,St1} = new_var(St0),		%New var for pattern
	    {V1,[{V,V1}],Vt,St1};		%Add to guard tests
	false ->
	    {V,[],add_element(V, Vt),St0}
    end;
from_pat({nil,_}, Vt, St) -> {[],[],Vt,St};
from_pat({integer,_,I}, Vt, St) -> {I,[],Vt,St};
from_pat({float,_,F}, Vt, St) -> {F,[],Vt,St};
from_pat({atom,_,A}, Vt, St) -> {?Q(A),[],Vt,St};	%Quoted atom
from_pat({string,_,S}, Vt, St) -> {?Q(S),[],Vt,St};	%Quoted string
from_pat({cons,_,H,T}, Vt0, St0) ->
    {Car,Eqt1,Vt1,St1} = from_pat(H, Vt0, St0),
    {Cdr,Eqt2,Vt2,St2} = from_pat(T, Vt1, St1),
    {from_cons(Car, Cdr),Eqt1++Eqt2,Vt2,St2};
from_pat({tuple,_,Es}, Vt0, St0) ->
    {Ss,Eqt,Vt1,St1} = from_pat_list(Es, Vt0, St0),
    {[tuple|Ss],Eqt,Vt1,St1};
from_pat({bin,_,Segs}, Vt0, St0) ->
    {Ss,Eqt,Vt1,St1} = from_pat_bitsegs(Segs, Vt0, St0),
    {[binary|Ss],Eqt,Vt1,St1};
from_pat({record,_,R,Fs}, Vt0, St0) ->		%Match a record
    MR = list_to_atom("match-" ++ atom_to_list(R)),
    {Sfs,Eqt,Vt1,St1} = from_rec_fields(Fs, Vt0, St0),
    {[MR|Sfs],Eqt,Vt1,St1};
from_pat({match,_,P1,P2}, Vt0, St0) ->		%Aliases
    {Lp1,Eqt1,Vt1,St1} = from_pat(P1, Vt0, St0),
    {Lp2,Eqt2,Vt2,St2} = from_pat(P2, Vt1, St1),
    {['=',Lp1,Lp2],Eqt1++Eqt2,Vt2,St2}.

from_pat_list([P|Ps], Vt0, St0) ->
    {Lp,Eqt,Vt1,St1} = from_pat(P, Vt0, St0),
    {Lps,Eqts,Vt2,St2} = from_pat_list(Ps, Vt1, St1),
    {[Lp|Lps],Eqt++Eqts,Vt2,St2};
from_pat_list([], Vt, St) -> {[],[],Vt,St}.

%% from_pat_bitsegs(Segs, VarTable, State) -> {Segs,EqTable,VarTable,State}.

from_pat_bitsegs([{bin_element,_,Seg,Size,Type}|Segs], Vt0, St0) ->
    {S,Eqt,Vt1,St1} = from_pat_bitseg(Seg, Size, Type, Vt0, St0),
    {Ss,Eqts,Vt2,St2} = from_pat_bitsegs(Segs, Vt1, St1),
    {[S|Ss],Eqt++Eqts,Vt2,St2};
from_pat_bitsegs([], Vt, St) -> {[],[],Vt,St}.

from_pat_bitseg({string,_,S}, Size, Type, Vt0, St0) ->
    {Lsize,Vt1,St1} = from_pat_bitseg_size(Size, Vt0, St0),
    {[S|from_bitseg_type(Type) ++ Lsize],[],Vt1,St1};
from_pat_bitseg(P, Size, Type, Vt0, St0) ->
    {Lp,Eqt,Vt1,St1} = from_pat(P, Vt0, St0),
    {Lsize,Vt2,St2} = from_pat_bitseg_size(Size, Vt1, St1),
    {[Lp|from_bitseg_type(Type) ++ Lsize],Eqt,Vt2,St2}.

from_pat_bitseg_size(default, Vt, St) -> {[],Vt,St};
from_pat_bitseg_size({var,_,V}, Vt, St) ->	%Size vars never match
    {[[size,V]],Vt,St};
from_pat_bitseg_size(Size, Vt0, St0) ->
    {Ssize,_,Vt1,St1} = from_pat(Size, Vt0, St0),
    {[[size,Ssize]],Vt1,St1}.

from_lit({nil,_}) -> [];
from_lit({integer,_,I}) -> I;
from_lit({float,_,F}) -> F;
from_lit({atom,_,A}) -> A;			%Quoted atom
from_lit({string,_,S}) -> S;
from_lit({cons,_,H,T}) ->
    [cons,from_lit(H),from_lit(T)];
from_lit({tuple,_,Es}) ->
    [tuple|from_lit_list(Es)].

from_lit_list(Es) -> [ from_lit(E) || E <- Es ].

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

%% mapfold2(Fun, Acc1, Acc2, List) -> {List,Acc1,Acc2}.

mapfoldl2(Fun, A0, B0, [E0|Es0]) ->
    {E1,A1,B1} = Fun(E0, A0, B0),
    {Es1,A2,B2} = mapfoldl2(Fun, A1, B1, Es0),
    {[E1|Es1],A2,B2};
mapfoldl2(_, A, B, []) -> {[],A,B}.
