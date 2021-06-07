%% Copyright (c) 2008-2021 Robert Virding
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
%%%
%%% Note that we don't really check code here as such, we assume the
%%% input is correct. If there is an error in the input we just fail.
%%% This allows us to accept forms which are actually illegal but we
%%% may special case, for example functions call in patterns which
%%% will become macro expansions.
%%%
%%% Having import from and rename forces us to explicitly convert the
%%% call as we can't use an import attribute to do this properly for
%%% us. Hence we collect the imports in lfe_codegen and pass them onto
%%% us.
%%%
%%% Module aliases are collected in lfe_codegen and passed on to us.

-module(lfe_translate).

-export([from_expr/1,from_expr/2,from_body/1,from_body/2,from_lit/1]).
-export([to_expr/2,to_expr/3,to_exprs/2,to_exprs/3,to_lit/2]).

-import(lists, [map/2,foldl/3,mapfoldl/3,foldr/3,splitwith/2]).

-include("lfe.hrl").

-record(from, {vc=0                             %Variable counter
              }).

%% from_expr(AST) -> Sexpr.
%% from_expr(AST, Variables) -> {Sexpr,Variables}.
%% from_body([AST]) -> Sexpr.
%% from_body([AST], Variables) -> {Sexpr,Variables}.
%%  Translate a vanilla Erlang expression into LFE. The main
%%  difficulty is in the handling of variables. The implicit matching
%%  of known variables in vanilla must be translated into explicit
%%  equality tests in guards (which is what the compiler does
%%  internally). For this we need to keep track of visible variables
%%  and detect when they reused in patterns.

from_expr(E) ->
    {S,_,_} = from_expr(E, ordsets:new(), #from{}),
    S.

from_expr(E, Vs0) ->
    Vt0 = ordsets:from_list(Vs0),               %We are clean
    {S,Vt1,_} = from_expr(E, Vt0, #from{}),
    {S,ordsets:to_list(Vt1)}.

from_body(Es) ->
    {Les,_,_} = from_body(Es, ordsets:new(), #from{}),
    [progn|Les].

from_body(Es, Vs0) ->
    Vt0 = ordsets:from_list(Vs0),               %We are clean
    {Les,Vt1,_} = from_body(Es, Vt0, #from{}),
    {[progn|Les],ordsets:to_list(Vt1)}.

%% from_expr(AST, VarTable, State) -> {Sexpr,VarTable,State}.

from_expr({var,_,V}, Vt, St) -> {V,Vt,St};      %Unquoted atom
from_expr({nil,_}, Vt, St) -> {[],Vt,St};
from_expr({integer,_,I}, Vt, St) -> {I,Vt,St};
from_expr({float,_,F}, Vt, St) -> {F,Vt,St};
from_expr({atom,_,A}, Vt, St) -> {?Q(A),Vt,St}; %Quoted atom
from_expr({string,_,S}, Vt, St) -> {?Q(S),Vt,St}; %Quoted string
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
from_expr({map,_,Assocs}, Vt0, St0) ->          %Build a map
    {Ps,Vt1,St1} = from_map_assocs(Assocs, Vt0, St0),
    {[map|Ps],Vt1,St1};
from_expr({map,_,Map,Assocs}, Vt0, St0) ->      %Update a map
    {Lm,Vt1,St1} = from_expr(Map, Vt0, St0),
    from_map_update(Assocs, nul, Lm, Vt1, St1);
%% Record special forms.
from_expr({record,_,Name,Fs}, Vt0, St0) ->
    {Lfs,Vt1,St1} = from_rec_fields(Fs, Vt0, St0),
    {['make-record',Name|Lfs],Vt1,St1};
from_expr({record_index,_,Name,{atom,_,F}}, Vt, St) -> %We KNOW!
    {['record-index',Name,F],Vt,St};
from_expr({record_field,_,E,Name,{atom,_,F}}, Vt0, St0) -> %We KNOW!
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {['record-field',Le,Name,F],Vt1,St1};
from_expr({record,_,E,Name,Fs}, Vt0, St0) ->
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {Lfs,Vt2,St2} = from_rec_fields(Fs, Vt1, St1),
    {['record-update',Le,Name|Lfs],Vt2,St2};
from_expr({record_field,_,_,_}=M, Vt, St) ->    %Pre R16 packages
    from_package_module(M, Vt, St);
%% Function special forms.
from_expr({'fun',_,{clauses,Cls}}, Vt, St0) ->
    {Lcls,St1} = from_fun_cls(Cls, Vt, St0),
    {['match-lambda'|Lcls],Vt,St1};             %Don't bother using lambda
from_expr({'fun',_,{function,F,A}}, Vt, St) ->
    %% These are just literal values.
    {[function,F,A],Vt,St};
from_expr({'fun',_,{function,M,F,A}}, Vt, St) ->
    %% These are abstract values.
    {[function,from_lit(M),from_lit(F),from_lit(A)],Vt,St};
%% Core control special forms.
from_expr({match,_,_,_}=Match, Vt, St) ->
    from_match(Match, Vt, St);
from_expr({block,_,Es}, Vt, St) ->
    from_block(Es, Vt, St);
from_expr({'if',_,Cls}, Vt0, St0) ->            %This is the Erlang if
    {Lcls,Vt1,St1} = from_icrt_cls(Cls, Vt0, St0),
    {['case',[]|Lcls],Vt1,St1};
from_expr({'case',_,E,Cls}, Vt0, St0) ->
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {Lcls,Vt2,St2} = from_icrt_cls(Cls, Vt1, St1),
    {['case',Le|Lcls],Vt2,St2};
from_expr({'receive',_,Cls}, Vt0, St0) ->
    {Lcls,Vt1,St1} = from_icrt_cls(Cls, Vt0, St0),
    {['receive'|Lcls],Vt1,St1};
from_expr({'receive',_,Cls,Timeout,Body}, Vt0, St0) ->
    {Lcls,Vt1,St1} = from_icrt_cls(Cls, Vt0, St0),
    {Lt,Vt2,St2} = from_expr(Timeout, Vt1, St1),
    {Lb,Vt3,St3} = from_body(Body, Vt2, St2),
    {['receive'|Lcls ++ [['after',Lt|Lb]]],Vt3,St3};
from_expr({'catch',_,E}, Vt0, St0) ->
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {['catch',Le],Vt1,St1};
from_expr({'try',_,Es,Scs,Ccs,As}, Vt, St) ->
    from_try(Es, Scs, Ccs, As, Vt, St);
%% More complex special forms. These become LFE macros.
from_expr({lc,_,E,Qs}, Vt0, St0) ->
    {Lqs,Vt1,St1} = from_lc_quals(Qs, Vt0, St0),
    {Le,Vt2,St2} = from_expr(E, Vt1, St1),
    {[lc,Lqs,Le],Vt2,St2};
%% Function calls.
from_expr({call,_,{remote,_,M,F},As}, Vt0, St0) -> %Remote function call
    {Lm,Vt1,St1} = from_expr(M, Vt0, St0),
    {Lf,Vt2,St2} = from_expr(F, Vt1, St1),
    {Las,Vt3,St3} = from_expr_list(As, Vt2, St2),
    {[call,Lm,Lf|Las],Vt3,St3};
from_expr({call,_,{atom,_,F},As}, Vt0, St0) ->  %Local function call
    {Las,Vt1,St1} = from_expr_list(As, Vt0, St0),
    {[F|Las],Vt1,St1};
from_expr({call,_,F,As}, Vt0, St0) ->           %F not an atom or remote
    {Lf,Vt1,St1} = from_expr(F, Vt0, St0),
    {Las,Vt2,St2} = from_expr_list(As, Vt1, St1),
    {[funcall,Lf|Las],Vt2,St2};
from_expr({op,_,Op,A}, Vt0, St0) ->
    {La,Vt1,St1} = from_expr(A, Vt0, St0),
    {[Op,La],Vt1,St1};
from_expr({op,_,Op,L,R}, Vt0, St0) ->
    {Ll,Vt1,St1} = from_expr(L, Vt0, St0),
    {Lr,Vt2,St2} = from_expr(R, Vt1, St1),
    {[Op,Ll,Lr],Vt2,St2}.

from_cons(Car, [list|Es]) -> [list,Car|Es];
from_cons(Car, []) -> [list,Car];
from_cons(Car, Cdr) -> [cons,Car,Cdr].

%% from_body(Expressions, VarTable, State) -> {Body,VarTable,State}.
%%  Handle '=' specially here and translate into let containing rest
%%  of body.

from_body([{match,_,_,_}=Match], Vt0,St0) ->    %Last match
    {Lm,Vt1,St1} = from_expr(Match, Vt0, St0),  %Must return pattern as value
    {[Lm],Vt1,St1};
from_body([{match,_,P,E}|Es], Vt0, St0) ->
    {Lp,Eqt,Vt1,St1} = from_pat(P, Vt0, St0),
    {Le,Vt2,St2} = from_expr(E, Vt1, St1),
    {Les,Vt3,St4} = from_body(Es, Vt2, St2),
    Leg = from_eq_tests(Eqt),                   %Implicit guard tests
    Lbody = from_add_guard(Leg, [Le]),
    {[['let',[[Lp|Lbody]]|Les]],Vt3,St4};
from_body([E|Es], Vt0, St0) ->
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {Les,Vt2,St2} = from_body(Es, Vt1, St1),
    {[Le|Les],Vt2,St2};
from_body([], Vt, St) -> {[],Vt,St}.

from_expr_list(Es, Vt, St) -> mapfoldl2(fun from_expr/3, Vt, St, Es).

%% from_block(Body, VarTable, State) -> {Block,State}.

from_block(Es, Vt0, St0) ->
    case from_body(Es, Vt0, St0) of
        {[Le],Vt1,St1} -> {Le,Vt1,St1};
        {Les,Vt1,St1} -> {[progn|Les],Vt1,St1}
    end.

%% from_add_guard(GuardTests, Body) -> Body.
%%  Only prefix with a guard when there are tests.

from_add_guard([], Body) -> Body;               %No guard tests
from_add_guard(Gts, Body) ->
    [['when'|Gts]|Body].

%% from_match(Match, VarTable, State) -> {LetForm,State}.
%%  Match returns the value of the expression. Use a let to do
%%  matching with an alias which we return for value.

from_match({match,L,P,E}, Vt0, St0) ->
    {Alias,St1} = new_from_var(St0),            %Alias variable value
    MP = {match,L,{var,L,Alias},P},
    {Lp,Eqt,Vt1,St2} = from_pat(MP, Vt0, St1),  %The alias pattern
    {Le,Vt2,St3} = from_expr(E, Vt1, St2),      %The expression
    Leg = from_eq_tests(Eqt),                   %Implicit guard tests
    Lbody = from_add_guard(Leg, [Le]),          %Now build the whole body
    {['let',[[Lp|Lbody]],Alias],Vt2,St3}.

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

%% from_map_assocs(MapAssocs, VarTable, State) -> {Pairs,VarTable,State}.

from_map_assocs([{_,_,Key,Val}|As], Vt0, St0) ->
    {Lk,Vt1,St1} = from_expr(Key, Vt0, St0),
    {Lv,Vt2,St2} = from_expr(Val, Vt1, St1),
    {Las,Vt3,St3} = from_map_assocs(As, Vt2, St2),
    {[Lk,Lv|Las],Vt3,St3};
from_map_assocs([], Vt, St) -> {[],Vt,St}.

%% from_map_update(MapAssocs, CurrAssoc, CurrMap, VarTable, State) ->
%%     {Map,VarTable,State}.
%%  We need to be a bit cunning here and do everything left-to-right
%%  and minimize nested calls.

from_map_update([{Assoc,_,Key,Val}|As], Curr, Map0, Vt0, St0) ->
    {Lk,Vt1,St1} = from_expr(Key, Vt0, St0),
    {Lv,Vt2,St2} = from_expr(Val, Vt1, St1),
    %% Check if can continue this mapping or need to start a new one.
    Map1 = if Assoc =:= Curr -> Map0 ++ [Lk,Lv];
              Assoc =:= map_field_assoc -> ['map-set',Map0,Lk,Lv];
              Assoc =:= map_field_exact -> ['map-update',Map0,Lk,Lv]
           end,
    from_map_update(As, Assoc, Map1, Vt2, St2);
%% from_map_update([{Assoc,_,Key,Val}|Fs], Assoc, Map0, Vt0, St0) ->
%%     {Lk,Vt1,St1} = from_expr(Key, Vt0, St0),
%%     {Lv,Vt2,St2} = from_expr(Val, Vt1, St1),
%%     from_map_update(Fs, Assoc, Map0 ++ [Lk,Lv], Vt2, St2);
%% from_map_update([{Assoc,_,Key,Val}|Fs], _, Map0, Vt0, St0) ->
%%     {Lk,Vt1,St1} = from_expr(Key, Vt0, St0),
%%     {Lv,Vt2,St2} = from_expr(Val, Vt1, St1),
%%     Op = if Assoc =:= map_field_assoc -> 'map-set';
%%             true -> 'map-update'
%%          end,
%%     from_map_update(Fs, Assoc, [Op,Map0,Lk,Lv], Vt2, St2);
from_map_update([], _, Map, Vt, St) -> {Map,Vt,St}.

%% from_rec_fields(Recfields, VarTable, State) -> {Recfields,VarTable,State}.

from_rec_fields([{record_field,_,{atom,_,F},V}|Fs], Vt0, St0) ->
    {Lv,Vt1,St1} = from_expr(V, Vt0, St0),
    {Lfs,Vt2,St2} = from_rec_fields(Fs, Vt1, St1),
    {[F,Lv|Lfs],Vt2,St2};
from_rec_fields([{record_field,_,{var,_,F},V}|Fs], Vt0, St0) ->
    %% Special case!!
    {Lv,Vt1,St1} = from_expr(V, Vt0, St0),
    {Lfs,Vt2,St2} = from_rec_fields(Fs, Vt1, St1),
    {[F,Lv|Lfs],Vt2,St2};
from_rec_fields([], Vt, St) -> {[],Vt,St}.

%% from_icrt_cls(Clauses, VarTable, State) -> {Clauses,VarTable,State}.
%% from_icrt_cl(Clause, VarTable, State) -> {Clause,VarTable,State}.
%%  If/case/receive/try clauses.
%%  No ; in guards, so no guard sequence only one list of guard tests.

from_icrt_cls(Cls, Vt, St) -> from_cls(fun from_icrt_cl/3, Vt, St, Cls).

from_icrt_cl({clause,_,[],[G],B}, Vt0, St0) ->          %If clause
    {Lg,Vt1,St1} = from_body(G, Vt0, St0),
    {Lb,Vt2,St2} = from_body(B, Vt1, St1),
    Lbody = from_add_guard(Lg, Lb),
    {['_'|Lbody],Vt2,St2};
from_icrt_cl({clause,_,H,[],B}, Vt0, St0) ->
    {[Lh],Eqt,Vt1,St1} = from_pats(H, Vt0, St0),        %List of one
    {Lb,Vt2,St2} = from_body(B, Vt1, St1),
    Leg = from_eq_tests(Eqt),
    Lbody = from_add_guard(Leg, Lb),
    {[Lh|Lbody],Vt2,St2};
from_icrt_cl({clause,_,H,[G],B}, Vt0, St0) ->
    {[Lh],Eqt,Vt1,St1} = from_pats(H, Vt0, St0),        %List of one
    {Lg,Vt2,St2} = from_body(G, Vt1, St1),
    {Lb,Vt3,St3} = from_body(B, Vt2, St2),
    Leg = from_eq_tests(Eqt),
    Lbody = from_add_guard(Leg ++ Lg, Lb),
    {[Lh|Lbody],Vt3,St3}.

%% from_fun_cls(Clauses, VarTable, State) -> {Clauses,State}.
%% from_fun_cl(Clause, VarTable, State) -> {Clause,VarTable,State}.
%%  Function clauses, all variables in the patterns are new variables
%%  which shadow existing variables without equality tests.

from_fun_cls(Cls, Vt, St0) ->
    {Lcls,_,St1} = from_cls(fun from_fun_cl/3, Vt, St0, Cls),
    {Lcls,St1}.

from_fun_cl({clause,_,H,[],B}, Vt0, St0) ->
    {Lh,Eqt,Vtp,St1} = from_pats(H, [], St0),
    Vt1 = ordsets:union(Vtp, Vt0),              %All variables so far
    {Lb,Vt2,St2} = from_body(B, Vt1, St1),
    Leg = from_eq_tests(Eqt),
    Lbody = from_add_guard(Leg, Lb),
    {[Lh|Lbody],Vt2,St2};
from_fun_cl({clause,_,H,[G],B}, Vt0, St0) ->
    {Lh,Eqt,Vtp,St1} = from_pats(H, [], St0),
    Vt1 = ordsets:union(Vtp, Vt0),              %All variables so far
    {Lg,Vt2,St2} = from_body(G, Vt1, St1),
    {Lb,Vt3,St3} = from_body(B, Vt2, St2),
    Leg = from_eq_tests(Eqt),
    Lbody = from_add_guard(Leg ++ Lg, Lb),
    {[Lh|Lbody],Vt3,St3}.

%% from_cls(ClauseFun, VarTable, State, Clauses) -> {Clauses,VarTable,State}.
%%  Translate the clauses but only export variables that are defined
%%  in all clauses, the intersection of the variables.

from_cls(Fun, Vt0, St0, [C]) ->
    {Lc,Vt1,St1} = Fun(C, Vt0, St0),
    {[Lc],Vt1,St1};
from_cls(Fun, Vt0, St0, [C|Cs]) ->
    {Lc,Vtc,St1} = Fun(C, Vt0, St0),
    {Lcs,Vtcs,St2} = from_cls(Fun, Vt0, St1, Cs),
    {[Lc|Lcs],ordsets:intersection(Vtc, Vtcs),St2}.

from_eq_tests(Gs) -> [ ['=:=',V,V1] || {V,V1} <- Gs ].

%% from_try(Exprs, CaseClauses, CatchClauses, After, VarTable, State) ->
%%     {Try,State}.
%%  Only return the parts which have contents.

from_try(Es, Scs, Ccs, As, Vt, St0) ->
    %% Try does not allow any exports!
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
          from_maybe('after', Las)],Vt,St4}.

from_maybe(_, []) -> [];
from_maybe(Tag, Es) -> [[Tag|Es]].

%% from_lc_quals(Qualifiers, VarTable, State) -> {Qualifiers,VarTable,State}.

from_lc_quals([{generate,_,P,E}|Qs], Vt0, St0) ->
    {Lp,Eqt,Vt1,St1} = from_pat(P, Vt0, St0),
    {Le,Vt2,St2} = from_expr(E, Vt1, St1),
    {Lqs,Vt3,St3} = from_lc_quals(Qs, Vt2, St2),
    Leg = from_eq_tests(Eqt),
    Lbody = from_add_guard(Leg, Le),
    {[['<-',Lp|Lbody]|Lqs],Vt3,St3};
from_lc_quals([T|Qs], Vt0, St0) ->
    {Lt,Vt1,St1} = from_expr(T, Vt0, St0),
    {Lqs,Vt2,St2} = from_lc_quals(Qs, Vt1, St1),
    {[Lt|Lqs],Vt2,St2};
from_lc_quals([], Vt, St) -> {[],Vt,St}.

%% from_package_module(Module, VarTable, State) -> {Module,VarTable,State}.
%%  We must handle the special case where in pre-R16 you could have
%%  packages with a dotted module path. It used a special record_field
%%  tuple. This does not work in R16 and later!

from_package_module({record_field,_,_,_}=M, Vt, St) ->
    Segs = erl_parse:package_segments(M),
    A = list_to_atom(packages:concat(Segs)),
    {?Q(A),Vt,St}.

%% new_from_var(State) -> {VarName,State}.

new_from_var(#from{vc=C}=St) ->
    V = list_to_atom(lists:concat(['-var-',C,'-'])),
    {V,St#from{vc=C+1}}.

%% from_pat(Pattern, VarTable, State) ->
%%     {Pattern,EqualVar,VarTable,State}.

from_pat({var,_,'_'}, Vt, St) -> {'_',[],Vt,St};    %Special case _
from_pat({var,_,V}, Vt, St0) ->                     %Unquoted atom
    case ordsets:is_element(V, Vt) of               %Is variable bound?
        true ->
            {V1,St1} = new_from_var(St0),       %New var for pattern
            {V1,[{V,V1}],Vt,St1};               %Add to guard tests
        false ->
            {V,[],ordsets:add_element(V, Vt),St0}
    end;
from_pat({nil,_}, Vt, St) -> {[],[],Vt,St};
from_pat({integer,_,I}, Vt, St) -> {I,[],Vt,St};
from_pat({float,_,F}, Vt, St) -> {F,[],Vt,St};
from_pat({atom,_,A}, Vt, St) -> {?Q(A),[],Vt,St};   %Quoted atom
from_pat({string,_,S}, Vt, St) -> {?Q(S),[],Vt,St}; %Quoted string
from_pat({cons,_,H,T}, Vt0, St0) ->
    {Car,Eqt1,Vt1,St1} = from_pat(H, Vt0, St0),
    {Cdr,Eqt2,Vt2,St2} = from_pat(T, Vt1, St1),
    {from_cons(Car, Cdr),Eqt1++Eqt2,Vt2,St2};
from_pat({tuple,_,Es}, Vt0, St0) ->
    {Ss,Eqt,Vt1,St1} = from_pats(Es, Vt0, St0),
    {[tuple|Ss],Eqt,Vt1,St1};
from_pat({bin,_,Segs}, Vt0, St0) ->
    {Ss,Eqt,Vt1,St1} = from_pat_bitsegs(Segs, Vt0, St0),
    {[binary|Ss],Eqt,Vt1,St1};
from_pat({map,_,Assocs}, Vt0, St0) ->
    {Ps,Eqt,Vt1,St1} = from_pat_map_assocs(Assocs, Vt0, St0),
    {[map|Ps],Eqt,Vt1,St1};
from_pat({record,_,Name,Fs}, Vt0, St0) ->          %Match a record
    {Sfs,Eqt,Vt1,St1} = from_pat_rec_fields(Fs, Vt0, St0),
    {['make-record',Name|Sfs],Eqt,Vt1,St1};
from_pat({record_index,_,Name,{atom,_,F}}, Vt, St) -> %We KNOW!
    {['record-index',Name,F],Vt,St};
from_pat({match,_,P1,P2}, Vt0, St0) ->          %Pattern aliases
    {Lp1,Eqt1,Vt1,St1} = from_pat(P1, Vt0, St0),
    {Lp2,Eqt2,Vt2,St2} = from_pat(P2, Vt1, St1),
    {['=',Lp1,Lp2],Eqt1++Eqt2,Vt2,St2};
%% Basically illegal syntax which maybe generated by internal tools.
from_pat({call,_,{atom,_,F},As}, Vt0, St0) ->
    %% This will never occur in real code but for macro expansions.
    {Las,Eqt,Vt1,St1} = from_pats(As, Vt0, St0),
    {[F|Las],Eqt,Vt1,St1}.

from_pats([P|Ps], Vt0, St0) ->
    {Lp,Eqt,Vt1,St1} = from_pat(P, Vt0, St0),
    {Lps,Eqts,Vt2,St2} = from_pats(Ps, Vt1, St1),
    {[Lp|Lps],Eqt++Eqts,Vt2,St2};
from_pats([], Vt, St) -> {[],[],Vt,St}.

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
from_pat_bitseg_size({var,_,V}, Vt, St) ->  %Size vars never match
    {[[size,V]],Vt,St};
from_pat_bitseg_size(Size, Vt0, St0) ->
    {Ssize,_,Vt1,St1} = from_pat(Size, Vt0, St0),
    {[[size,Ssize]],Vt1,St1}.

%% from_pat_map_assocs(Fields, VarTable, State) ->
%%     {Fields,EqTable,VarTable,State}.

from_pat_map_assocs([{map_field_exact,_,Key,Val}|As], Vt0, St0) ->
    {Lk,Eqt1,Vt1,St1} = from_pat(Key, Vt0, St0),
    {Lv,Eqt2,Vt2,St2} = from_pat(Val, Vt1, St1),
    {Lfs,Eqt3,Vt3,St3} = from_pat_map_assocs(As, Vt2, St2),
    {[Lk,Lv|Lfs],Eqt1 ++ Eqt2 ++ Eqt3,Vt3,St3};
from_pat_map_assocs([], Vt, St) -> {[],[],Vt,St}.

%% from_pat_rec_fields(Recfields, VarTable, State) ->
%%     {Recfields,EqTable,VarTable,State}.

from_pat_rec_fields([{record_field,_,{atom,_,F},P}|Fs], Vt0, St0) ->
    {Lp,Eqt,Vt1,St1} = from_pat(P, Vt0, St0),
    {Lfs,Eqts,Vt2,St2} = from_pat_rec_fields(Fs, Vt1, St1),
    {[F,Lp|Lfs],Eqt++Eqts,Vt2,St2};
from_pat_rec_fields([{record_field,_,{var,_,F},P}|Fs], Vt0, St0) ->
    %% Special case!!
    {Lp,Eqt,Vt1,St1} = from_pat(P, Vt0, St0),
    {Lfs,Eqts,Vt2,St2} = from_pat_rec_fields(Fs, Vt1, St1),
    {[F,Lp|Lfs],Eqt++Eqts,Vt2,St2};
from_pat_rec_fields([], Vt, St) -> {[],[],Vt,St}.

%% from_lit(Literal) -> Literal.
%%  Build a literal value from AST. No quoting here.

from_lit(Lit) ->
    erl_parse:normalise(Lit).

%% Converting LFE to Erlang AST.
%% This is relatively straightforward except for 2 things:
%% - No shadowing of variables so they must be uniquely named.
%% - Local functions must lifted to top level. This is difficult for
%% one expression so this is illegal here as we don't do it.
%%
%% We keep track of all existing variables so when we get a variable
%% in a pattern we can check if this variable has been used before. If
%% so then we must create a new unique variable, add a guard test and
%% add the old-new mapping to the variable table. The existence is
%% global while the mapping is local to that scope. Multiple
%% occurences of variables in an LFE pattern map directly to multiple
%% occurrences in the Erlang AST.

%% Use macros for key-value tables if they exist.
 -ifdef(HAS_FULL_KEYS).
-define(NEW_VT, #{}).
-define(VT_GET(K, Vt), maps:get(K, Vt)).
-define(VT_GET(K, Vt, Def), maps:get(K, Vt, Def)).
-define(VT_IS_KEY(K, Vt), maps:is_key(K, Vt)).
-define(VT_PUT(K, V, Vt), Vt#{K => V}).
-else.
-define(NEW_VT, orddict:new()).
-define(VT_GET(K, Vt), orddict:fetch(K, Vt)).
-define(VT_GET(K, Vt, Def),
        %% Safe as no new variables created.
        case orddict:is_key(K, Vt) of
            true -> orddict:fetch(K, Vt);
            false -> Def
        end).
-define(VT_IS_KEY(K, Vt), orddict:is_key(K, Vt)).
-define(VT_PUT(K, V, Vt), orddict:store(K, V, Vt)).
-endif.

%% safe_fetch(Key, Dict, Default) -> Value.
%%  Fetch a value with a default if it doesn't exist.

%% safe_fetch(Key, Dict, Def) ->
%%     case orddict:find(Key, Dict) of
%%         {ok,Val} -> Val;
%%         error -> Def
%%     end.

-record(to, {vs=[],                             %Existing variables
             vc=?NEW_VT,                        %Variable counter
             imports=[],                        %Function renames
             aliases=[]                         %Module aliases
            }).

%% to_expr(Expr, LineNumber) -> ErlExpr.
%% to_expr(Expr, LineNumber, {Imports, Aliases}) -> ErlExpr.
%% to_exprs(Expr, LineNumber) -> ErlExprs.
%% to_exprs(Expr, LineNumber, {Imports, Aliases}) -> ErlExprs.

to_expr(E, L) ->
    to_expr(E, L, {[],[]}).

to_expr(E, L, {Imports,Aliases}) ->
    {Ee,_} = to_expr(E, L, ?NEW_VT, #to{imports=Imports,aliases=Aliases}),
    Ee.

to_exprs(Es, L) ->
    to_exprs(Es, L, {[],[]}).

to_exprs(Es, L, {Imports,Aliases}) ->
    {Ees,_} = to_exprs(Es, L, ?NEW_VT, #to{imports=Imports,aliases=Aliases}),
    Ees.

%% to_expr(Expr, LineNumber, VarTable, State) -> {ErlExpr,State}.

%% Core data special forms.
to_expr(?Q(Lit), L, _, St) -> {to_lit(Lit, L),St};
to_expr([cons,H,T], L, Vt, St0) ->
    {Eh,St1} = to_expr(H, L, Vt, St0),
    {Et,St2} = to_expr(T, L, Vt, St1),
    {{cons,L,Eh,Et},St2};
to_expr([car,E], L, Vt, St0) ->
    {Ee,St1} = to_expr(E, L, Vt, St0),
    {{call,L,{atom,L,hd},[Ee]},St1};
to_expr([cdr,E], L, Vt, St0) ->
    {Ee,St1} = to_expr(E, L, Vt, St0),
    {{call,L,{atom,L,tl},[Ee]},St1};
to_expr([list|Es], L, Vt, St) ->
    Fun = fun (E, {Tail,St0}) ->
                  {Ee,St1} = to_expr(E, L, Vt, St0),
                  {{cons,L,Ee,Tail},St1}
          end,
    foldr(Fun, {{nil,L},St}, Es);
to_expr(['list*'|Es], L, Vt, St) ->             %Macro
    to_exprs_s(fun to_expr/4, L, Vt, St, Es);
to_expr([tuple|Es], L, Vt, St0) ->
    {Ees,St1} = to_exprs(Es, L, Vt, St0),
    {{tuple,L,Ees},St1};
to_expr([tref,T,I], L, Vt, St0) ->
    {Et,St1} = to_expr(T, L, Vt, St0),
    {Ei,St2} = to_expr(I, L, Vt, St1),
    %% Get the argument order correct.
    {{call,L,{atom,L,element},[Ei,Et]},St2};
to_expr([tset,T,I,V], L, Vt, St0) ->
    {Et,St1} = to_expr(T, L, Vt, St0),
    {Ei,St2} = to_expr(I, L, Vt, St1),
    {Ev,St2} = to_expr(V, L, Vt, St2),
    %% Get the argument order correct.
    {{call,L,{atom,L,setelement},[Ei,Et,Ev]},St2};
to_expr([binary|Segs], L, Vt, St0) ->
    {Esegs,St1} = to_bitsegs(Segs, L, Vt, St0),
    {{bin,L,Esegs},St1};
to_expr([map|Pairs], L, Vt, St0) ->
    {Eps,St1} = to_map_pairs(Pairs, map_field_assoc, L, Vt, St0),
    {{map,L,Eps},St1};
to_expr([msiz,Map], L, Vt, St) ->
    to_expr([map_size,Map], L, Vt, St);
to_expr([mref,Map,Key], L, Vt, St) ->
    {As,St1} = to_exprs([Key, Map], L, Vt, St),
    to_remote_call({atom,L,maps}, {atom,L,get}, As, L, St1);
to_expr([mset,Map|Pairs], L, Vt, St) ->
    to_map_set(Map, Pairs, L, Vt, St);
to_expr([mupd,Map|Pairs], L, Vt, St) ->
    to_map_update(Map, Pairs, L, Vt, St);
to_expr([mrem,Map|Keys], L, Vt, St) ->
    to_map_remove(Map, Keys, L, Vt, St);
to_expr(['map-size',Map], L, Vt, St) ->
    to_expr([map_size,Map], L, Vt, St);
to_expr(['map-get',Map,Key], L, Vt, St) ->
    {As,St1} = to_exprs([Key, Map], L, Vt, St),
    to_remote_call({atom,L,maps}, {atom,L,get}, As, L, St1);
to_expr(['map-set',Map|Pairs], L, Vt, St) ->
    to_map_set(Map, Pairs, L, Vt, St);
to_expr(['map-update',Map|Pairs], L, Vt, St) ->
    to_map_update(Map, Pairs, L, Vt, St);
to_expr(['map-remove',Map|Keys], L, Vt, St) ->
    to_map_remove(Map, Keys, L, Vt, St);
%% Record special forms.
to_expr(['make-record',Name|Fs], L, Vt, St0) ->
    {Efs,St1} = to_rec_fields(Fs, L, Vt, St0),
    {{record,L,Name,Efs},St1};
to_expr(['record-index',Name,F], L, _, St) ->
    {{record_index,L,Name,{atom,L,F}},St};
to_expr(['record-field',E,Name,F], L, Vt, St0) ->
    {Ee,St1} = to_expr(E, L, Vt, St0),
    {{record_field,L,Ee,Name,{atom,L,F}},St1};
to_expr(['record-update',E,Name|Fs], L, Vt, St0) ->
    {Ee,St1} = to_expr(E, L, Vt, St0),
    {Efs,St2} = to_rec_fields(Fs, L, Vt, St1),
    {{record,L,Ee,Name,Efs},St2};
%% Function forms.
to_expr([function,F,Ar], L, Vt, St) ->
    %% Must handle the special cases here.
    case lfe_internal:is_erl_bif(F, Ar) of
        true -> to_expr([function,erlang,F,Ar], L, Vt, St);
        false ->
            case lfe_internal:is_lfe_bif(F, Ar) of
                true -> to_expr([function,lfe,F,Ar], L, Vt, St);
                false -> {{'fun',L,{function,F,Ar}},St}
            end
    end;
to_expr([function,M,F,Ar], L, _, St) ->
    %% Need the abstract values here.
    {{'fun',L,{function,to_lit(M, L),to_lit(F, L),to_lit(Ar, L)}},St};
%% Special known data type operations.
to_expr(['andalso'|Es], L, Vt, St) ->
    to_lazy_logic(Es, 'andalso', L, Vt, St);
to_expr(['orelse'|Es], L, Vt, St) ->
    to_lazy_logic(Es, 'orelse', L, Vt, St);
%% Core closure special forms.
to_expr([lambda,Args|Body], L, Vt, St) ->
    to_lambda(Args, Body, L, Vt, St);
to_expr(['match-lambda'|Cls], L, Vt, St0) ->
    {Ecls,St1} = to_fun_cls(Cls, L, Vt, St0),
    {{'fun',L,{clauses,Ecls}},St1};
to_expr(['let',Lbs|B], L, Vt, St) ->
    to_let(Lbs, B, L, Vt, St);
to_expr(['let-function'|_], L, _, _) ->         %Can't do this efficently
    illegal_code_error(L, 'let-function');
to_expr(['letrec-function'|_], L, _, _) ->      %Can't do this efficently
    illegal_code_error(L, 'letrec-function');
%% Core control special forms.
to_expr([progn|B], L, Vt, St) ->
    to_block(B, L, Vt, St);
to_expr(['if'|Body], L, Vt, St) ->
    to_if(Body, L, Vt, St);
to_expr(['case'|Body], L, Vt, St) ->
    to_case(Body, L, Vt, St);
to_expr(['receive'|Cls], L, Vt, St) ->
    to_receive(Cls, L, Vt, St);
to_expr(['catch'|B], L, Vt, St0) ->
    {Eb,St1} = to_block(B, L, Vt, St0),
    {{'catch',L,Eb},St1};
to_expr(['try'|Try], L, Vt, St) ->              %Can't do this yet
    %% lfe_io:format("try ~w\n~p\n", [L,['try'|Try]]),
    to_try(Try, L, Vt, St);
to_expr([funcall,F|As], L, Vt, St0) ->
    {Ef,St1} = to_expr(F, L, Vt, St0),
    {Eas,St2} = to_exprs(As, L, Vt, St1),
    {{call,L,Ef,Eas},St2};
%% Special known macros.
to_expr([lc,Qs|Es], L, Vt0, St0) ->
    {Eqs,Vt1,St1} = to_lc_quals(Qs, L, Vt0, St0),
    {Ees,St2} = to_block(Es, L, Vt1, St1),
    {{lc,L,Ees,Eqs},St2};
%% General function calls.
to_expr([call,?Q(erlang),?Q(F)|As], L, Vt, St0) ->
    %% This is semantically the same but some tools behave differently
    %% (qlc_pt).
    {Eas,St1} = to_exprs(As, L, Vt, St0),
    case is_erl_op(F, length(As)) of
        true -> {list_to_tuple([op,L,F|Eas]),St1};
        false ->
            to_remote_call({atom,L,erlang}, {atom,L,F}, Eas, L, St1)
    end;
to_expr([call,?Q(M0),F|As], L, Vt, St0) ->
    %% Alias modules are literals.
    Mod = case orddict:find(M0, St0#to.aliases) of
              {ok,M1} -> M1;
              error -> M0
          end,
    {Ef,St1} = to_expr(F, L, Vt, St0),
    {Eas,St2} = to_exprs(As, L, Vt, St1),
    to_remote_call({atom,L,Mod}, Ef, Eas, L, St2);
to_expr([call,M,F|As], L, Vt, St0) ->
    {Em,St1} = to_expr(M, L, Vt, St0),
    {Ef,St2} = to_expr(F, L, Vt, St1),
    {Eas,St3} = to_exprs(As, L, Vt, St2),
    to_remote_call(Em, Ef, Eas, L, St3);
%% General function call.
to_expr([F|As], L, Vt, St0) when is_atom(F) ->
    {Eas,St1} = to_exprs(As, L, Vt, St0),
    Ar = length(As),                            %Arity
    %% Check for import.
    case orddict:find({F,Ar}, St1#to.imports) of
        {ok,{Mod,R}} ->                         %Imported
            to_remote_call({atom,L,Mod}, {atom,L,R}, Eas, L, St1);
        error ->                                %Not imported
            case is_erl_op(F, Ar) of
                true -> {list_to_tuple([op,L,F|Eas]),St1};
                false ->
                    case lfe_internal:is_lfe_bif(F, Ar) of
                        true ->
                            to_remote_call({atom,L,lfe}, {atom,L,F}, Eas, L, St1);
                        false ->
                            {{call,L,{atom,L,F},Eas},St1}
                    end
            end
    end;
to_expr([_|_]=List, L, _, St) ->
    case lfe_lib:is_posint_list(List) of
        true -> {{string,L,List},St};
        false ->
            illegal_code_error(L, list)         %Not right!
    end;
to_expr(V, L, Vt, St) when is_atom(V) ->        %Unquoted atom
    to_expr_var(V, L, Vt, St);
to_expr(Lit, L, _, St) ->                       %Everything else is a literal
    {to_lit(Lit, L),St}.

to_expr_var(V, L, Vt, St) ->
    Var = ?VT_GET(V, Vt, V),                    %Hmm
    {{var,L,Var},St}.

to_remote_call(M, F, As, L, St) ->
    {{call,L,{remote,L,M,F},As},St}.

%% is_erl_op(Op, Arity) -> bool().
%% Is Op/Arity one of the known Erlang operators?

is_erl_op(Op, Ar) ->
    erl_internal:arith_op(Op, Ar)
    orelse erl_internal:bool_op(Op, Ar)
    orelse erl_internal:comp_op(Op, Ar)
    orelse erl_internal:list_op(Op, Ar)
    orelse erl_internal:send_op(Op, Ar).

to_body(Es, L, Vt, St) ->
    Fun = fun (E, St0) -> to_expr(E, L, Vt, St0) end,
    mapfoldl(Fun, St, Es).

to_exprs(Es, L, Vt, St) ->
    Fun = fun (E, St0) -> to_expr(E, L, Vt, St0) end,
    mapfoldl(Fun, St, Es).

to_exprs_s(Fun, L, Vt, St, [E]) -> Fun(E, L, Vt, St);
to_exprs_s(Fun, L, Vt, St0, [E|Es]) ->
    {Les,St1} = to_exprs_s(Fun, L, Vt, St0, Es),
    {Le,St2} = Fun(E, L, Vt, St1),
    {{cons,L,Le,Les},St2};
to_exprs_s(_, L, _, St, []) -> {{nil,L},St}.

to_pats_s(Fun, L, Pvs, Vt, St, [E]) -> Fun(E, L, Pvs, Vt, St);
to_pats_s(Fun, L, Pvs0, Vt0, St0, [E|Es]) ->
    {Les,Pvs1,Vt1,St1} = to_pats_s(Fun, L, Pvs0, Vt0, St0, Es),
    {Le,Pvs2, Vt2,St2} = Fun(E, L, Pvs1, Vt1, St1),
    {{cons,L,Le,Les},Pvs2,Vt2,St2};
to_pats_s(_, L, Pvs, Vt, St, []) -> {{nil,L},Pvs,Vt,St}.

%% to_bitsegs(Segs, LineNumber, VarTable, State) -> {Segs,State}.
%%  We don't do any real checking here but just assume that everything
%%  is correct and in worst case pass the buck to the Erlang compiler.


to_bitsegs(Ss, L, Vt, St) ->
    Fun = fun (S, St0) -> to_bitseg(S, L, Vt, St0) end,
    mapfoldl(Fun, St, Ss).

to_bitseg([Val|Specs]=Seg, L, Vt, St) ->
    case lfe_lib:is_posint_list(Seg) of
        true ->
            {{bin_element,L,{string,L,Seg},default,default},St};
        false ->
            to_bin_element(Val, Specs, L, Vt, St)
    end;
to_bitseg(Val, L, Vt, St) ->
    to_bin_element(Val, [], L, Vt, St).

to_bin_element(Val, Specs, L, Vt, St0) ->
    {Eval,St1} = to_expr(Val, L, Vt, St0),
    {Size,Type} = to_bitseg_type(Specs, default, []),
    {Esiz,St2} = to_bin_size(Size, L, Vt, St1),
    {{bin_element,L,Eval,Esiz,Type},St2}.

to_bitseg_type([[size,Size]|Specs], _, Type) ->
    to_bitseg_type(Specs, Size, Type);
to_bitseg_type([[unit,Unit]|Specs], Size, Type) ->
    to_bitseg_type(Specs, Size, Type ++ [{unit,Unit}]);
to_bitseg_type([Spec|Specs], Size, Type) ->
    to_bitseg_type(Specs, Size, Type ++ [Spec]);
to_bitseg_type([], Size, []) -> {Size,default};
to_bitseg_type([], Size, Type) -> {Size,Type}.

to_bin_size(all, _, _, St) -> {default,St};
to_bin_size(default, _, _, St) -> {default,St};
to_bin_size(undefined, _, _, St) -> {default,St};
to_bin_size(Size, L, Vt, St) -> to_expr(Size, L, Vt, St).

%% to_map_set(Map, Pairs, L, Vt, State) -> {MapSet,State}.
%% to_map_update(Map, Pairs, L, Vt, State) -> {MapUpdate,State}.
%% to_map_remove(Map, Keys, L, Vt, State) -> {MapRemove,State}.

to_map_set(Map, Pairs, L, Vt, St0) ->
    {Em,St1} = to_expr(Map, L, Vt, St0),
    {Eps,St2} = to_map_pairs(Pairs, map_field_assoc, L, Vt, St1),
    {{map,L,Em,Eps},St2}.

to_map_update(Map, Pairs, L, Vt, St0) ->
    {Em,St1} = to_expr(Map, L, Vt, St0),
    {Eps,St2} = to_map_pairs(Pairs, map_field_exact, L, Vt, St1),
    {{map,L,Em,Eps},St2}.

to_map_remove(Map, Keys, L, Vt, St0) ->
    {Em,St1} = to_expr(Map, L, Vt, St0),
    {Eks,St2} = to_exprs(Keys, L, Vt, St1),
    Fun = fun (K, {F,St}) ->
                  to_remote_call({atom,L,maps}, {atom,L,remove}, [K,F], L, St)
          end,
    lists:foldl(Fun, {Em,St2}, Eks).

%% to_map_pairs(Pairs, LineNumber, VarTable, State) -> {Fields,State}.

to_map_pairs([K,V|Ps], Field, L, Vt, St0) ->
    {Ek,St1} = to_expr(K, L, Vt, St0),
    {Ev,St2} = to_expr(V, L, Vt, St1),
    {Eps,St3} = to_map_pairs(Ps, Field, L, Vt, St2),
    {[{Field,L,Ek,Ev}|Eps],St3};
to_map_pairs([], _, _, _, St) -> {[],St}.

%% to_rec_fields(Fields, LineNumber, VarTable, State) -> {Fields,State}.

to_rec_fields(['_',V|Fs], L, Vt, St0) ->
    %% Special case!!
    {Ev,St1} = to_expr(V, L, Vt, St0),
    {Efs,St2} = to_rec_fields(Fs, L, Vt, St1),
    {[{record_field,L,{var,L,'_'},Ev}|Efs],St2};
to_rec_fields([F,V|Fs], L, Vt, St0) ->
    {Ev,St1} = to_expr(V, L, Vt, St0),
    {Efs,St2} = to_rec_fields(Fs, L, Vt, St1),
    {[{record_field,L,{atom,L,F},Ev}|Efs],St2};
to_rec_fields([], _, _, St) -> {[],St}.

%% to_fun_cls(Clauses, LineNumber) -> Clauses.
%% to_fun_cl(Clause, LineNumber) -> Clause.
%%  Function clauses.

to_fun_cls(Cls, L, Vt, St) ->
    Fun = fun (Cl, St0) -> to_fun_cl(Cl, L, Vt, St0) end,
    mapfoldl(Fun, St, Cls).

to_fun_cl([As,['when']|B], L, Vt0, St0) ->
    {Eas,Vt1,St1} = to_pats(As, L, Vt0, St0),
    {Eb,St2} = to_body(B, L, Vt1, St1),
    {{clause,L,Eas,[],Eb},St2};
to_fun_cl([As,['when'|G]|B], L, Vt0, St0) ->
    {Eas,Vt1,St1} = to_pats(As, L, Vt0, St0),
    {Eg,St2} = to_body(G, L, Vt1, St1),
    {Eb,St3} = to_body(B, L, Vt1, St2),
    {{clause,L,Eas,[Eg],Eb},St3};
to_fun_cl([As|B], L, Vt0, St0) ->
    {Eas,Vt1,St1} = to_pats(As, L, Vt0, St0),
    {Eb,St2} = to_body(B, L, Vt1, St1),
    {{clause,L,Eas,[],Eb},St2}.

%% to_lazy_logic(Exprs, Type, LineNumber, VarTable, State) -> {Logic,State}.
%%  These go pairwise right-to-left.

to_lazy_logic([E1,E2], Type, L, Vt, St0) ->
    {Ee1,St1} = to_expr(E1, L, Vt, St0),
    {Ee2,St2} = to_expr(E2, L, Vt, St1),
    {{op,L,Type,Ee1,Ee2},St2};
to_lazy_logic([E1|Es], Type, L, Vt, St0) ->
    {Ee1,St1} = to_expr(E1, L, Vt, St0),
    {Ees,St2} = to_lazy_logic(Es, Type, L, Vt, St1),
    {{op,L,Type,Ee1,Ees},St2}.

%% to_lambda(Args, Body, LineNumber, VarTable, State) -> {Lambda,State}.

to_lambda(As, B, L, Vt, St0) ->
    {Ecl,St1} = to_fun_cl([As|B], L, Vt, St0),
    {{'fun',L,{clauses,[Ecl]}},St1}.

%% to_let(VarBindings, Body, LineNumber, VarTable, State) -> {Block,State}.

to_let(Lbs, B, L, Vt0, St0) ->
    {Ebs,Vt1,St1} = to_let_bindings(Lbs, L, Vt0, St0),
    {Eb,St2} = to_body(B, L, Vt1, St1),
    {{block,L,Ebs ++ Eb},St2}.

%% to_let_bindings(Bindings, LineNumber, VarTable, State) ->
%%     {Block,VarTable,State}.
%%  When we have a guard translate into a case but special case where
%%  we have an empty guard as erlang compiler doesn't like this.

to_let_bindings(Lbs, L, Vt, St) ->
    Fun = fun ([P,E], Vt0, St0) ->
                  {Ep,Vt1,St1} = to_pat(P, L, Vt0, St0),
                  {Ee,St2} = to_expr(E, L, Vt0, St1),
                  {{match,L,Ep,Ee},Vt1,St2};
              ([P,['when'],E], Vt0, St0) ->     %Just to keep it short
                  {Ep,Vt1,St1} = to_pat(P, L, Vt0, St0),
                  {Ee,St2} = to_expr(E, L, Vt0, St1),
                  {{match,L,Ep,Ee},Vt1,St2};
              ([P,['when'|G],E], Vt0, St0) ->
                  {Ep,Vt1,St1} = to_pat(P, L, Vt0, St0),
                  {Eg,St2} = to_body(G, L, Vt1, St1),
                  {Ee,St3} = to_expr(E, L, Vt1, St2),
                  {{'case',L,Ee,[{clause,L,[Ep],[Eg],[Ep]}]},Vt1,St3}
          end,
    mapfoldl2(Fun, Vt, St, Lbs).

%% to_block(Expressions, LineNumber, VarTable, State) -> {Block,State}.
%%  Specially check for empty block and then just return (), and for
%%  block with one expression and then just return that expression.

to_block(Es, L, Vt, St0) ->
    case to_exprs(Es, L, Vt, St0) of
        {[Ee],St1} -> {Ee,St1};                 %No need to wrap
        {[],St1} -> {{nil,L},St1};              %Returns ()
        {Ees,St1} -> {{block,L,Ees},St1}        %Must wrap
    end.

%% to_if(IfBody, LineNumber, VarTable, State) -> {ErlCase,State}.

to_if([Test,True], L, Vt, St) ->
    to_if(Test, True, ?Q(false), L, Vt, St);
to_if([Test,True,False], L, Vt, St) ->
    to_if(Test, True, False, L, Vt, St);
to_if(_, L, _, _) ->
    illegal_code_error(L, 'if').

to_if(Test, True, False, L, Vt, St0) ->
    {Etest,St1} = to_expr(Test, L, Vt, St0),
    {Ecls,St2} = to_icr_cls([[?Q(true),True],[?Q(false),False]], L, Vt, St1),
    {{'case',L,Etest,Ecls},St2}.

%% to_case(CaseBody, LineNumber, VarTable, State) -> {ErlCase,State}.

to_case([E|Cls], L, Vt, St0) ->
    {Ee,St1} = to_expr(E, L, Vt, St0),
    {Ecls,St2} = to_icr_cls(Cls, L, Vt, St1),
    {{'case',L,Ee,Ecls},St2};
to_case(_, L, _, _) ->
    illegal_code_error(L, 'case').

%% to_receive(RecClauses, LineNumber, VarTable, State) -> {ErlRec,State}.

to_receive(Cls0, L, Vt, St0) ->
    %% Get the right receive form depending on whether there is an after.
    {Cls1,A} = splitwith(fun (['after'|_]) -> false; (_) -> true end, Cls0),
    {Ecls,St1} = to_icr_cls(Cls1, L, Vt, St0),
    case A of
        [['after',T|B]] ->
            {Et,St2} = to_expr(T, L, Vt, St1),
            {Eb,St3} = to_body(B, L, Vt, St2),
            {{'receive',L,Ecls,Et,Eb},St3};
        [] ->
            {{'receive',L,Ecls},St1}
    end.

%% to_icr_cls(Clauses, LineNumber, VarTable, State) -> {Clauses,State}.
%% to_icr_cl(Clause, LineNumber, VarTable, State) -> {Clause,State}.
%%  If/case/receive clauses.

to_icr_cls(Cls, L, Vt, St) ->
    Fun = fun (Cl, St0) -> to_icr_cl(Cl, L, Vt, St0) end,
    mapfoldl(Fun, St, Cls).

to_icr_cl([P,['when']|B], L, Vt0, St0) ->
    {Ep,Vt1,St1} = to_pat(P, L, Vt0, St0),
    {Eb,St2} = to_body(B, L, Vt1, St1),
    {{clause,L,[Ep],[],Eb},St2};
to_icr_cl([P,['when'|G]|B], L, Vt0, St0) ->
    {Ep,Vt1,St1} = to_pat(P, L, Vt0, St0),
    {Eg,St2} = to_body(G, L, Vt1, St1),
    {Eb,St3} = to_body(B, L, Vt1, St2),
    {{clause,L,[Ep],[Eg],Eb},St3};
to_icr_cl([P|B], L, Vt0, St0) ->
    {Ep,Vt1,St1} = to_pat(P, L, Vt0, St0),
    {Eb,St2} = to_body(B, L, Vt1, St1),
    {{clause,L,[Ep],[],Eb},St2}.

%% to_try(Try, LineNumber, VarTable, State) -> {ErlTry,State}.
%%  Step down the try body doing each section separately then put them
%%  together. We expand _ catch pattern to {_,_,_}. We remove wrapping
%%  progn in try expression which is not really necessary.

to_try([E|Try], L, Vt, St0) ->
    {Ee,St1} = to_try_expr(E, L, Vt, St0),
    {Ecase,Ecatch,Eafter,St2} = to_try(Try, L, Vt, St1, [], [], []),
    {{'try',L,Ee,Ecase,Ecatch,Eafter},St2}.

to_try_expr([progn|Exprs], L, Vt, St) ->
    to_exprs(Exprs, L, Vt, St);
to_try_expr(Expr, L, Vt, St) ->
    to_exprs([Expr], L, Vt, St).

to_try([['case'|Case]|Try], L, Vt, St0, _, Ecatch, Eafter) ->
    {Ecase,St1} = to_icr_cls(Case, L, Vt, St0),
    to_try(Try, L, Vt, St1, Ecase, Ecatch, Eafter);
to_try([['catch'|Catch]|Try], L, Vt, St0, Ecase, _, Eafter) ->
    {Ecatch,St1} = to_try_cls(Catch, L, Vt, St0),
    to_try(Try, L, Vt, St1, Ecase, Ecatch, Eafter);
to_try([['after'|After]|Try], L, Vt, St0, Ecase, Ecatch, _) ->
    {Eafter,St1} = to_exprs(After, L, Vt, St0),
    to_try(Try, L, Vt, St1, Ecase, Ecatch, Eafter);
to_try([], _, _, St, Ecase, Ecatch, Eafter) ->
    {Ecase,Ecatch,Eafter,St}.

to_try_cls(Cls, L, Vt, St) ->
    Fun = fun (Cl, St0) -> to_try_cl(Cl, L, Vt, St0) end,
    lists:mapfoldl(Fun, St, Cls).

to_try_cl(['_'|Body], L, Vt, St) ->
    to_try_cl([[tuple,'_','_','_']|Body], L, Vt, St);
to_try_cl(Cl, L, Vt, St) ->
    to_icr_cl(Cl, L, Vt, St).

%% to_lc_quals(Qualifiers, LineNumber, VarTable, State) ->
%%     {Qualifiers,VarTable,State}.
%%  Can't use mapfoldl2 as guard habling modifies Qualifiers.

to_lc_quals([['<-',P,E]|Qs], L, Vt0, St0) ->
    {Ep,Vt1,St1} = to_pat(P, L, Vt0, St0),
    {Ee,St2} = to_expr(E, L, Vt1, St1),
    {Eqs,Vt2,St3} = to_lc_quals(Qs, L, Vt1, St2),
    {[{generate,L,Ep,Ee}|Eqs],Vt2,St3};
to_lc_quals([['<-',P,['when'],E]|Qs], L, Vt, St) ->
    to_lc_quals([['<-',P,E]|Qs], L, Vt, St);         %Skip empty guard
to_lc_quals([['<-',P,['when'|G],E]|Qs], L, Vt, St) ->
    to_lc_quals([['<-',P,E]|G ++ Qs], L, Vt, St);    %Move guards to tests
to_lc_quals([T|Qs], L, Vt0, St0) ->
    {Et,St1} = to_expr(T, L, Vt0, St0),
    {Eqs,Vt1,St2} = to_lc_quals(Qs, L, Vt0, St1),
    {[Et|Eqs],Vt1,St2};
to_lc_quals([], _, Vt, St) -> {[],Vt,St}.

%% new_to_var(Base, State) -> {VarName, State}.
%%  Each base has it's own counter which makes it easier to keep track
%%  of a series. We make sure the variable actually is new and update
%%  the state.

new_to_var(Base, #to{vs=Vs,vc=Vct}=St) ->
    C = ?VT_GET(Base, Vct, 0),
    new_to_var_loop(Base, C, Vs, Vct, St).

new_to_var_loop(Base, C, Vs, Vct, St) ->
    V = list_to_atom(lists:concat(["-",Base,"-",C,"-"])),
    case lists:member(V, Vs) of
        true -> new_to_var_loop(Base, C+1, Vs, Vct, St);
        false ->
            {V,St#to{vs=[V|Vs],vc=?VT_PUT(Base, C+1, Vct)}}
    end.

%% to_pat(Pattern, LineNumber, VarTable, State) -> {Pattern,VarTable,State}.
%% to_pat(Pattern, LineNumber, PatVars, VarTable, State) ->
%%     {Pattern,VarTable,State}.

to_pat(Pat, L, Vt0, St0) ->
    {Epat,_Pvs,Vt1,St1} = to_pat(Pat, L, [], Vt0, St0),
    {Epat,Vt1,St1}.

to_pat([], L, Pvs, Vt, St) -> {{nil,L},Pvs,Vt,St};
to_pat(I, L, Pvs, Vt, St) when is_integer(I) ->
    {{integer,L,I},Pvs,Vt,St};
to_pat(F, L, Pvs, Vt, St) when is_float(F) ->
    {{float,L,F},Pvs,Vt,St};
to_pat(V, L, Pvs, Vt, St) when is_atom(V) ->    %Unquoted atom
    to_pat_var(V, L, Pvs, Vt, St);
to_pat(T, L, Pvs, Vt, St) when is_tuple(T) ->   %Tuple literal
    {to_lit(T, L),Pvs,Vt,St};
to_pat(B, L, Pvs, Vt, St) when is_binary(B) ->  %Binary literal
    {to_lit(B, L),Pvs,Vt,St};
to_pat(M, L, Pvs, Vt, St) when ?IS_MAP(M) ->    %Map literal
    {to_lit(M, L),Pvs,Vt,St};
to_pat(?Q(P), L, Pvs, Vt, St) ->                %Everything quoted here
    {to_lit(P, L),Pvs,Vt,St};
to_pat([cons,H,T], L, Pvs0, Vt0, St0) ->
    {[Eh,Et],Pvs1,Vt1,St1} = to_pats([H,T], L, Pvs0, Vt0, St0),
    {{cons,L,Eh,Et},Pvs1,Vt1,St1};
to_pat([list|Es], L, Pvs, Vt, St) ->
    Fun = fun (E, {Tail,Pvs0,Vt0,St0}) ->
                  {Ee,Pvs1,Vt1,St1} = to_pat(E, L, Pvs0, Vt0, St0),
                  {{cons,L,Ee,Tail},Pvs1,Vt1,St1}
          end,
    foldr(Fun, {{nil,L},Pvs,Vt,St}, Es);
to_pat(['list*'|Es], L, Pvs, Vt, St) ->         %Macro
    to_pats_s(fun to_pat/5, L, Pvs, Vt, St, Es);
to_pat([tuple|Es], L, Pvs0, Vt0, St0) ->
    {Ees,Pvs1,Vt1,St1} = to_pats(Es, L, Pvs0, Vt0, St0),
    {{tuple,L,Ees},Pvs1,Vt1,St1};
to_pat([binary|Segs], L, Pvs0, Vt0, St0) ->
    {Esegs,Pvs1,Vt1,St1} = to_pat_bitsegs(Segs, L, Pvs0, Vt0, St0),
    {{bin,L,Esegs},Pvs1,Vt1,St1};
to_pat([map|Pairs], L, Pvs0, Vt0, St0) ->
    {As,Pvs1,Vt1,St1} = to_pat_map_pairs(Pairs, L, Pvs0, Vt0, St0),
    {{map,L,As},Pvs1,Vt1,St1};
to_pat(['make-record',R|Fs], L, Pvs0, Vt0, St0) ->
    {Efs,Pvs1,Vt1,St1} = to_pat_rec_fields(Fs, L, Pvs0, Vt0, St0),
    {{record,L,R,Efs},Pvs1,Vt1,St1};
to_pat(['record-index',R,F], L, Pvs, Vt, St) ->
    {{record_index,L,R,{atom,L,F}},Pvs,Vt,St};
to_pat(['=',P1,P2], L, Pvs0, Vt0, St0) ->       %Alias
    {Ep1,Pvs1,Vt1,St1} = to_pat(P1, L, Pvs0, Vt0, St0),
    {Ep2,Pvs2,Vt2,St2} = to_pat(P2, L, Pvs1, Vt1, St1),
    {{match,L,Ep1,Ep2},Pvs2, Vt2,St2};
to_pat([_|_]=List, L, Pvs, Vt, St) ->
    case lfe_lib:is_posint_list(List) of
        true -> {to_lit(List, L),Pvs,Vt,St};
        false -> illegal_code_error(L, string)
    end.

to_pats(Ps, L, Vt0, St0) ->
     {Eps,_Pvs1,Vt1,St1} = to_pats(Ps, L, [], Vt0, St0),
    {Eps,Vt1,St1}.

to_pats(Ps, L, Pvs, Vt, St) ->
    Fun = fun (P, Pvs0, Vt0, St0) -> to_pat(P, L, Pvs0, Vt0, St0) end,
    mapfoldl3(Fun, Pvs, Vt, St, Ps).

to_pat_var('_', L, Pvs, Vt, St) ->              %Don't need to handle _
    {{var,L,'_'},Pvs,Vt,St};
to_pat_var(V, L, Pvs, Vt0, St0) ->
    case lists:member(V, Pvs) of
        true ->                                 %Have seen this var in pattern
            V1 = ?VT_GET(V, Vt0),               % so reuse it
            {{var,L,V1},Pvs,Vt0,St0};
        false ->
            {V1,St1} = new_to_var(V, St0),
            Vt1 = ?VT_PUT(V, V1, Vt0),
            {{var,L,V1},[V|Pvs],Vt1,St1}
    end.

to_pat_map_pairs([K,V|Ps], L, Pvs0, Vt0, St0) ->
    {Ek,Pvs1,Vt1,St1} = to_pat(K, L, Pvs0, Vt0, St0),
    {Ev,Pvs2,Vt2,St2} = to_pat(V, L, Pvs1, Vt1, St1),
    {Eps,Pvs3,Vt3,St3} = to_pat_map_pairs(Ps, L, Pvs2, Vt2, St2),
    {[{map_field_exact,L,Ek,Ev}|Eps],Pvs3,Vt3,St3};
to_pat_map_pairs([], _, Pvs, Vt, St) -> {[],Pvs,Vt,St}.

%% to_pat_bitsegs(Segs, LineNumber, VarTable, State) -> {Segs,State}.
%%  We don't do any real checking here but just assume that everything
%%  is correct and in worst case pass the buck to the Erlang compiler.

to_pat_bitsegs(Ss, L, Pvs, Vt, St) ->
    Fun = fun (S, Pvs0, Vt0, St0) -> to_pat_bitseg(S, L, Pvs0, Vt0, St0) end,
    mapfoldl3(Fun, Pvs, Vt, St, Ss).

to_pat_bitseg([Val|Specs]=Seg, L, Pvs, Vt, St) ->
    case lfe_lib:is_posint_list(Seg) of
        true ->
            {{bin_element,L,{string,L,Seg},default,default},St};
        false ->
            to_pat_bin_element(Val, Specs, L, Pvs, Vt, St)
    end;
to_pat_bitseg(Val, L, Pvs, Vt, St) ->
    to_pat_bin_element(Val, [], L, Pvs, Vt, St).

to_pat_bin_element(Val, Specs, L, Pvs0, Vt0, St0) ->
    {Eval,Pvs1,Vt1,St1} = to_pat(Val, L, Pvs0, Vt0, St0),
    {Size,Type} = to_bitseg_type(Specs, default, []),
    {Esiz,Pvs2,Vt2,St2} = to_pat_bin_size(Size, L, Pvs1, Vt1, St1),
    {{bin_element,L,Eval,Esiz,Type},Pvs2,Vt2,St2}.

to_pat_bin_size(all, _, Pvs, Vt, St) -> {default,Pvs,Vt,St};
to_pat_bin_size(default, _, Pvs, Vt, St) -> {default,Pvs,Vt,St};
to_pat_bin_size(undefined, _, Pvs, Vt, St) -> {default,Pvs,Vt,St};
to_pat_bin_size(Size, L, Pvs, Vt, St) -> to_pat(Size, L, Pvs, Vt, St).

%% to_pat_rec_fields(Fields, LineNumber, PatVars, VarTable, State) ->
%%     {Fields,PatVars,VarTable,State}.

to_pat_rec_fields(['_',P|Fs], L, Pvs0, Vt0, St0) ->
    %% Special case!!
    {Ep,Pvs1,Vt1,St1} = to_pat(P, L, Pvs0, Vt0, St0),
    {Efs,Pvs2,Vt2,St2} = to_pat_rec_fields(Fs, L, Pvs1, Vt1, St1),
    {[{record_field,L,{var,L,'_'},Ep}|Efs],Pvs2,Vt2,St2};
to_pat_rec_fields([F,P|Fs], L, Pvs0, Vt0, St0) ->
    {Ep,Pvs1,Vt1,St1} = to_pat(P, L, Pvs0, Vt0, St0),
    {Efs,Pvs2,Vt2,St2} = to_pat_rec_fields(Fs, L, Pvs1, Vt1, St1),
    {[{record_field,L,{atom,L,F},Ep}|Efs],Pvs2,Vt2,St2};
to_pat_rec_fields([], _, Pvs, Vt, St) -> {[],Pvs,Vt,St}.

%% to_lit(Literal, LineNumber) -> ErlLiteral.
%%  Convert a literal value. Note that we KNOW it is a literal value.

to_lit(Lit, L) ->
    %% This does all the work for us.
    erl_parse:abstract(Lit, L).

%% mapfoldl2(Fun, Acc1, Acc2, List) -> {List,Acc1,Acc2}.
%%  Like normal mapfoldl but with 2 accumulators.

mapfoldl2(Fun, A0, B0, [E0|Es0]) ->
    {E1,A1,B1} = Fun(E0, A0, B0),
    {Es1,A2,B2} = mapfoldl2(Fun, A1, B1, Es0),
    {[E1|Es1],A2,B2};
mapfoldl2(_, A, B, []) -> {[],A,B}.

mapfoldl3(Fun, A0, B0, C0, [E0|Es0]) ->
    {E1,A1,B1,C1} = Fun(E0, A0, B0, C0),
    {Es1,A2,B2,C2} = mapfoldl3(Fun, A1, B1, C1, Es0),
    {[E1|Es1],A2,B2,C2};
mapfoldl3(_, A, B, C, []) -> {[],A,B,C}.

illegal_code_error(Line, Error) ->
    error({illegal_code,Line,Error}).
