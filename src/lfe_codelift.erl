%% Copyright (c) 2008-2018 Robert Virding
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

%%% File    : lfe_codelift.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang lambda lifting local functions.

%%% Lambda lift local functions to the top-level. We do this ourselves
%%% to have better control and to be able to do non-recursive
%%% functions in a better way.

-module(lfe_codelift).

-export([function/3]).

-export([comp_define/1]).
-export([lift_func/2,lift_expr/3,ivars_expr/1]).

-export([test/1]).

%% We do a lot of quoting!
-define(Q(E), [quote,E]).
-define(BQ(E), [backquote,E]).
-define(C(E), [comma,E]).
-define(C_A(E), ['comma-at',E]).

-record(cl, {func=[],                           %Current function
             arity=0,
             line=0,
             vc=0,                              %Local variable index
             fc=0                               %Local function index
            }).

%% comp_define(DefForm) -> Funcs

comp_define({Name,Def,Line}) ->
    Fs = [ ['define-function',N,[],D] || {N,D,_} <- function(Name, Def, Line) ],
    [progn|Fs].

%% function(Name, Def, Line) -> [{Name,Def,Line}].
%%  Lambda lift all the local functions and return a list of all
%%  functions.

function(Name, Def, Line) ->
    Ar = func_arity(Def),
    St = #cl{func=Name,arity=Ar,line=Line,vc=0,fc=0},
    %% Lambda lift the function.
    Func = {Name,Def,Line},
    {Funcs,[],_} = lift_loop([Func], St),
    %% io:format("codelift ~p\n", [{Func,Funcs}]),
    Funcs.

%% lift_loop(Functions, State) -> {TopFuncs,LiftedFuncs,State}.
%%  Repeatedly traverse Functions and LiftedFunctions until everything
%%  has been lifted. Return all as TopFuncs.

lift_loop(Funcs0, St0) ->
    {Funcs1,Lds,St1} = lift_funcs(Funcs0, St0),
    if Lds =:= [] ->
            {Funcs1,[],St1};
       true ->
            {Lfuncs,Lds1,St2} = lift_loop(Lds, St1),
            {Funcs1 ++ Lfuncs,Lds1,St2}
    end.

%% lift_func(Name, Definiton, State) -> {Functions,State}.
%%  Lambda lift the local functions in an a function. Return a list of
%%  the resulting functions.

lift_func({Name,Def0,L}, St0) ->
    {Def1,Lds,St1} = lift_expr(Def0, [], St0),
    {{Name,Def1,L},Lds,St1}.

lift_funcs(Defs, St) ->
    Fun = fun (Func0, {Funcs, Lds0, St0}) ->
                  {Func1,Lds,St1} = lift_func(Func0, St0),
                  {[Func1|Funcs],Lds ++ Lds0,St1}
          end,
    lists:foldl(Fun, {[],[],St}, Defs).

%% lift_expr(Expr, LocalDefs, State) -> {AST,LocalDefs,State}.
%%  Lambda lift the local functions in an expression.

%% Core data special forms.
lift_expr(?Q(E), Lds, St) -> {?Q(E), Lds,St};
lift_expr([function,_,_]=Func, Lds, St) ->
    {Func,Lds,St};
lift_expr([function,_,_,_]=Func, Lds, St) ->
    {Func,Lds,St};
%% Core closure special forms.
lift_expr([lambda,Args|Body0], Lds0, St0) ->
    {Body1,Lds1,St1} = lift_exprs(Body0, Lds0, St0),
    {[lambda,Args|Body1],Lds1,St1};
lift_expr(['match-lambda'|Cls0], Lds0, St0) ->
    {Cls1,Lds1,St1} = lift_cls(Cls0, Lds0, St0),
    {['match-lambda'|Cls1],Lds1,St1};
lift_expr(['let',Vbs|Body], Lds, St) ->
    lift_let(Vbs, Body, Lds, St);
lift_expr(['let-function',Fbs|Body], Lds, St) ->
    lift_let_function(Fbs, Body, Lds, St);
lift_expr(['letrec-function',Fbs|Body], Lds, St) ->
    lift_letrec_function(Fbs, Body, Lds, St);
%% Core control special forms.
lift_expr([progn|Body0], Lds0, St0) ->
    {Body1,Lds1,St1} = lift_exprs(Body0, Lds0, St0),
    {[progn|Body1],Lds1,St1};
lift_expr(['if'|Body0], Lds0, St0) ->
    {Body1,Lds1,St1} = lift_exprs(Body0, Lds0, St0),
    {['if'|Body1],Lds1,St1};
lift_expr(['case',Expr|Cls], Lds, St) ->
    lift_case(Expr, Cls, Lds, St);
lift_expr(['catch'|Body0], Lds0, St0) ->
    {Body1,Lds1,St1} = lift_exprs(Body0, Lds0, St0),
    {['catch'|Body1],Lds1,St1};
lift_expr([funcall|Body0], Lds0, St0) ->
    {Body1,Lds1,St1} = lift_exprs(Body0, Lds0, St0),
    {[funcall|Body1],Lds1,St1};
lift_expr([call|Body0], Lds0, St0) ->
    {Body1,Lds1,St1} = lift_exprs(Body0, Lds0, St0),
    {[call|Body1],Lds1,St1};
%% General cases.
lift_expr([Func|Args0], Lds0, St0) when is_atom(Func) ->
    {Args1,Lds1,St1} = lift_exprs(Args0, Lds0, St0),
    {[Func|Args1],Lds1,St1};
lift_expr(Lit, Lds, St) -> {Lit,Lds,St}.

lift_exprs(Exprs, Lds, St) ->
    Fun = fun (E0, {Es,Lds0,St0}) ->
                  {E1,Lds1,St1} = lift_expr(E0, Lds0, St0),
                  {[E1|Es],Lds1,St1}
          end,
    lists:foldr(Fun, {[],Lds,St}, Exprs).

lift_let(Vbs0, Body0, Lds0, St0) ->
    Fun = fun ([Pat,['when'|_]=G,Expr0], {Ldsa,Sta}) ->
                  {Expr1,Ldsb,Stb} = lift_expr(Expr0, Ldsa, Sta),
                  {[Pat,G,Expr1],{Ldsb,Stb}};
              ([Pat,Expr0], {Ldsa,Sta}) ->
                  {Expr1,Ldsb,Stb} = lift_expr(Expr0, Ldsa, Sta),
                  {[Pat,Expr1],{Ldsb,Stb}}
          end,
    {Vbs1,{Lds1,St1}} = lists:mapfoldl(Fun, {Lds0,St0}, Vbs0),
    {Body1,Lds2,St2} = lift_exprs(Body0, Lds1, St1),
    {['let',Vbs1|Body1],Lds2,St2}.

%% lift_let_function(FuncBindings, LocalDefines, State) ->
%%     {LocalBody,LocalDefines,State}.
%%  We can check imported vars separately for each local function as
%%  they do not know of each other.

lift_let_function(Fbs0, Body0, Lds0, St0) ->
    %% Build new name function binding and name transform data.
    Line = St0#cl.line,
    Nfun = fun ([Name,Def0], Ts, Sta) ->
                   Ar = func_arity(Def0),
                   {New,Stb} = new_local_fun_name(Name, Ar, Sta),
		   %% Get the imported variables.
		   Ivs  = ivars_expr(Def0, [], []),
                   Def1 = append_ivars(Def0, Ivs),
                   {{New,Def1,Line},[{trans,Name,Ar,New,Ivs}|Ts],Stb}
           end,
    %% Transform calls in the body.
    {Fbs1,Trans,St1} = mapfoldl2(Nfun, [], St0, Fbs0),
    Lds1 = Fbs1 ++ Lds0,
    %% Apply tranformations to Body.
    Bfun = fun ({trans,Name,Ar,New,Ivs}, B) ->
                   trans_expr(B, Name, Ar, New, Ivs)
           end,
    Body1 = lists:foldl(Bfun, [progn|Body0], Trans),
    lift_expr(Body1, Lds1, St1).

%% lift_letrec_function(FuncBindings, LocalDefines, State) ->
%%     {LocalBody,LocalDefines,State}.
%%  We cheat a bit when checking imported vars, we just take the union
%%  of the variables from all the local functions and pass them to all
%%  functions.

lift_letrec_function(Fbs0, Body0, Lds0, St0) ->
    %% Get the imported variables.
    Ifun = fun ([_,Def], Ivs) -> ivars_expr(Def, [], Ivs) end,
    Ivars = lists:foldl(Ifun, [], Fbs0),
    %% Build new name function binding and name transform data.
    Line = St0#cl.line,
    Nfun = fun ([Name,Def0], Ts, Sta) ->
                   Ar = func_arity(Def0),
                   {New,Stb} = new_local_fun_name(Name, Ar, Sta),
                   Def1 = append_ivars(Def0, Ivars),
                   {{New,Def1,Line},[{trans,Name,Ar,New}|Ts],Stb}
           end,
    {Fbs1,Trans,St1} = mapfoldl2(Nfun, [], St0, Fbs0),
    %% Transform calls in the letrec form.
    Tfun = fun ({trans,Name,Ar,New}, Fbs) ->
                   Ffun = fun ({Nn,Def0,L}) ->
                                  Def1 = trans_expr(Def0, Name, Ar, New, Ivars),
                                  {Nn,Def1,L}
                          end,
                   lists:map(Ffun, Fbs)
           end,
    Fbs2 = lists:foldl(Tfun, Fbs1, Trans),
    Lds1 = Fbs2 ++ Lds0,
    %% Apply tranformations to Body.
    Bfun = fun ({trans,Name,Ar,New}, B) ->
                   trans_expr(B, Name, Ar, New, Ivars)
           end,
    Body1 = lists:foldl(Bfun, [progn|Body0], Trans),
    {Body2,Lds2,St2} = lift_expr(Body1, Lds1, St1),
    {Body2,Lds2,St2}.

append_ivars([lambda,Args|Body], Ivars) ->
    [lambda,Args ++ Ivars|Body];
append_ivars(['match-lambda'|Cls0], Ivars) ->
    Fun = fun ([Pats|Body]) -> [Pats ++ Ivars|Body] end, 
    Cls1 = lists:map(Fun, Cls0),
    ['match-lambda'|Cls1].

lift_cls(Cls, Lds, St) ->
    Fun = fun ([Pats,['when'|_]=G|Body0], {Cls0,Lds0,St0}) ->
                  {Body1,Lds1,St1} = lift_exprs(Body0, Lds0, St0),
                  {[[Pats,G|Body1]|Cls0],Lds1,St1};
              ([Pats|Body0], {Cls0,Lds0,St0}) ->
                  {Body1,Lds1,St1} = lift_exprs(Body0, Lds0, St0),
                  {[[Pats|Body1]|Cls0],Lds1,St1}
          end,
    lists:foldr(Fun, {[],Lds,St}, Cls).

lift_case(Expr0, Cls0, Lds0, St0) ->
    {Expr1,Lds1,St1} = lift_expr(Expr0, Lds0, St0),
    {Cls1,Lds2,St2} = lift_cls(Cls0, Lds1, St1),
    {['case',Expr1|Cls1],Lds2,St2}.

%% trans_expr(Call, Name, Arity, NewName, ImportedVars) -> Expr.
%%  Translate function call from old Name to New and add imported
%%  variables.

%% Core data special forms.
trans_expr(?Q(E), _, _, _, _) -> ?Q(E);
trans_expr([binary|Segs0],  Name, Ar, New, Ivars) ->
    Segs1 = trans_bitsegs(Segs0, Name, Ar, New, Ivars),
    [binary|Segs1];
trans_expr([function,F,A]=Func, Name, Ar, New, Ivars) ->
    if F =:= Name, A =:= Ar ->
	    %% Must return a function of arity A here which calls the
	    %% lifted functions! Can access the imported variables.
	    Vars = new_vars(A),
	    [lambda,Vars,[New|Vars++Ivars]];
       true ->
	    Func
    end;
trans_expr([function,_,_,_]=Func, _, _, _, _) ->
    Func;
%% Core closure special forms.
trans_expr([lambda,Args|Body0], Name, Ar, New, Ivars) ->
    Body1 = trans_exprs(Body0, Name, Ar, New, Ivars),
    [lambda,Args|Body1];
trans_expr(['match-lambda'|Cls0], Name, Ar, New, Ivars) ->
    Cls1 = trans_cls(Cls0, Name, Ar, New, Ivars),
    ['match-lambda'|Cls1];
trans_expr(['let',Vbs|Body], Name, Ar, New, Ivars) ->
    trans_let(Vbs, Body, Name, Ar, New, Ivars);
trans_expr(['let-function',Fbs|Body], Name, Ar, New, Ivars) ->
    trans_let_function(Fbs, Body, Name, Ar, New, Ivars);
trans_expr(['letrec-function',Fbs|Body], Name, Ar, New, Ivars) ->
    trans_letrec_function(Fbs, Body, Name, Ar, New, Ivars);
%% Core control special forms.
trans_expr([progn|Body], Name, Ar, New, Ivars) ->
    [progn|trans_exprs(Body, Name, Ar, New, Ivars)];
trans_expr(['if'|Body], Name, Ar, New, Ivars) ->
    ['if'|trans_exprs(Body, Name, Ar, New, Ivars)];
trans_expr(['case',Expr|Cls], Name, Ar, New, Ivars) ->
    trans_case(Expr, Cls, Name, Ar, New, Ivars);
trans_expr(['receive'|Cls], Name, Ar, New, Ivars) ->
    ['receive'|trans_cls(Cls, Name, Ar, New, Ivars)];
trans_expr(['catch'|Body], Name, Ar, New, Ivars) ->
    ['catch'|trans_exprs(Body, Name, Ar, New, Ivars)];
trans_expr(['try'|Body],  Name, Ar, New, Ivars) ->
    trans_try(Body, Name, Ar, New, Ivars);
trans_expr([funcall|Body], Name, Ar, New, Ivars) ->
    [funcall|trans_exprs(Body, Name, Ar, New, Ivars)];
trans_expr([call|Body], Name, Ar, New, Ivars) ->
    [call|trans_exprs(Body, Name, Ar, New, Ivars)];
%% General cases.
trans_expr([Fun|Args0], Name, Ar, New, Ivars) when is_atom(Fun) ->
    %% Most of the coe data special forms can be handled here as well.
    Far = length(Args0),
    Args1 = trans_exprs(Args0, Name, Ar, New, Ivars),
    if Fun =:= Name,
       Far =:= Ar -> [New|Args1 ++ Ivars];
       true -> [Fun|Args1]
    end;
trans_expr(Lit, _, _, _, _) -> Lit.

trans_exprs(Exprs, Name, Ar, New, Ivars) ->
    Fun = fun (E) -> trans_expr(E, Name, Ar, New, Ivars) end,
    lists:map(Fun, Exprs).

trans_bitsegs(Segs, Name, Ar, New, Ivars) ->
    Fun = fun (Seg) -> trans_bitseg(Seg, Name, Ar, New, Ivars) end,
    lists:map(Fun, Segs).

trans_bitseg([Val0|Specs0], Name, Ar, New, Ivars) ->
    Val1 = trans_expr(Val0, Name, Ar, New, Ivars),
    Fun = fun ([size,E]) -> [size,trans_expr(E, Name, Ar, New, Ivars)] end,
    Specs1 = lists:map(Fun, Specs0),
    [Val1|Specs1];
trans_bitseg(Seg, Name, Ar, New, Ivars) ->
    trans_expr(Seg, Name, Ar, New, Ivars) .

trans_cls(Cls, Name, Ar, New, Ivars) ->
    Fun = fun (Cl) -> trans_cl(Cl, Name, Ar, New, Ivars) end,
    lists:map(Fun, Cls).

%% trans_cl(Clause, Name, Arity, NewName, ImportedVars) -> Clause.
%%  We know that there are no interesting functions in the guard.

trans_cl([Pat,['when'|_]=G|Body], Name, Ar, New, Ivars) ->
    [Pat,G|trans_exprs(Body, Name, Ar, New, Ivars)];
trans_cl([Pat|Body], Name, Ar, New, Ivars) ->
    [Pat|trans_exprs(Body, Name, Ar, New, Ivars)].

trans_let(Vbs0, Body0, Name, Ar, New, Ivars) ->
    Fun = fun ([Pat,['when'|_]=G,Expr0]) ->
                  Expr1 = trans_expr(Expr0, Name, Ar, New, Ivars),
                  [Pat,G,Expr1];
              ([Pat,Expr0]) ->
                  Expr1 = trans_expr(Expr0, Name, Ar, New, Ivars),
                  [Pat,Expr1]
          end,
    Vbs1 = lists:map(Fun, Vbs0),
    Body1 = trans_exprs(Body0, Name, Ar, New, Ivars),
    ['let',Vbs1|Body1].

trans_let_function(Fbs0, Body0, Name, Ar, New, Ivars) ->
    Fbs1 = trans_let_fbs(Fbs0, Name, Ar, New, Ivars),
    Body1 = trans_exprs(Body0, Name, Ar, New, Ivars),
    ['let-function',Fbs1|Body1].

trans_letrec_function(Fbs0, Body0, Name, Ar, New, Ivars) ->
    Fbs1 = trans_let_fbs(Fbs0, Name, Ar, New, Ivars),
    Body1 = trans_exprs(Body0, Name, Ar, New, Ivars),
    ['letrec-function',Fbs1|Body1].

trans_let_fbs(Fbs, Name, Ar, New, Ivars) ->
    Fun = fun ([F,Def]) -> [F,trans_expr(Def, Name, Ar, New, Ivars)] end,
    lists:map(Fun, Fbs).

trans_case(Expr0, Cls0, Name, Ar, New, Ivars) ->
    Expr1 = trans_expr(Expr0, Name, Ar, New, Ivars),
    Cls1 = trans_cls(Cls0, Name, Ar, New, Ivars),
    ['case',Expr1|Cls1].

%% trans_try(TryBody, Name, Arity, NewName, ImportedVars) -> Try.
%%  Step down the try body doing each section separately.

trans_try([E0|Rest0], Name, Ar, New, Ivars) ->
    E1 = trans_expr(E0, Name, Ar, New, Ivars),
    Rest1 = trans_try_case(Rest0, Name, Ar, New, Ivars),
    ['try',E1|Rest1].

trans_try_case([['case'|Cls0]|Rest0], Name, Ar, New, Ivars) ->
    Cls1 = trans_cls(Cls0, Name, Ar, New, Ivars),
    Rest1 = trans_try_catch(Rest0, Name, Ar, New, Ivars),
    [['case'|Cls1]|Rest1];
trans_try_case(Rest, Name, Ar, New, Ivars) ->
    trans_try_catch(Rest, Name, Ar, New, Ivars).

trans_try_catch([['catch'|Cls0]|Rest0], Name, Ar, New, Ivars) ->
    Cls1 = trans_cls(Cls0, Name, Ar, New, Ivars),
    Rest1 = trans_try_after(Rest0, Name, Ar, New, Ivars),
    [['catch'|Cls1]|Rest1];
trans_try_catch(Rest, Name, Ar, New, Ivars) ->
    trans_try_after(Rest, Name, Ar, New, Ivars).

trans_try_after([['after'|Body0]], Name, Ar, New, Ivars) ->
    Body1 = trans_exprs(Body0, Name, Ar, New, Ivars),
    [['after'|Body1]];
trans_try_after([], _, _, _, _) -> [].
                       
func_arity([lambda,Args|_]) -> length(Args);
func_arity(['match-lambda'|Cls]) ->
    match_lambda_arity(Cls).

match_lambda_arity([[Pats|_]|_]) -> length(Pats).

%% new_local_fun_name(Name, Arity, State) -> {FunName,State}.
%%  Create a name for a local function. The name has a similar basic
%%  format as those created in Core Erlang, though not overlapping.

new_local_fun_name(Local, Lar, #cl{func=Func,arity=Far,fc=C}=St) ->
    Name = lists:concat(["-lfe-",Func,"/",Far,"-local-",Local,"/",Lar,"-",C,"-"]),
    {list_to_atom(Name),St#cl{fc=C+1}}.

new_vars(N) when N > 0 ->
    Var = lists:concat(["+var+",N,"+"]),
    [list_to_atom(Var)|new_vars(N-1)];
new_vars(0) -> [].

%% ivars_expr(CoreExpr) -> ImportedVars.
%% ivars_expr(CoreExpr, KnownVars, ImportedVars) -> ImportedVars.
%%  Return the imported variables in a Core expression.

ivars_expr(Core) ->
    ivars_expr(Core, ordsets:new(), ordsets:new()).

%% Core data special forms.
ivars_expr(?Q(_), _Kvars, Ivars) -> Ivars;
ivars_expr([binary|Segs], Kvars, Ivars) ->
    ivars_bitsegs(Segs, Kvars, Ivars);
ivars_expr([function,_,_], _, Ivars) -> Ivars;
ivars_expr([function,_,_,_], _, Ivars) -> Ivars;
%% Core closure special forms.
ivars_expr([lambda,Args|Body], Kvars, Ivars) ->
    ivars_fun_cl([Args|Body], Kvars, Ivars);
ivars_expr(['match-lambda'|Cls], Kvars, Ivars) ->
    ivars_fun_cls(Cls, Kvars, Ivars);
ivars_expr(['let',Vbs|Body], Kvars, Ivars) ->
    ivars_let(Vbs, Body, Kvars, Ivars);
ivars_expr(['let-function',Fbs|Body], Kvars, Ivars) ->
    ivars_let_function(Fbs, Body, Kvars, Ivars);
ivars_expr(['letrec-function',Fbs|Body], Kvars, Ivars) ->
    ivars_let_function(Fbs, Body, Kvars, Ivars);
%% Core control special forms.
ivars_expr([progn|Body], Kvars, Ivars) ->
    ivars_exprs(Body, Kvars, Ivars);
ivars_expr(['if'|Body], Kvars, Ivars) ->
    ivars_exprs(Body, Kvars, Ivars);
ivars_expr(['case',Expr|Cls], Kvars, Ivars0) ->
    Ivars1 = ivars_expr(Expr, Kvars, Ivars0),
    ivars_cls(Cls, Kvars, Ivars1);
ivars_expr(['receive'|Cls], Kvars, Ivars) ->
    ivars_rec_cls(Cls, Kvars, Ivars);
ivars_expr(['catch'|Body], Kvars, Ivars) ->
    ivars_exprs(Body, Kvars, Ivars);
ivars_expr(['try'|Body], Kvars, Ivars) ->
    ivars_try(Body, Kvars, Ivars);
ivars_expr([funcall|Args], Kvars, Ivars) ->
    ivars_exprs(Args, Kvars, Ivars);
ivars_expr([call|Args], Kvars, Ivars) ->
    ivars_exprs(Args, Kvars, Ivars);
%% General cases.
ivars_expr([Fun|Args], Kvars, Ivars) when is_atom(Fun) ->
    ivars_exprs(Args, Kvars, Ivars);
ivars_expr(Var, Kvars, Ivars) when is_atom(Var) ->
    case ordsets:is_element(Var, Kvars) of
        true -> Ivars;
        false -> ordsets:add_element(Var, Ivars)
    end;
ivars_expr(_Lit, _Kvars, Ivars) -> Ivars.       %All literals

ivars_exprs(Exprs, Kvars, Ivars) ->
    Fun = fun (E, Ivs) -> ivars_expr(E, Kvars, Ivs) end,
    lists:foldl(Fun, Ivars, Exprs).

ivars_bitsegs(Segs, Kvars, Ivars) ->
    Fun = fun (Seg, Ivs) -> ivars_bitseg(Seg, Kvars, Ivs) end,
    lists:foldl(Fun, Ivars, Segs).

ivars_bitseg([Val|Specs], Kvars, Ivars0) ->
    %% This works even if bitseg is a string.
    Ivars1 = ivars_expr(Val, Kvars, Ivars0),
    Fun = fun ([size,S], Ivs) -> ivars_expr(S, Kvars, Ivs);
              (_, Ivs) -> Ivs
          end,
    lists:foldl(Fun, Ivars1, Specs);
ivars_bitseg(Val, Kvars, Ivars) ->
    ivars_expr(Val, Kvars, Ivars).

ivars_let(Vbs, Body, Kvars0, Ivars0) ->
    Fun = fun ([Pat,['when'|G],Expr], {Kvs0,Ivs0}) ->
                  Pvs = ivars_pat(Pat),
                  Kvs1 = ordsets:union(Pvs, Kvs0),
                  Ivs1 = ivars_exprs(G, Kvs1, Ivs0),
                  {Kvs1,ivars_expr(Expr, Kvs1, Ivs1)};
              ([Pat,Expr], {Kvs0,Ivs}) ->
                  Pvs = ivars_pat(Pat),
                  Kvs1 = ordsets:union(Pvs, Kvs0),
                  {Kvs1,ivars_expr(Expr, Kvs1, Ivs)}
          end,
    {Kvars1,Ivars1} = lists:foldl(Fun, {Kvars0,Ivars0}, Vbs),
    ivars_exprs(Body, Kvars1, Ivars1).

ivars_let_function(Fbs, Body, Kvars, Ivars0) ->
    Fun = fun ([_,Def], Ivs) -> ivars_expr(Def, Kvars, Ivs) end,
    Ivars1 = lists:foldl(Fun, Ivars0, Fbs),
    ivars_exprs(Body, Kvars, Ivars1).

ivars_fun_cls(Cls, Kvars, Ivars) ->
    Fun = fun (Cl, Ivs) -> ivars_fun_cl(Cl, Kvars, Ivs) end,
    lists:foldl(Fun, Ivars, Cls).

ivars_fun_cl([Pats|Body], Kvars, Ivars) ->
    ivars_cl([[list|Pats]|Body], Kvars, Ivars).

ivars_cls(Cls, Kvars, Ivars) ->
    Fun = fun (Cl, Ivs) -> ivars_cl(Cl, Kvars, Ivs) end,
    lists:foldl(Fun, Ivars, Cls).

ivars_rec_cls(Cls, Kvars, Ivars) ->
    Fun = fun (['after'|Body], Ivs) -> ivars_exprs(Body, Kvars, Ivs);
              (Cl, Ivs) -> ivars_cl(Cl, Kvars, Ivs)
          end,
    lists:foldl(Fun, Ivars, Cls).

ivars_cl([Pat,['when'|G]|Body], Kvars0, Ivars0) ->
    Pvs = ivars_pat(Pat),
    Kvars1 = ordsets:union(Pvs, Kvars0),
    Ivars1 = ivars_exprs(G, Kvars1, Ivars0),
    ivars_exprs(Body, Kvars1, Ivars1);
ivars_cl([Pat|Body], Kvars0, Ivars) ->
    Pvs = ivars_pat(Pat),
    Kvars1 = ordsets:union(Pvs, Kvars0),
    ivars_exprs(Body, Kvars1, Ivars).

%% trans_try(TryBody, KnownVars, ImportedVars) -> ImportedVars.
%%  Step down the try body doing each section separately.

ivars_try([E|Rest], Kvars, Ivars0) ->
    Ivars1 = ivars_expr(E, Kvars, Ivars0),
    ivars_try_case(Rest, Kvars, Ivars1).

ivars_try_case([['case'|Cls]|Rest], Kvars, Ivars0) ->
    Ivars1 = ivars_cls(Cls, Kvars, Ivars0),
    ivars_try_catch(Rest, Kvars, Ivars1);
ivars_try_case(Rest, Kvars, Ivars) ->
    ivars_try_catch(Rest, Kvars, Ivars).

ivars_try_catch([['catch'|Cls]|Rest], Kvars, Ivars0) ->
    Ivars1 = ivars_cls(Cls, Kvars, Ivars0),
    ivars_try_after(Rest, Kvars, Ivars1);
ivars_try_catch(Rest, Kvars, Ivars) ->
    ivars_try_after(Rest, Kvars, Ivars).

ivars_try_after([['after'|Cls]], Kvars, Ivars) ->
    ivars_cls(Cls, Kvars, Ivars);
ivars_try_after([], _, Ivars) -> Ivars.

%% ivars_pat(Pattern) -> PatternVars.
%% ivars_pat(Pattern, PatternVars) -> PatternVars.

ivars_pat(Pat) -> ivars_pat(Pat, ordsets:new()).

ivars_pat(?Q(_), Pvars) -> Pvars;
ivars_pat([list|Es], Pvars) ->
    ivars_pats(Es, Pvars);
ivars_pat([_Fun|Args], Pvars) -> 
    ivars_pats(Args, Pvars);
ivars_pat(Var, Pvars) when is_atom(Var) ->
    ordsets:add_element(Var, Pvars);
ivars_pat(_List, Pvars) -> Pvars.               %All literals

ivars_pats(Pats, Pvars) ->
    Fun = fun (P, Pvs) -> ivars_pat(P, Pvs) end,
    lists:foldl(Fun, Pvars, Pats).

%% mapfoldl2(Fun, Acc1, Acc2, List) -> {List,Acc1,Acc2}.

mapfoldl2(Fun, A0, B0, [E0|Es0]) ->
    {E1,A1,B1} = Fun(E0, A0, B0),
    {Es1,A2,B2} = mapfoldl2(Fun, A1, B1, Es0),
    {[E1|Es1],A2,B2};
mapfoldl2(_, A, B, []) -> {[],A,B}.

%% test(Which) -> Sexpr.

test(1) ->
    %% Straight forward with no func/arity clashes with macros.
    ['let-function',
     [[a,[lambda,[s],[foo,s]]],
      [b,[lambda,[x,y],[bar,[a,x],y]]]],
     [b,42,43]];
test(2) ->
    %% Importing variables.
    ['let-function',
     [[a,[lambda,[s],[foo,s,'i-2']]],
      [b,['match-lambda',[[x,y],[bar,[a,x],y]]]],
      [c,[lambda,[m],[b,m,'i-1']]]],
     [c,43]];
test(3) ->
    %% Have local function a/1 and also calling global a/2.
    ['let-function',
     [[a,[lambda,[s],[foo,s]]],
      [b,[lambda,[x,y],[bar,[a,x],[a,99,y]]]]],
     [b,42,43]];
test(4) ->
    %% Have a local function a/1 call a global a/1.
    ['let-function',
     [[a,[lambda,[s],[a,s]]],
      [b,[lambda,[x,y],[b,[a,x],y]]]],
     [b,42,43]];
test(5) ->
    %% Create a function to local a function.
    [lambda,[x,y,z],
     ['let-function',
      [[foo,[lambda,[a,b],[g1,a,b,z]]]],
      [g2,[function,foo,2],x,y,z]]];
%% Letrec tests.
test(11) ->
    %% Straight forward with no func/arity clashes with macros.
    ['letrec-function',
     [[a,[lambda,[s],[b,10,s]]],
      [b,[lambda,[x,y],[bar,[a,x],y]]]],
     [b,42,43]];
test(12) ->
    %% Importing variables.
    ['letrec-function',
     [[a,[lambda,[s],[b,s,'i-2']]],
      [b,['match-lambda',[[x,y],[bar,[a,x],y]]]],
      [c,[lambda,[m],[b,m,'i-1']]]],
     [c,43]];
test(13) ->
    %% Have local function a/1 and also calling global a/2.
    ['letrec-function',
     [[a,[lambda,[s],[foo,s]]],
      [b,[lambda,[x,y],[bar,[a,x],[a,99,y]]]]],
     [b,42,43]].
