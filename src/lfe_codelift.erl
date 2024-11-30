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

-export([record/3,function/3]).

-export([comp_define/1]).
-export([lift_func/2,lift_expr/3,lift_expr/4,ivars_expr/1]).

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

-record(lift, {type,                            %Lift type
               name,                            %New lifted name
               ivars}).                         %Imported variables

%% comp_define(DefForm) -> Funcs

comp_define({Name,Def,Line}) ->
    Fs = [ ['define-function',N,[],D] || {N,D,_} <- function(Name, Def, Line) ],
    [progn|Fs].

%% record(Name, Fields, Line) -> {RecDef,Functions}.
%%  Lambda lift the record field definitions and return the lifted
%%  fields and generated functions.

record(Name, Fs, Line) ->
    St0 = #cl{func=Name,arity=record,line=Line,vc=0,fc=0},
    Lifts = orddict:new(),
    {Lfs,Fncs,St1} = lift_rec_fields(Fs, Lifts, [], St0),
    {Lfncs,[],_} = lift_loop(Fncs, St1),
    {Lfs,Lfncs}.

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
    Lifts = orddict:new(),
    {Def1,Lds,St1} = lift_expr(Def0, Lifts, [], St0),
    {{Name,Def1,L},Lds,St1}.

lift_funcs(Defs, St) ->
    Fun = fun (Func0, {Funcs, Lds0, St0}) ->
                  {Func1,Lds,St1} = lift_func(Func0, St0),
                  {[Func1|Funcs],Lds ++ Lds0,St1}
          end,
    lists:foldl(Fun, {[],[],St}, Defs).

%% lift_expr(Expr, LocalDefs, State) -> {AST,LocalDefs,State}.
%% lift_expr(Expr, LiftedFuncs, LocalDefs, State) -> {AST,LocalDefs,State}.
%%  Lambda lift the local functions in an expression. The Lifts are
%%  the current function lifts which need to be done.

lift_expr(Expr, Lds, St) ->
    lift_expr(Expr, [], Lds, St).

%% Core data special forms.
lift_expr(?Q(E), _Lifts, Lds, St) -> {?Q(E),Lds,St};
%% Record special forms.
lift_expr(['record',Name|Args], Lifts, Lds0, St0) ->
    {Largs,Lds1,St1} = lift_rec_args(Args, Lifts, Lds0, St0),
    {['record',Name|Largs],Lds1,St1};
%% make-record has been deprecated but we sill accept it for now.
lift_expr(['make-record',Name|Args], Lifts, Lds0, St0) ->
    {Largs,Lds1,St1} = lift_rec_args(Args, Lifts, Lds0, St0),
    {['make-record',Name|Largs],Lds1,St1};
lift_expr(['is-record',E,Name], Lifts, Lds0, St0) ->
    {Le,Lds1,St1} = lift_expr(E, Lifts, Lds0, St0),
    {['is-record',Le,Name],Lds1,St1};
lift_expr(['record-index',_Name,_F]=Ri, _Lifts, Lds, St) ->
    {Ri,Lds,St};
lift_expr(['record-field',E,Name,F], Lifts, Lds0, St0) ->
    {Le,Lds1,St1} = lift_expr(E, Lifts, Lds0, St0),
    {['record-field',Le,Name,F],Lds1,St1};
lift_expr(['record-update',E,Name|Args], Lifts, Lds0, St0) ->
    {Le,Lds1,St1} = lift_expr(E, Lifts, Lds0, St0),
    {Largs,Lds2,St2} = lift_rec_args(Args, Lifts, Lds1, St1),
    {['record-update',Le,Name|Largs],Lds2,St2};
%% Struct special forms.
lift_expr(['struct',Name|Args], Lifts, Lds0, St0) ->
    {Largs,Lds1,St1} = lift_rec_args(Args, Lifts, Lds0, St0),
    {['struct',Name|Largs],Lds1,St1};
lift_expr(['is-struct',E], Lifts, Lds0, St0) ->
    {Le,Lds1,St1} = lift_expr(E, Lifts, Lds0, St0),
    {['is-struct',Le],Lds1,St1};
lift_expr(['is-struct',E,Name], Lifts, Lds0, St0) ->
    {Le,Lds1,St1} = lift_expr(E, Lifts, Lds0, St0),
    {['is-struct',Le,Name],Lds1,St1};
lift_expr(['struct-field',E, Name,F], Lifts, Lds0, St0) ->
    {Le,Lds1,St1} = lift_expr(E, Lifts, Lds0, St0),
    {['struct-field',Le,Name,F],Lds1,St1};
lift_expr(['struct-update',E,Name|Args], Lifts, Lds0, St0) ->
    {Le,Lds1,St1} = lift_expr(E, Lifts, Lds0, St0),
    {Largs,Lds2,St2} = lift_rec_args(Args, Lifts, Lds1, St1),
    {['struct-update',Le,Name|Largs],Lds2,St2};
%% Function forms.
lift_expr([function,Name,Arity], Lifts, Lds, St) ->
    lift_function_ref(Name, Arity, Lifts, Lds, St);
lift_expr([function,_,_,_]=Func, _Lifts, Lds, St) ->
    {Func,Lds,St};
%% Core closure special forms.
lift_expr([lambda,Args|Body0], Lifts, Lds0, St0) ->
    {Body1,Lds1,St1} = lift_exprs(Body0, Lifts, Lds0, St0),
    {[lambda,Args|Body1],Lds1,St1};
lift_expr(['match-lambda'|Cls0], Lifts, Lds0, St0) ->
    {Cls1,Lds1,St1} = lift_cls(Cls0, Lifts, Lds0, St0),
    {['match-lambda'|Cls1],Lds1,St1};
lift_expr(['let',Vbs|Body], Lifts, Lds, St) ->
    lift_let(Vbs, Body, Lifts, Lds, St);
lift_expr(['let-function',Fbs|Body], Lifts, Lds, St) ->
    lift_let_function(Fbs, Body, Lifts, Lds, St);
lift_expr(['letrec-function',Fbs|Body], Lifts, Lds, St) ->
    lift_letrec_function(Fbs, Body, Lifts, Lds, St);
%% Core control special forms.
lift_expr([progn|Body0], Lifts, Lds0, St0) ->
    {Body1,Lds1,St1} = lift_exprs(Body0, Lifts, Lds0, St0),
    {[progn|Body1],Lds1,St1};
lift_expr(['if'|Body0], Lifts, Lds0, St0) ->
    {Body1,Lds1,St1} = lift_exprs(Body0, Lifts, Lds0, St0),
    {['if'|Body1],Lds1,St1};
lift_expr(['case',Expr|Cls], Lifts, Lds, St) ->
    lift_case(Expr, Cls, Lifts, Lds, St);
lift_expr(['maybe'|Body], Lifts, Lds, St) ->
    lift_maybe(Body, Lifts, Lds, St);
lift_expr(['catch'|Body0], Lifts, Lds0, St0) ->
    {Body1,Lds1,St1} = lift_exprs(Body0, Lifts, Lds0, St0),
    {['catch'|Body1],Lds1,St1};
lift_expr(['try'|Try], Lifts, Lds, St) ->
    lift_try(Try, Lifts, Lds, St);
lift_expr([funcall|Args0], Lifts, Lds0, St0) ->
    {Args1,Lds1,St1} = lift_exprs(Args0, Lifts, Lds0, St0),
    {[funcall|Args1],Lds1,St1};
%% List/binary comprehensions.
lift_expr(['lc',Qs,E], Lifts, Lds, St) ->
    lift_comp('lc', Qs, E, Lifts, Lds, St);
lift_expr(['list-comp',Qs,E], Lifts, Lds, St) ->
    lift_comp('list-comp', Qs, E, Lifts, Lds, St);
lift_expr(['bc',Qs,E], Lifts, Lds, St) ->
    lift_comp('bc', Qs, E, Lifts, Lds, St);
lift_expr(['binary-comp',Qs,E], Lifts, Lds, St) ->
    lift_comp('binary-comp', Qs, E, Lifts, Lds, St);
%% Finally the general cases.
lift_expr([call|Args0], Lifts, Lds0, St0) ->
    {Args1,Lds1,St1} = lift_exprs(Args0, Lifts, Lds0, St0),
    {[call|Args1],Lds1,St1};
lift_expr([Func|Args], Lifts, Lds, St) when is_atom(Func) ->
    lift_func_call(Func, Args, Lifts, Lds, St);
%% Everything else is a literal.
lift_expr(Lit, _Lifts, Lds, St) -> {Lit,Lds,St}.

lift_func_call(Name, Args0, Lifts, Lds0, St0) ->
    %% Most of the core data special forms can be handled here as well.
    {Args1,Lds1,St1} = lift_exprs(Args0, Lifts, Lds0, St0),
    Arity = length(Args1),
    Call = case lifted_function(Name, Arity, Lifts) of
               {yes,#lift{type=call,name=NewName,ivars=Ivars}} ->
                   [NewName | Args1 ++ Ivars];
               {yes,#lift{type=apply,name=NewName,ivars=Ivars}} ->
                   [funcall,NewName | Args1 ++ Ivars];
               no ->
                   [Name | Args1]
           end,
    {Call,Lds1,St1}.

lift_exprs(Exprs, Lifts, Lds, St) ->
    Fun = fun (E0, {Es,Lds0,St0}) ->
                  {E1,Lds1,St1} = lift_expr(E0, Lifts, Lds0, St0),
                  {[E1|Es],Lds1,St1}
          end,
    lists:foldr(Fun, {[],Lds,St}, Exprs).

lift_rec_fields([[F,V|Type]|Fs], Lifts, Lds0, St0) ->
    {Lv,Lds1,St1} = lift_expr(V, Lifts, Lds0, St0),
    {Lfs,Lds2,St2} = lift_rec_fields(Fs, Lifts, Lds1, St1),
    {[[F,Lv|Type]|Lfs],Lds2,St2};
lift_rec_fields([F|Fs], Lifts, Lds0, St0) ->
    {Lfs,Lds1,St1} = lift_rec_fields(Fs, Lifts, Lds0, St0),
    {[F|Lfs],Lds1,St1};
lift_rec_fields([], _Lifts, Lds, St) -> {[],Lds,St}.

lift_rec_args([F,V|As], Lifts, Lds0, St0) ->
    {Lv,Lds1,St1} = lift_expr(V, Lifts, Lds0, St0),
    {Las,Lds2,St2} = lift_rec_args(As, Lifts, Lds1, St1),
    {[F,Lv|Las],Lds2,St2};
lift_rec_args([], _Lifts, Lds, St) -> {[],Lds,St}.

%% lift_function_ref(Name, Arity, Lifts, LocalDefs, State) ->
%%     {Lifted,LocalDefs,State}.
%%  Check if [function,Name,Arity] needs to tbe lifted and whether
%%  Ivars force it to become a lambda.

lift_function_ref(Name, Arity, Lifts, Lds, St) ->
    Lfunc = case lifted_function(Name, Arity, Lifts) of
                {yes,#lift{name=NewName,ivars=Ivars}} ->
                    if length(Ivars) > 0 ->
                            Vars = new_vars(Arity),
                            [lambda,Vars,[funcall,?Q(NewName) | Vars ++ Ivars]];
                       true ->
                            [function,NewName,Arity]
                    end;
                no ->                   %When not lifted
                    [function,Name,Arity]
            end,
    {Lfunc,Lds,St}.

%% lift_let(VarBindings, Body, LiftedFuncs, LocalDefines, State) ->
%%     {Let,LocalDefines,State}.

lift_let(Vbs0, Body0, Lifts, Lds0, St0) ->
    {Vbs1,Lds1,St1} = lift_let_bindings(Vbs0, Lifts, Lds0, St0),
    {Body1,Lds2,St2} = lift_exprs(Body0, Lifts, Lds1, St1),
    {['let',Vbs1|Body1],Lds2,St2}.

lift_let_bindings(Vbs0, Lifts, Lds0, St0) ->
    Fun = fun ([Pat,['when'|_]=G,Expr0], {Ldsa,Sta}) ->
                  {Expr1,Ldsb,Stb} = lift_expr(Expr0, Lifts, Ldsa, Sta),
                  {[Pat,G,Expr1],{Ldsb,Stb}};
              ([Pat,Expr0], {Ldsa,Sta}) ->
                  {Expr1,Ldsb,Stb} = lift_expr(Expr0, Lifts, Ldsa, Sta),
                  {[Pat,Expr1],{Ldsb,Stb}}
          end,
    {Vbs1,{Lds1,St1}} = lists:mapfoldl(Fun, {Lds0,St0}, Vbs0),
    {Vbs1,Lds1,St1}.

%% lift_let_function(FuncBindings, Body, LiftedFuncs, LocalDefines, State) ->
%%     {LocalBody,LocalDefines,State}.
%%  We can check imported vars separately for each local function as
%%  they do not know of each other.

lift_let_function(Fbs0, Body, Lifts0, Lds0, St0) ->
    %% Build new name function binding and name transform data.
    Line = St0#cl.line,
    Nfun = fun ([Name,Def0], {Ls0,Lda,Sta}) ->
                   Ar = func_arity(Def0),
                   %% Get imported variables and append them in definition.
                   Ivs  = ivars_expr(Def0, [], []),
                   Def1 = append_ivars(Def0, Ivs),
                   {Def2,Ldb,Stb} = lift_expr(Def1, Lifts0, Lda, Sta),
                   {New,Ls1,Stc} = lift_function(Name, Ar, call, Ivs, Ls0, Stb),
                   {{New,Def2,Line},{Ls1,Ldb,Stc}}
           end,
    %% Transform calls in the body.
    {Fbs1,{Lifts1,Lds1,St1}} = lists:mapfoldl(Nfun, {Lifts0,Lds0,St0}, Fbs0),
    %% io:format("lf ~p\n   ~p\b", [Fbs1,Lds1]),
    Lds2 = Fbs1 ++ Lds1,
    lift_expr([progn | Body], Lifts1, Lds2, St1).

%% lift_letrec_function(FuncBindings, Body, LiftedFuncs, LocalDefines, State) ->
%%     {LocalBody,LocalDefines,State}.
%%  We cheat a bit when checking imported vars, we just take the union
%%  of the variables from all the local functions and pass them to all
%%  functions.

lift_letrec_function(Fbs0, Body, Lifts0, Lds0, St0) ->
    %% Get all the imported variables.
    Ifun = fun ([_,Def], Ivs) -> ivars_expr(Def, [], Ivs) end,
    Ivars = lists:foldl(Ifun, [], Fbs0),
    %% io:format("lrf ~p\n", [Ivars]),
    %% Lift the local defined functions and get their new names and data.
    Lfun = fun ([Name,Def], {Lsa,Sta}) ->
                   Ar = func_arity(Def),
                   {New,Lsb,Stb} = lift_function(Name, Ar, call, Ivars, Lsa, Sta),
                   {{New,Def},{Lsb,Stb}}
           end,
    {Fbs1,{Lifts1,St1}} = lists:mapfoldl(Lfun, {Lifts0,St0}, Fbs0),
    %% Lift the local functions with the joint lifted local functions.
    Line = St0#cl.line,
    Efun = fun ({New,Def0}, {Lda,Sta}) ->
                   Def1 = append_ivars(Def0, Ivars),
                   {Def2,Ldb,Stb} = lift_expr(Def1, Lifts1, Lda, Sta),
                   {{New,Def2,Line}, {Ldb,Stb}}
           end,
    {Fbs2,{Lds1,St2}} = lists:mapfoldl(Efun, {Lds0,St1}, Fbs1),
    Lds2 = Fbs2 ++ Lds1,
    lift_expr([progn | Body], Lifts1, Lds2, St2).

%% lift_function(Name, Arity, LiftType, Ivars, Lifts, State) ->
%%     {NewName,Lifts,State}.
%% lifted_function(Name, Arity, Lifts) -> {yes,Lift} | no.
%%  Lift a function, and check whether a function has been liftes and
%%  return irs lift data.

lift_function(Name, Arity, Type, Ivars, Lifts0, St0) ->
    {NewName,St1} = new_local_fun_name(Name, Arity, St0),
    Lift = #lift{type=Type,name=NewName,ivars=Ivars},
    Lifts1 = orddict:store({Name,Arity}, Lift, Lifts0),
    {NewName,Lifts1,St1}.
    
lifted_function(Name, Arity, Lifts) ->
    case orddict:find({Name,Arity}, Lifts) of
        {ok,Value} -> {yes,Value};
        error -> no
    end.
                       
func_arity([lambda,Args|_]) -> length(Args);
func_arity(['match-lambda',[Pats|_]|_]) ->
    length(Pats).

append_ivars([lambda,Args|Body], Ivars) ->
    [lambda,Args ++ Ivars|Body];
append_ivars(['match-lambda'|Cls0], Ivars) ->
    Fun = fun ([Pats|Body]) -> [Pats ++ Ivars|Body] end, 
    Cls1 = lists:map(Fun, Cls0),
    ['match-lambda'|Cls1].

lift_cls(Cls, Lifts, Lds, St) ->
    Fun = fun ([Pats,['when'|_]=G|Body0], {Cls0,Lds0,St0}) ->
                  {Body1,Lds1,St1} = lift_exprs(Body0, Lifts, Lds0, St0),
                  {[[Pats,G|Body1]|Cls0],Lds1,St1};
              ([Pats|Body0], {Cls0,Lds0,St0}) ->
                  {Body1,Lds1,St1} = lift_exprs(Body0, Lifts, Lds0, St0),
                  {[[Pats|Body1]|Cls0],Lds1,St1}
          end,
    lists:foldr(Fun, {[],Lds,St}, Cls).         %From the right!

lift_case(Expr0, Cls0, Lifts, Lds0, St0) ->
    {Expr1,Lds1,St1} = lift_expr(Expr0, Lifts, Lds0, St0),
    {Cls1,Lds2,St2} = lift_cls(Cls0, Lifts, Lds1, St1),
    {['case',Expr1|Cls1],Lds2,St2}.

%% lift_maybe(Body, LifteFuncs, LocalDefines, State) ->
%%     {Maybe,LocalDefines,State}.
%%  We must also explicitly handle let and explicitly lift their
%%  bodies as maybe bodies.

lift_maybe(Body0, Lifts, Lds0, St0) ->
    {Body1,Lds1,St1} = lift_maybe_body(Body0, Lifts, Lds0, St0),
    {['maybe'|Body1],Lds1,St1}.

lift_maybe_body([['?=',Pat,Expr0]|Mes0], Lifts, Lds0, St0) ->
    {Expr1,Lds1,St1} = lift_expr(Expr0, Lifts, Lds0, St0),
    {Mes1,Lds2,St2} = lift_maybe_body(Mes0, Lifts, Lds1, St1),
    {[['?=',Pat,Expr1]|Mes1],Lds2,St2};
lift_maybe_body([['let',Vbs|Body]|Mes0], Lifts, Lds0, St0) ->
    {Let,Lds1,St1} = lift_maybe_let(Vbs, Body, Lifts, Lds0, St0),
    {Mes1,Lds2,St2} = lift_maybe_body(Mes0, Lifts, Lds1, St1),
    {[Let|Mes1],Lds2,St2};
lift_maybe_body(['else'|Cls0], Lifts, Lds0, St0) ->
    {Cls1,Lds1,St1} = lift_cls(Cls0, Lifts, Lds0, St0),
    {['else'|Cls1],Lds1,St1};
lift_maybe_body([Expr0|Mes0], Lifts, Lds0, St0) ->
    {Expr1,Lds1,St1} = lift_expr(Expr0, Lifts, Lds0, St0),
    {Mes1,Lds2,St2} = lift_maybe_body(Mes0, Lifts, Lds1, St1),
    {[Expr1|Mes1],Lds2,St2};
lift_maybe_body([], _Lift, Lds, St) ->
    {[],Lds,St}.

lift_maybe_let(Vbs0, Body0, Lifts, Lds0, St0) ->
    {Vbs1,Lds1,St1} = lift_let_bindings(Vbs0, Lifts, Lds0, St0),
    {Body1,Lds2,St2} = lift_maybe_body(Body0, Lifts, Lds1, St1),
    {['let',Vbs1|Body1],Lds2,St2}.

%% lift_try(TryBody, LiftedFuncs, LocalDefs, State) ->
%%     {TryBody,LocalDefs,State}.
%%  Step down the try body lifting the local functions.

lift_try(Try0, Lifts, Lds0, St0) ->
    Fun = fun (T0, {L0,S0}) ->
                  {T1,L1,S1} = lift_try_1(T0, Lifts, L0, S0),
                  {T1,{L1,S1}}
          end,
    {Try1,{Lds1,St1}} = lists:mapfoldl(Fun, {Lds0,St0}, Try0),
    {['try'|Try1],Lds1,St1}.

lift_try_1(['case'|Case0], Lifts, Lds0, St0) ->
    {Case1,Lds1,St1} = lift_cls(Case0, Lifts, Lds0, St0),
    {['case'|Case1],Lds1,St1};
lift_try_1(['catch'|Catch0], Lifts, Lds0, St0) ->
    {Catch1,Lds1,St1} = lift_cls(Catch0, Lifts, Lds0, St0),
    {['catch'|Catch1],Lds1,St1};
lift_try_1(['after'|After0], Lifts, Lds0, St0) ->
    {After1,Lds1,St1} = lift_exprs(After0, Lifts, Lds0, St0),
    {['after'|After1],Lds1,St1};
lift_try_1(E, Lifts, Lds, St) ->                %The try expression.
    lift_expr(E, Lifts, Lds, St).

%% lift_comp(Commprehension, Qualifiers, Expr, LiftedFuncs, LocalDefs, State) ->
%%     {Comprehension,LocalDefs,State}.
%%  Lift comprehensions. Only the expressions in the comprehensions
%%  need to be lifted, no guards or patterns.

lift_comp(Comp, Qs0, E0, Lifts, Lds0, St0) ->
    %% io:format("lc ~p\n", [[Comp,Qs0,E0]]),
    {Qs1,Lds1,St1} = lift_comp_quals(Qs0, Lifts, Lds0, St0),
    {E1,Lds2,St2} = lift_expr(E0, Lifts, Lds1, St1),
    {[Comp,Qs1,E1],Lds2,St2}.

lift_comp_quals(Qs, Lifts, Lds, St) ->
    lists:foldr(fun (Q0, {Qs0,Lds0,St0}) ->
                        {Q1,Lds1,St1} = lift_comp_qual(Q0, Lifts, Lds0, St0),
                        {[Q1|Qs0],Lds1,St1}
                end, {[],Lds,St}, Qs).

lift_comp_qual(['<-',Pat,E0], Lifts, Lds0, St0) ->
    {E1,Lds1,St1} = lift_expr(E0, Lifts, Lds0, St0),
    {['<-',Pat,E1],Lds1,St1};
lift_comp_qual(['<-',Pat,G,E0], Lifts, Lds0, St0) ->
    {E1,Lds1,St1} = lift_expr(E0, Lifts, Lds0, St0),
    {['<-',Pat,G,E1],Lds1,St1};
lift_comp_qual(['<=',Pat,E0], Lifts, Lds0, St0) ->
    {E1,Lds1,St1} = lift_expr(E0, Lifts, Lds0, St0),
    {['<=',Pat,E1],Lds1,St1};
lift_comp_qual(['<=',Pat,G,E0], Lifts, Lds0, St0) ->
    {E1,Lds1,St1} = lift_expr(E0, Lifts, Lds0, St0),
    {['<=',Pat,G,E1],Lds1,St1};
lift_comp_qual(Test, Lifts, Lds, St) ->
    lift_expr(Test, Lifts, Lds, St).

%% new_local_fun_name(Name, Arity, State) -> {FunName,State}.
%%  Create a name for a local function. The name has a similar basic
%%  format as those created in Core Erlang, though not overlapping.

new_local_fun_name(Local, Lar, #cl{func=Func,arity=Far,fc=C}=St) ->
    Name = lists:concat(["-lfe-",Func,"/",Far,
                         "-local-",Local,"/",Lar,
                         "-",C,"-"]),
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
%% Record forms.
ivars_expr(['record',_|Args], Kvars, Ivars) ->
    ivars_record_args(Args, Kvars, Ivars);
%% make-record has been deprecated but we sill accept it for now.
ivars_expr(['make-record',_|Args], Kvars, Ivars) ->
    ivars_record_args(Args, Kvars, Ivars);
ivars_expr(['is-record',E,_], Kvars, Ivars) ->
    ivars_expr(E, Kvars, Ivars);
ivars_expr(['record-index',_,_], _, Ivars) -> Ivars;
ivars_expr(['record-field',E,_,_], Kvars, Ivars) ->
    ivars_expr(E, Kvars, Ivars);
ivars_expr(['record-update',E,_|Args], Kvars, Ivars0) ->
    Ivars1 = ivars_expr(E, Kvars, Ivars0),
    ivars_record_args(Args, Kvars, Ivars1);
%% Struct special forms.
ivars_expr(['struct',_Name|Args], Kvars, Ivars) ->
    ivars_struct_args(Args, Kvars, Ivars);
ivars_expr(['is-struct',E], Kvars, Ivars) ->
    ivars_expr(E, Kvars, Ivars);
ivars_expr(['is-struct',E,_], Kvars, Ivars) ->
    ivars_expr(E, Kvars, Ivars);
ivars_expr(['struct-field',E,_Name,_Field], Kvars, Ivars) ->
    ivars_expr(E, Kvars, Ivars);
ivars_expr(['struct-update',E,_Name|Args], Kvars, Ivars0) ->
    Ivars1 = ivars_expr(E, Kvars, Ivars0),
    ivars_struct_args(Args, Kvars, Ivars1);
%% Function forms.
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
    ivars_receive_cls(Cls, Kvars, Ivars);
ivars_expr(['catch'|Body], Kvars, Ivars) ->
    ivars_exprs(Body, Kvars, Ivars);
ivars_expr(['try'|Body], Kvars, Ivars) ->
    ivars_try(Body, Kvars, Ivars);
ivars_expr([funcall|Args], Kvars, Ivars) ->
    ivars_exprs(Args, Kvars, Ivars);
ivars_expr([call|Args], Kvars, Ivars) ->
    ivars_exprs(Args, Kvars, Ivars);
%% List/binary comprehensions.
ivars_expr(['lc',Qs,E], Kvars, Ivars) ->
    ivars_comp(Qs, E, Kvars, Ivars);
ivars_expr(['list-comp',Qs,E], Kvars, Ivars) ->
    ivars_comp(Qs, E, Kvars, Ivars);
ivars_expr(['bc',Qs,E], Kvars, Ivars) ->
    ivars_comp(Qs, E, Kvars, Ivars);
ivars_expr(['binary-comp',Qs,E], Kvars, Ivars) ->
    ivars_comp(Qs, E, Kvars, Ivars);
%% General cases.
ivars_expr([Fun|Args], Kvars, Ivars) when is_atom(Fun) ->
    ivars_exprs(Args, Kvars, Ivars);
ivars_expr(Var, Kvars, Ivars) when is_atom(Var) ->
    case ordsets:is_element(Var, Kvars) of
        true -> Ivars;
        false -> ordsets:add_element(Var, Ivars)
    end;
%% Everything else is a literal.
ivars_expr(_Lit, _Kvars, Ivars) -> Ivars.

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

%% ivars_record_args(Args, Kvars, Ivars) -> Ivars.
%% ivars_struct_args(Args, Kvars, Ivars) -> Ivars.
%%  Get the Ivars form record/struct argument lists.

ivars_record_args([_F,V|As], Kvars, Ivars0) ->
    Ivars1 = ivars_expr(V, Kvars, Ivars0),
    ivars_record_args(As, Kvars, Ivars1);
ivars_record_args([], _, Ivars) -> Ivars.

ivars_struct_args([_F,V|As], Kvars, Ivars0) ->
    Ivars1 = ivars_expr(V, Kvars, Ivars0),
    ivars_struct_args(As, Kvars, Ivars1);
ivars_struct_args([], _, Ivars) -> Ivars.

%% ivars_let(VariableBindings, Body, Kvars, Ivars) -> Ivars.
%%  Get Ivars from a let form.

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

%% ivars_let_function(FunctionBindings, Body, Kvars, Ivars) -> Ivars.
%%  Get the Ivars from a let-function/letrec-function form.

ivars_let_function(Fbs, Body, Kvars, Ivars0) ->
    Fun = fun ([_,Def], Ivs) -> ivars_expr(Def, Kvars, Ivs) end,
    Ivars1 = lists:foldl(Fun, Ivars0, Fbs),
    ivars_exprs(Body, Kvars, Ivars1).

ivars_fun_cls(Cls, Kvars, Ivars) ->
    Fun = fun (Cl, Ivs) -> ivars_fun_cl(Cl, Kvars, Ivs) end,
    lists:foldl(Fun, Ivars, Cls).

ivars_fun_cl([Pats|Body], Kvars, Ivars) ->
    ivars_clause([[list|Pats]|Body], Kvars, Ivars).

ivars_cls(Cls, Kvars, Ivars) ->
    Fun = fun (Cl, Ivs) -> ivars_clause(Cl, Kvars, Ivs) end,
    lists:foldl(Fun, Ivars, Cls).

ivars_receive_cls(Cls, Kvars, Ivars) ->
    Fun = fun (['after'|Body], Ivs) -> ivars_exprs(Body, Kvars, Ivs);
              (Cl, Ivs) -> ivars_clause(Cl, Kvars, Ivs)
          end,
    lists:foldl(Fun, Ivars, Cls).

%% ivars_clause(Clause, Kvars, Ivars) -> Ivars.
%%  Get the Ivars from a function/case/receive clause.

ivars_clause([Pat,['when'|G]|Body], Kvars0, Ivars0) ->
    Pvs = ivars_pat(Pat),
    Kvars1 = ordsets:union(Pvs, Kvars0),
    Ivars1 = ivars_exprs(G, Kvars1, Ivars0),
    ivars_exprs(Body, Kvars1, Ivars1);
ivars_clause([Pat|Body], Kvars0, Ivars) ->
    Pvs = ivars_pat(Pat),
    Kvars1 = ordsets:union(Pvs, Kvars0),
    ivars_exprs(Body, Kvars1, Ivars).

%% ivars_try(TryBody, KnownVars, ImportedVars) -> ImportedVars.
%%  Get the Ivars from a try. Step down the try body doing each
%%  section separately.

ivars_try(Try, Kvars, Ivars) ->
    lists:foldl(fun (T, Ivs) -> ivars_try_1(T, Kvars, Ivs) end,
                Ivars, Try).

ivars_try_1(['case'|Case], Kvars, Ivars) ->
    ivars_cls(Case, Kvars, Ivars);
ivars_try_1(['catch'|Catch], Kvars, Ivars) ->
    ivars_cls(Catch, Kvars, Ivars);
ivars_try_1(['after'|After], Kvars, Ivars) ->
    ivars_exprs(After, Kvars, Ivars);
ivars_try_1(E, Kvars, Ivars) ->                 %The try expression.
    ivars_expr(E, Kvars, Ivars).

%% ivars_comp(Qualifiers, Expr, KnownVars, ImportedVars) -> ImportedVars,
%%  Get the Ivars from a list/binary comprehension.

ivars_comp(Qs, E, Kvars0, Ivars0) ->
    {Kvars1,Ivars1} = ivars_comp_quals(Qs, Kvars0, Ivars0),
    ivars_expr(E, Kvars1, Ivars1).

ivars_comp_quals(Qs, Kvars, Ivars) ->
    lists:foldl(fun (Q, {Kvars0,Ivars0}) ->
                        {Kvars1,Ivars1} = ivars_comp_qual(Q, Kvars0, Ivars0),
                        {Kvars1,Ivars1}
                end, {Kvars,Ivars}, Qs).

ivars_comp_qual(['<-',Pat,Gen], Kvars, Ivars) ->
    ivars_comp_qual(Pat, [], Gen, Kvars, Ivars);
ivars_comp_qual(['<-',Pat,['when'|G],Gen], Kvars, Ivars) ->
    ivars_comp_qual(Pat, G, Gen, Kvars, Ivars);
ivars_comp_qual(['<=',Pat,Gen], Kvars, Ivars) ->
    ivars_comp_qual(Pat, [], Gen, Kvars, Ivars);
ivars_comp_qual(['<=',Pat,['when'|G],Gen], Kvars, Ivars) ->
    ivars_comp_qual(Pat, G, Gen, Kvars, Ivars);
ivars_comp_qual(Test, Kvars, Ivars) ->
    {Kvars,ivars_expr(Test, Kvars, Ivars)}.

ivars_comp_qual(Pat, G, Gen, Kvars0, Ivars0) ->
    Pvs = ivars_pat(Pat),
    Kvars1 = ordsets:union(Pvs, Kvars0),
    Ivars1 = ivars_exprs(G, Kvars1, Ivars0),
    Ivars2 = ivars_expr(Gen, Kvars1, Ivars1),
    {Kvars1,Ivars2}.

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

%% mapfoldl2(Fun, A0, B0, [E0|Es0]) ->
%%     {E1,A1,B1} = Fun(E0, A0, B0),
%%     {Es1,A2,B2} = mapfoldl2(Fun, A1, B1, Es0),
%%     {[E1|Es1],A2,B2};
%% mapfoldl2(_, A, B, []) -> {[],A,B}.

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
