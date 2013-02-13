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

%% File    : lfe_macro.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang macro expander.

%% Expand macros and record definitions (into macros), also handles
%% quasiquote/backquote in an R6RS compatible way.

-module(lfe_macro).

%% These work on individual expressions.
-export([expand_expr/2,expand_expr_1/2,expand_expr_all/2]).

%% These work on list of forms in "file format".
-export([expand_forms/2,macro_forms/2,format_error/1]).

-export([mbe_syntax_rules_proc/4,mbe_syntax_rules_proc/5,
	 mbe_match_pat/3,mbe_get_bindings/3,mbe_expand_pattern/3]).


%% -compile([export_all]).

-import(lfe_lib, [new_env/0,add_fbinding/4,is_fbound/3,
		  add_mbinding/3,is_mbound/2,get_mbinding/2,
		  is_symb_list/1,is_proper_list/1]).

-import(lists, [any/2,all/2,map/2,foldl/3,foldr/3,mapfoldl/3,
		reverse/1,reverse/2,member/2,concat/1]).
-import(orddict, [find/2,store/3]).
-import(ordsets, [add_element/2,is_element/2]).

-define(Q(E), [quote,E]).			%We do a lot of quoting!
-define(BQ(E), [backquote,E]).
-define(UQ(E), [unquote,E]).
-define(UQ_S(E), ['unquote-splicing',E]).

-record(mac, {expand=true,			%Expand everything
	      vc=0,				%Variable counter
	      fc=0,				%Function counter
	      errors=[],			%Errors
	      warnings=[]			%Warnings
	     }).

%% Errors
format_error({bad_form,Type}) ->
    lfe_io:format1("bad form: ~w", [Type]);
format_error({expand_macro,Call,_}) ->
    %% Can be very big so only print limited depth.
    lfe_io:format1("error expanding ~W", [Call,7]).

%% expand_expr(Form, Env) -> {yes,Exp} | no.
%% expand_expr_1(Form, Env) -> {yes,Exp} | no.
%%  User functions for testing macro expansions, either one expansion
%%  or as far as it can go.

expand_expr_1([Name|_]=Call, Env) when is_atom(Name) ->
    St = #mac{expand=false},			%Default state
    case exp_macro(Call, Env, St) of
	{yes,Exp,_} -> {yes,Exp};
	no -> no
    end;
expand_expr_1(_, _) -> no.

expand_expr([Name|_]=Call, Env) when is_atom(Name) ->
    St0 = #mac{expand=false},			%Default state
    case exp_macro(Call, Env, St0) of
	{yes,Exp0,St1} ->
	    {Exp1,_} = expand_expr_loop(Exp0, Env, St1),
	    {yes,Exp1};
	no -> no
    end;
expand_expr(_, _) -> no.

expand_expr_loop([Name|_]=Call, Env, St0) when is_atom(Name) ->
    case exp_macro(Call, Env, St0) of
	{yes,Exp,St1} -> expand_expr_loop(Exp, Env, St1);
	no -> {Call,St0}
    end;
expand_expr_loop(E, _, St) -> {E,St}.

%% expand_expr_all(From, Env) -> Exp.
%%  Expand all the macros in an expression.

expand_expr_all(F, Env) ->
    {Ef,_} = expand(F, Env, #mac{expand=true}),
    Ef.

%% expand_forms(FileForms, Env) -> {FileForms,Env}.
%%  Expand forms in "file format", {Form,LineNumber}.

expand_forms(Fs, Env) ->
    do_forms(Fs, Env, true).

%% macro_forms(FileForms, Env) -> {FileForms,Env}.
%%  Collect, and remove, all macro definitions in a list of forms. All
%%  top level macro calls are also expanded and any new macro
%%  definitions are collected.

macro_forms(Fs, Env) ->
    do_forms(Fs, Env, false).

do_forms(Fs0, Env0, Expand) ->
    {Fs1,Env1,St} = pass(Fs0, Env0, #mac{expand=Expand}),
    case St#mac.errors of
	[] -> {ok,Fs1,Env1,St#mac.warnings};	%No errors
	Es -> {error,Es,St#mac.warnings}
    end.

%% pass(FileForms, Env, State) -> {FileForms,Env,State}.
%%  Pass over a list of fileforms, {Form,Line}, collecting and
%%  removing all macro defintions. All forms must be expanded at
%%  top-level to check form, but all can be expanded to full depth.
%%  Nesting of forms by progn is preserved.

pass([{['progn'|Pfs0],L}|Fs0], Env0, St0) ->
    {Pfs1,Env1,St1} = pass_progn(Pfs0, L, Env0, St0),
    {Fs1,Env2,St2} = pass(Fs0, Env1, St1),
    {[{['progn'|Pfs1],L}|Fs1],Env2,St2};
pass([{['eval-when-compile'|Ewcs0],L}|Fs0], Env0, St0) ->
    {Ecws1,Env1,St1} = pass_ewc(Ewcs0, L, Env0, St0),
    {Fs1,Env2,St2} = pass(Fs0, Env1, St1),
    {[{['progn'|Ecws1],L}|Fs1],Env2,St2};
pass([{['define-macro'|Def]=F,L}|Fs0], Env0, St0) ->
    case pass_define_macro(Def, L, Env0, St0) of
	{yes,Env1,St1} -> pass(Fs0, Env1, St1);
	{no,St1} ->
	    %% Ignore it and pass it on to generate error later.
	    {Fs1,Env1,St2} = pass(Fs0, Env0, St1),
	    {[{F,L}|Fs1],Env1,St2}
    end;
pass([{F,L}|Fs0], Env0, St0) ->
    %% First expand enough to test top form, else maybe expand all.
    case pass_expand_expr(F, L, Env0, St0, St0#mac.expand) of
	{yes,Exp,St1} ->			%Top form expanded
	    pass([{Exp,L}|Fs0], Env0, St1);
	{no,F1,St1} -> 				%Expanded all if flag set
	    {Fs1,Env1,St2} = pass(Fs0, Env0, St1),
	    {[{F1,L}|Fs1],Env1,St2}
    end;
pass([], Env, St) -> {[],Env,St}.

%% pass_progn(Forms, Line, Env, State) -> {Forms,Env,State}.
%%  Pass over a list of forms collecting and removing all macro
%%  defintions. All forms must be expanded at top-level to check form,
%%  but all can be expanded to full depth. Nesting of forms by progn
%%  is preserved.

pass_progn([['progn'|Pfs0]|Fs0], L, Env0, St0) ->
    {Pfs1,Env1,St1} = pass_progn(Pfs0, L, Env0, St0),
    {Fs1,Env2,St2} = pass_progn(Fs0, L, Env1, St1),
    {[['progn'|Pfs1]|Fs1],Env2,St2};
pass_progn([['eval-when-compile'|Ewcs0]|Fs0], L, Env0, St0) ->
    {Ecws1,Env1,St1} = pass_ewc(Ewcs0, L, Env0, St0),
    {Fs1,Env2,St2} = pass_progn(Fs0, L, Env1, St1),
    {[['progn'|Ecws1]|Fs1],Env2,St2};
pass_progn([['define-macro'|Def]=F|Fs0], L, Env0, St0) ->
    case pass_define_macro(Def, L, Env0, St0) of
	{yes,Env1,St1} -> pass_progn(Fs0, L, Env1, St1);
	{no,St1} ->
	    %% Ignore it and pass it on to generate error later.
	    {Fs1,Env1,St2} = pass_progn(Fs0, L, Env0, St1),
	    {[F|Fs1],Env1,St2}
    end;
pass_progn([F|Fs0], L, Env0, St0) ->
    %% First expand enough to test top form, if so process again.
    case pass_expand_expr(F, L, Env0, St0, St0#mac.expand) of
	{yes,Exp,St1} ->			%Top form expanded
	    pass_progn([Exp|Fs0], L, Env0, St1);
	{no,F1,St1} -> 				%Expanded all if flag set
	    {Fs1,Env1,St2} = pass_progn(Fs0, L, Env0, St1),
	    {[F1|Fs1],Env1,St2}
    end;
pass_progn([], _, Env, St) -> {[],Env,St}.

%% pass_ewc(Forms, Line, Env, State) -> {Forms,Env,State}.
%%  Pass over a list of forms collecting and removing all function
%%  defintions. All forms must be expanded at top-level to check
%%  form. Nesting of forms by progn is preserved. All functions are
%%  put into one letrec structure in environment so they are mutally
%%  recursive, while unrecognised forms are returned. They can call
%%  functions in previously defined eval-when-compile's.

pass_ewc(Fs0, L, Env0, St0) ->
    {Fs1,Fbs,Env1,St1} = pass_ewc(Fs0, L, [], Env0, St0),
    Env2 = lfe_eval:make_letrec_env(Fbs, Env1),
    {Fs1,Env2,St1}.

pass_ewc([['progn'|Pfs0]|Fs0], L, Fbs0, Env0, St0) ->
    {Pfs1,Fbs1,Env1,St1} = pass_ewc(Pfs0, L, Fbs0, Env0, St0),
    {Fs1,Fbs2,Env2,St2} = pass_ewc(Fs0, L, Fbs1, Env1, St1),
    {[['progn'|Pfs1]|Fs1],Fbs2,Env2,St2};
pass_ewc([['eval-when-compile'|Ewcs0]|Fs0], L, Fbs0, Env0, St0) ->
    {Ecws1,Fbs1,Env1,St1} = pass_ewc(Ewcs0, L, Fbs0, Env0, St0),
    {Fs1,Fbs2,Env2,St2} = pass_ewc(Fs0, L, Fbs1, Env1, St1),
    {[['progn'|Ecws1]|Fs1],Fbs2,Env2,St2};
%% Do we want macros here???
%% pass_ewc([['define-macro'|Def]=F|Fs0], L, Fbs0, Env0, St0) ->
%%     case pass_define_macro(Def, L, Env0, St0) of
%% 	{yes,Env1,St1} -> pass_ewc(Fs0, L, Fbs0, Env1, St1);
%% 	{no,St1} ->
%% 	    %% Ignore it and pass it on to generate error later.
%% 	    {Fs1,Fbs1,Env1,St2} = pass_ewc(Fs0, L, Fbs0, Env0, St1),
%% 	    {[F|Fs1],Fbs1,Env1,St2}
%%     end;
pass_ewc([['define-function',Name,Def]=F|Fs0], L, Fbs0, Env0, St0) ->
    case func_arity(Def) of
	{yes,Ar} ->				%Definition not too bad
	    Fb = {Name,Ar,Def},
	    %% Env1 = lfe_eval:add_expr_func(Name, Ar, Def, Env0),
	    pass_ewc(Fs0, L, [Fb|Fbs0], Env0, St0);
	no ->					%Definition really bad
	    %% Ignore it and pass it on to generate error later.
	    {Fs1,Fbs1,Env1,St1} = pass_ewc(Fs0, L, Fbs0, Env0, St0),
	    {[F|Fs1],Fbs1,Env1,St1}
    end;
pass_ewc([F|Fs0], L, Fbs0, Env0, St0) ->
    %% First expand enough to test top form, if so process again.
    case pass_expand_expr(F, L, Env0, St0, false) of
	{yes,Exp,St1} ->			%Top form expanded
	    pass_ewc([Exp|Fs0], L, Fbs0, Env0, St1);
	{no,F1,St1} -> 				%Not expanded
	    {Fs1,Fbs1,Env1,St2} = pass_ewc(Fs0, L, Fbs0, Env0, St1),
	    {[F1|Fs1],Fbs1,Env1,St2}
    end;
pass_ewc([], _, Fbs, Env, St) -> {[],Fbs,Env,St}.

func_arity([lambda,Args|_]) ->
    case is_symb_list(Args) of
	true -> {yes,length(Args)};
	false -> no
    end;
func_arity(['match-lambda',[Pat|_]|_]) ->
    case is_proper_list(Pat) of
	true -> {yes,length(Pat)};
	false -> no
    end;
func_arity(_) -> no.

%% pass_expand_expr(Expr, Line, Env, State, ExpandFlag) ->
%%     {yes,Exp,State} | {no,State}.
%% Try to macro expand Expr, catch errors and return them in State.
%% Only try to expand list expressions.

pass_expand_expr([_|_]=E0, L, Env, St0, Expand) ->
    try
	case exp_macro(E0, Env, St0) of
	    {yes,_,_}=Yes -> Yes;
	    no when Expand ->			%Expand all if flag set.
		{E1,St1} = expand(E0, Env, St0),
		{no,E1,St1};
	    no -> {no,E0,St0}
	end
    catch
	_:Error -> {no,E0,add_error(L, Error, St0)}
    end;
pass_expand_expr(E, _, _, St, _) -> {no,E,St}.

%% add_error(Line, Error, State) -> State.
%% add_warning(Line, Warning, State) -> State.

add_error(L, E, St) ->
    St#mac{errors=St#mac.errors ++ [{L,?MODULE,E}]}.

%% add_warning(L, W, St) ->
%%     St#mac{warnings=St#mac.warnings ++ [{L,?MODULE,W}]}.

%% pass_define_macro([Name,Def], Line, Env, State) ->
%%     {yes,Env,State} | {no,State}.
%% Add the macro definition to the environment. We do a small format
%% check.

pass_define_macro([Name,Def], L, Env, St) ->
    case Def of
	['lambda'|_] -> {yes,add_mbinding(Name, Def, Env),St};
	['match-lambda'|_] -> {yes,add_mbinding(Name, Def, Env),St};
	_ -> {no,add_error(L, {bad_form,macro}, St)}
    end.

%% expand(Form, Env, State) -> {Form,State}.
%%  Completely expand a form using expansions in Env and pre-defined
%%  macros.  N.B. builtin core forms cannot be overidden and are
%%  handled here first. The core forms also are particular about how
%%  their bodies are to be expanded.

%% Known Core forms which cannot be overidden.
expand([quote,_]=Q, _, St) -> {Q,St};
expand([cons,H0,T0], Env, St0) ->
    {H1,St1} = expand(H0, Env, St0),
    {T1,St2} = expand(T0, Env, St1),
    {[cons,H1,T1],St2};
expand([car,E0], Env, St0) ->			%Catch these to prevent
    {E1,St1} = expand(E0, Env, St0),		%redefining them
    {[car,E1],St1};
expand([cdr,E0], Env, St0) ->
    {E1,St1} = expand(E0, Env, St0),
    {[cdr,E1],St1};
expand([list|As0], Env, St0) ->
    {As1,St1} = expand_tail(As0, Env, St0),
    {[list|As1],St1};
expand([tuple|As0], Env, St0) ->
    {As1,St1} = expand_tail(As0, Env, St0),
    {[tuple|As1],St1};
expand([binary|As0], Env, St0) ->
    {As1,St1} = expand_tail(As0, Env, St0),
    {[binary|As1],St1};
expand(['lambda',Head|B0], Env, St0) ->
    {B1,St1} = expand_tail(B0, Env, St0),
    {['lambda',Head|B1],St1};
expand(['match-lambda'|B0], Env, St0) ->
    {B1,St1} = expand_ml_clauses(B0, Env, St0),
    {['match-lambda'|B1],St1};
expand(['let',Vbs0|B0], Env, St0) ->
    %% We don't really have to syntax check very strongly here so we
    %% can use normal clause expansion. Lint will catch errors.
    {Vbs1,St1} = expand_clauses(Vbs0, Env, St0),
    {B1,St2} = expand_tail(B0, Env, St1),
    {['let',Vbs1|B1],St2};
expand(['let-function',Fbs|B], Env, St) ->
    expand_let_function(Fbs, B, Env, St);
expand(['letrec-function',Fbs|B], Env, St) ->
    expand_letrec_function(Fbs, B, Env, St);
expand(['let-macro',Mbs|B], Env, St) ->
    expand_let_macro(Mbs, B, Env, St);
expand(['progn'|B0], Env, St0)->
    {B1,St1} = expand_tail(B0, Env, St0),
    {['progn'|B1],St1};
expand(['if'|B0], Env, St0) ->
    {B1,St1} = expand_tail(B0, Env, St0),
    {['if'|B1],St1};
expand(['case',E0|Cls0], Env, St0) ->
    {E1,St1} = expand(E0, Env, St0),
    {Cls1,St2} = expand_clauses(Cls0, Env, St1),
    {['case',E1|Cls1],St2};
expand(['receive'|Cls0], Env, St0) ->
    {Cls1,St1} = expand_clauses(Cls0, Env, St0),
    {['receive'|Cls1],St1};
expand(['catch'|B0], Env, St0) ->
    {B1,St1} = expand_tail(B0, Env, St0),
    {['catch'|B1],St1};
expand(['try',E|B], Env, St) ->
    expand_try(E, B, Env, St);
expand(['funcall'|As0], Env, St0) ->
    {As1,St1} = expand_tail(As0, Env, St0),
    {['funcall'|As1],St1};
expand(['call'|As0], Env, St0) ->
    {As1,St1} = expand_tail(As0, Env, St0),
    {['call'|As1],St1};
expand(['define-function',Head|B0], Env, St0) ->
    %% Needs to be handled specially to protect Head.
    {B1,St1} = expand_tail(B0, Env, St0),
    {['define-function',Head|B1],St1};
%% Now the case where we can have macros.
expand([Fun|_]=Call, Env, St0) when is_atom(Fun) ->
    %% Expand top macro as much as possible.
    case exp_macro(Call, Env, St0) of
	{yes,Exp,St1} -> expand(Exp, Env, St1);
	no -> expand_tail(Call, Env, St0)
    end;
%%     case get_mbinding(Fun, Env) of
%% 	{yes,Def} ->
%% 	    {Exp,St1} = exp_userdef_macro(Call, Def, Env, St0),
%% 	    expand(Exp, Env, St1);		%Expand macro expansion again
%% 	no ->
%% 	    %% Not there then use defaults.
%% 	    case exp_predef_macro(Call, Env, St0) of
%% 		{yes,Exp,St1} -> expand(Exp, Env, St1);
%% 		no -> expand_tail(Call, Env, St0)
%% 	    end
%%     end;
expand([_|_]=Call, Env, St) -> expand_tail(Call, Env, St);
expand(Tup, _, St) when is_tuple(Tup) ->
    %% Should we expand this? We assume implicit quote here.
    {Tup,St};
%% Everything else is atomic.
expand(F, _, St) -> {F,St}.			%Atomic

%% expand_list(Exprs, Env, State) -> {Exps,State}.
%%  Expand a proper list of exprs.

expand_list(Es, Env, St) ->
    mapfoldl(fun (E, S) -> expand(E, Env, S) end, St, Es).

%% expand_tail(Tail, Env, State) -> {Etail,State}.
%% expand_tail(ExpFun, Tail, Env, State) -> {Etail,State}.
%%  Expand the tail of a list, need not be a proper list.

expand_tail(Tail, Env, St) ->
    expand_tail(fun expand/3, Tail, Env, St).

expand_tail(Fun, [E0|Es0], Env, St0) ->
    {E1,St1} = Fun(E0, Env, St0),
    {Es1,St2} = expand_tail(Fun, Es0, Env, St1),
    {[E1|Es1],St2};
expand_tail(_, [], _, St) -> {[],St};
expand_tail(Fun, E, Env, St) -> Fun(E, Env, St). %Same on improper tail.

%% expand_clauses(Clauses, Env, State) -> {ExpCls,State}.
%% expand_ml_clauses(Clauses, Env, State) -> {ExpCls,State}.
%%  Expand macros in clause patterns, guards and body. Must handle
%%  match-lambda clauses differently as pattern is an explicit list of
%%  patterns *NOT* a pattern which is a list. This will affect what is
%%  detected a macro call.

expand_clauses(Cls, Env, St) ->
    expand_tail(fun expand_clause/3, Cls, Env, St).

expand_clause([P0,['when'|G0]|B0], Env, St0) ->
    {P1,St1} = expand(P0, Env, St0),
    {G1,St2} = expand_tail(G0, Env, St1),
    {B1,St3} = expand_tail(B0, Env, St2),
    {[P1,['when'|G1]|B1],St3};
expand_clause([P0|B0], Env, St0) ->
    {P1,St1} = expand(P0, Env, St0),
    {B1,St2} = expand_tail(B0, Env, St1),
    {[P1|B1],St2};
expand_clause(Other, Env, St) -> expand(Other, Env, St).

expand_ml_clauses(Cls, Env, St) ->
    expand_tail(fun expand_ml_clause/3, Cls, Env, St).

expand_ml_clause([Ps0,['when'|G0]|B0], Env, St0) ->
    {Ps1,St1} = expand_tail(Ps0, Env, St0),
    {G1,St2} = expand_tail(G0, Env, St1),
    {B1,St3} = expand_tail(B0, Env, St2),
    {[Ps1,['when'|G1]|B1],St3};
expand_ml_clause([Ps0|B0], Env, St0) ->
    {Ps1,St1} = expand_tail(Ps0, Env, St0),
    {B1,St2} = expand_tail(B0, Env, St1),
    {[Ps1|B1],St2};
expand_ml_clause(Other, Env, St) -> expand(Other, Env, St).

%% expand_let_function(FuncBindings, Body, Env, State) -> {Expansion,State}.
%% expand_letrec_function(FuncBindings, Body, Env, State) -> {Expansion,State}.
%%  Expand a let/letrec-function. Here we are only interested in
%%  marking functions as bound in the env and not what they are bound
%%  to, we will not be calling them. We only want to shadow macros of
%%  the same name.

expand_let_function(Fbs0, B0, Env, St0) ->
    {Fbs1,B1,St1} = do_expand_let_function(Fbs0, B0, Env, St0),
    {['let-function',Fbs1|B1],St1}.

expand_letrec_function(Fbs0, B0, Env, St0) ->
    {Fbs1,B1,St1} = do_expand_let_function(Fbs0, B0, Env, St0),
    {['letrec-function',Fbs1|B1],St1}.

do_expand_let_function(Fbs0, B0, Env0, St0) ->
    %% Only very limited syntax checking here (see above).
    Env1 = foldl(fun ([V,['lambda',Args|_]], Env) when is_atom(V) ->
			 case is_proper_list(Args) of
			     true -> add_fbinding(V, length(Args), dummy, Env);
			     false -> Env
			 end;
		     ([V,['match-lambda',[Pats|_]|_]], Env) when is_atom(V) ->
			 case is_proper_list(Pats) of
			     true -> add_fbinding(V, length(Pats), dummy, Env);
			     false -> Env
			 end;
		     (_, Env) -> Env
		 end, Env0, Fbs0),
    {Fbs1,St1} = expand_clauses(Fbs0, Env1, St0),
    {B1,St2} = expand_tail(B0, Env1, St1),
    {Fbs1,B1,St2}.

%% expand_let_macro(MacroBindings, Body, Env, State) -> {Expansion,State}.
%%  Expand a let_syntax. We add the actual macro binding to the env as
%%  we may need them while expanding the body.

expand_let_macro(Mbs, B0, Env0, St0) ->
    %% Add the macro defs from expansion and return body in a progn.
    Env1 = foldl(fun ([Name,['lambda'|_]=Def], Env) when is_atom(Name) ->
			 add_mbinding(Name, Def, Env);
		     ([Name,['match-lambda'|_]=Def], Env) when is_atom(Name) ->
			 add_mbinding(Name, Def, Env);
		     (_, Env) -> Env		%Ignore mistakes
		 end, Env0, Mbs),
    {B1,St1} = expand_tail(B0, Env1, St0),	%Expand the body
    {['progn'|B1],St1}.

expand_try(E0, B0, Env, St0) ->
    {E1,St1} = expand(E0, Env, St0),
    {B1,St2} = expand_tail(fun (['case'|Cls0], E, Sta) ->
				   {Cls1,Stb} = expand_clauses(Cls0, E, Sta),
				   {['case'|Cls1],Stb};
			       (['catch'|Cls0], E, Sta) ->
				   {Cls1,Stb} = expand_clauses(Cls0, E, Sta),
				   {['catch'|Cls1],Stb};
			       (['after'|A0], E, Sta) ->
				   {A1,Stb} = expand_tail(A0, E, Sta),
				   {['after'|A1],Stb};
			       (Other, _, St) -> {Other,St}
			   end, B0, Env, St1),
    {['try',E1|B1],St2}.

%% exp_macro(Call, Env, State) -> {yes,Exp,State} | no.
%%  Expand the macro in top call, but not if it is a core form.

exp_macro([Name|_]=Call, Env, St) ->
    case lfe_lib:is_core_form(Name) of
	true -> no;				%Never expand core forms
	false ->
	    case get_mbinding(Name, Env) of
		{yes,Def} ->
		    %% User macro bindings.
		    exp_userdef_macro(Call, Def, Env, St);
		no ->
		    %% Default macro bindings.
		    exp_predef_macro(Call, Env, St)
	    end
    end.

%% exp_userdef_macro(Call, Def, Env, State) -> {yes,Exp,State}.
%%  Evaluate the macro definition by applying it to the call args. The
%%  definition is either a lambda or match-lambda, expand it and apply
%%  it to argument list.

exp_userdef_macro([Mac|Args], Def0, Env, St0) ->
    %%lfe_io:format("udef: ~p\n", [[Mac|Args]]),
    %%lfe_io:format("macro: ~p\n", [Def0]),
    try
	{Def1,St1} = expand(Def0, Env, St0),	%Expand definition
	Exp = lfe_eval:apply(Def1, [Args,Env], Env),
	{yes,Exp,St1}
    catch
	error:Error ->
	    %% St = erlang:get_stacktrace(),
	    erlang:error({expand_macro,[Mac|Args],Error})
    end.

%% exp_predef_macro(Call, Env, State) -> {yes,Exp,State} | no.
%%  Evaluate predefined macro definition catching errors.

exp_predef_macro(Call, Env, St) ->
    %%lfe_io:format("pdef: ~p\n", [Call]),
    try
	exp_predef(Call, Env, St)
    catch
	error:Error ->
	    %% St = erlang:get_stacktrace(),
	    erlang:error({expand_macro,Call,Error})
    end.

%% exp_predef(Form, Env, State) -> {yes,Form,State} | no.
%%  Handle the builtin predefined macros but only one at top-level and
%%  only once.  Expand must be called on result to fully expand
%%  macro. This is basically doing exactly the same as if they were
%%  user defined.

%% Builtin default macro expansions.
exp_predef([caar,E], _, St) -> {yes,[car,[car,E]],St};
exp_predef([cadr,E], _, St) -> {yes,[car,[cdr,E]],St};
exp_predef([cdar,E], _, St) -> {yes,[cdr,[car,E]],St};
exp_predef([cddr,E], _, St) -> {yes,[cdr,[cdr,E]],St};
%% Arithmetic operations and comparison operations.
%% Be careful to make these behave as if they were a function and
%% strictly evalated all their arguments.
exp_predef(['+'|Es], _, St0) ->
    case Es of
	[] -> {yes,0,St0};			%Identity
	_ ->
	    {Exp,St1} = exp_arith(Es, '+', St0),
	    {yes,Exp,St1}
    end;
exp_predef(['-'|Es], _, St0) ->
    case Es of
	[_|_] ->				%Non-empty argument list
	    {Exp,St1} = exp_arith(Es, '-', St0),
	    {yes,Exp,St1}
    end;
exp_predef(['*'|Es], _, St0) ->
    case Es of
	[] -> {yes,1,St0};			%Identity
	[_] -> {yes,exp_bif('*', [1|Es]),St0};	%Check if number
	_ ->
	    {Exp,St1} = exp_arith(Es, '*', St0),
	    {yes,Exp,St1}
    end;
exp_predef(['/'|Es], _, St0) ->
    case Es of
	[_] -> {yes,exp_bif('/', [1|Es]),St0};	%According to definition
	_ ->
	    {Exp,St1} = exp_arith(Es, '/', St0),
	    {yes,Exp,St1}
    end;
exp_predef([Op|Es], _, St0)			%Logical operators
  when Op == '>'; Op == '>='; Op == '<'; Op == '=<';
       Op == '=='; Op == '/='; Op == '=:='; Op == '=/=' ->
    case Es of
	[_|_] ->
	    {Exp,St1} = exp_comp(Es, Op, St0),
	    {yes,Exp,St1}
    end;
exp_predef([backquote,Bq], _, St) ->		%We do this here.
    {yes,exp_backquote(Bq),St};
exp_predef(['++'|Abody], _, St) ->
    Exp = exp_append(Abody),
    {yes,Exp,St};
exp_predef([':',M,F|As], _, St) ->
    {yes,['call',?Q(M),?Q(F)|As], St};
exp_predef(['?'|As], _, St) ->
    Exp = case As of
	      [To,Def] -> ['receive',['omega','omega'],['after',To,Def]];
	      [To] -> ['?',To,[exit,?Q(timeout)]];
	      [] -> ['receive',['omega','omega']]
	  end,
    {yes,Exp, St};
exp_predef(['list*'|As], _, St) ->
    Exp = case As of
	      [E] -> E;
	      [E|Es] -> [cons,E,['list*'|Es]];
	      [] -> []
	  end,
    {yes,Exp,St};
exp_predef(['let*'|Lbody], _, St) ->
    Exp = case Lbody of
	      [[Vb|Vbs]|B] -> ['let',[Vb],['let*',Vbs|B]];
	      [[]|B] -> ['progn'|B];
	      [Vb|B] -> ['let',Vb|B]		%Pass error to let for lint.
	  end,
    {yes,Exp,St};
exp_predef(['flet*'|Lbody], _, St) ->
    Exp = case Lbody of
	      [[Fb|Fbs]|B] -> ['flet',[Fb],['flet*',Fbs|B]];
	      [[]|B] -> ['progn'|B];
	      [Fb|B] -> ['flet',Fb|B]		%Pass error to flet for lint.
	  end,
    {yes,Exp,St};
exp_predef(['cond'|Cbody], _, St) ->
    Exp = case Cbody of
	      [['else'|B]] -> ['progn'|B];
	      [[['?=',P,E]|B]|Cond] ->
		  ['case',E,[P|B],['_',['cond'|Cond]]];
	      [[['?=',P,['when'|_]=G,E]|B]|Cond] ->
		  ['case',E,[P,G|B],['_',['cond'|Cond]]];
	      [[Test|B]|Cond] ->
		  ['if',Test,['progn'|B],['cond'|Cond]];
	      [] -> ?Q(false)
	  end,
    {yes,Exp,St};
exp_predef(['do'|Dbody], _, St0) ->
    %% (do ((v i c) ...) (test val) . body) but of limited use as it
    %% stands as we have to everything in new values.
    [Pars,[Test,Ret]|B] = Dbody,		%Check syntax
    {Vs,Is,Cs} = foldr(fun ([V,I,C], {Vs,Is,Cs}) -> {[V|Vs],[I|Is],[C|Cs]} end,
		       {[],[],[]}, Pars),
    {Fun,St1} = new_fun_name("do", St0),
    Exp = ['letrec-function',
	   [[Fun,[lambda,Vs,
		  ['if',Test,Ret,
		   ['progn'] ++ B ++ [[Fun|Cs]]]]]],
	   [Fun|Is]],
    {yes,Exp,St1};
exp_predef([lc|Lbody], _, St0) ->
    %% (lc (qual ...) e ...)
    [Qs|Es] = Lbody,
    {Exp,St1} = lc_te(Es, Qs, St0),
    {yes,Exp,St1};
exp_predef([bc|Bbody], _, St0) ->
    %% (bc (qual ...) e ...)
    [Qs|Es] = Bbody,
    {Exp,St1} = bc_te(Es, Qs, St0),
    {yes,Exp,St1};
exp_predef(['andalso'|Abody], _, St) ->
    Exp = case Abody of
	      [E] -> E;				%Let user check last call
	      [E|Es] -> ['if',E,['andalso'|Es],?Q(false)];
	      [] -> ?Q(true)
	  end,
    {yes,Exp,St};
exp_predef(['orelse'|Obody], _, St) ->
    Exp = case Obody of
	      [E] -> E;				%Let user check last call
	      [E|Es] -> ['if',E,?Q(true),['orelse'|Es]];
	      [] -> ?Q(false)
	  end,
    {yes,Exp,St};
exp_predef(['fun',F,Ar], _, St0) when is_atom(F), is_integer(Ar), Ar >= 0 ->
    {Vs,St1} = new_symbs(Ar, St0),
    {yes,['lambda',Vs,[F|Vs]],St1};
exp_predef(['fun',M,F,Ar], _, St0)
  when is_atom(M), is_atom(F), is_integer(Ar), Ar >= 0 ->
    {Vs,St1} = new_symbs(Ar, St0),
    {yes,['lambda',Vs,['call',?Q(M),?Q(F)|Vs]],St1};
exp_predef(['defrecord'|Def], Env0, St0) ->
    {Funs,Macs,_,St1} = exp_defrecord(Def, Env0, St0),
    {yes,[progn,['eval-when-compile'|Funs]|Macs],St1};
exp_predef(['include-file'|Ibody], _, St) ->
    %% This is a VERY simple include file macro!
    [F] = Ibody,
    case lfe_io:read_file(F) of			%Try to read file
	{ok,Fs} -> {yes,['progn'|Fs],St};
	{error,E} -> erlang:error({E,F}), no
    end;
exp_predef(['include-lib'|Ibody], _, St) ->
    %% This is a VERY simple include lib macro! First try to include
    %% the file directly else assume first directory name is a library
    %% name.
    [F] = Ibody,
    Fs = case lfe_io:read_file(F) of
	     {ok,Fs0} -> Fs0;
	     {error,_} ->
		 [LibName|Rest] = filename:split(F),
		 case code:lib_dir(list_to_atom(LibName)) of
		     LibDir when is_list(LibDir) ->
			 LibF = filename:join([LibDir|Rest]),
			 case lfe_io:read_file(LibF) of
			     {ok,Fs0} -> Fs0;
			     {error,E} -> erlang:error({E,LibF})
			 end;
		     {error,E} -> erlang:error({E,F})
		 end
	 end,
    {yes,['progn'|Fs],St};
%% Compatibility macros for the older Scheme like syntax.
exp_predef(['begin'|Body], _, St) ->
    {yes,['progn'|Body],St};
exp_predef(['define',Head|Body], _, St) ->
    Exp = case is_symb_list(Head) of
	      true ->
		  ['define-function',hd(Head),[lambda,tl(Head)|Body]];
	      false ->
		  %% Let next step catch errors here.
		  ['define-function',Head|Body]
	  end,
    {yes,Exp,St};
exp_predef(['define-record'|Def], _, St) ->
    {yes,[defrecord|Def],St};
exp_predef(['define-syntax',Name,Def], _, St) ->
    Mdef = exp_syntax(Name, Def),
    {yes,['define-macro'|Mdef],St};
exp_predef(['let-syntax',Defs|Body], _, St) ->
    Mdefs = map(fun ([Name,Def]) -> exp_syntax(Name, Def) end, Defs),
    {yes,['let-macro',Mdefs|Body],St};
%% Common Lisp inspired macros.
exp_predef([defmodule|Mod], _, St) ->
    {yes,['define-module'|Mod],St};
exp_predef([defun,Name|Def], _, St) ->
    %% Educated guess whether traditional (defun name (a1 a2 ...) ...)
    %% or matching (defun name (patlist1 ...) (patlist2 ...))
    Fdef = exp_defun(Name, Def),
    {yes,['define-function'|Fdef],St};
exp_predef([defmacro,Name|Def], _, St) ->
    %% Educated guess whether traditional (defmacro name (a1 a2 ...) ...)
    %% or matching (defmacro name (patlist1 ...) (patlist2 ...))
    Mdef = exp_defmacro(Name, Def),
    {yes,['define-macro'|Mdef],St};
exp_predef([defsyntax,Name|Rules], _, St) ->
    {yes,['define-macro'|exp_rules(Name, [], Rules)],St};
exp_predef([flet,Defs|Body], _, St) ->
    Fdefs = map(fun ([Name|Def]) -> exp_defun(Name, Def) end, Defs),
    {yes,['let-function',Fdefs|Body], St};
exp_predef([fletrec,Defs|Body], _, St) ->
    Fdefs = map(fun ([Name|Def]) -> exp_defun(Name, Def) end, Defs),
    {yes,['letrec-function',Fdefs|Body], St};
exp_predef([macrolet,Defs|Body], _, St) ->
    Mdefs = map(fun ([Name|Def]) -> exp_defmacro(Name, Def) end, Defs),
    {yes,['let-macro',Mdefs|Body],St};
exp_predef([syntaxlet,Defs|Body], _, St) ->
    Mdefs = map(fun ([Name|Rules]) -> exp_rules(Name, [], Rules) end, Defs),
    {yes,['let-macro',Mdefs|Body],St};
%% This has to go here for the time being so as to be able to macro
%% expand body.
exp_predef(['match-spec'|Body], Env, St0) ->
    %% Expand it like a match-lambda.
    {Exp,St1} = expand_ml_clauses(Body, Env, St0),
    MS = lfe_ms:expand(Exp),
    {yes,MS,St1};
%% (qlc (lc (qual ...) e ...) opts)
exp_predef([qlc,LC], Env, St) -> exp_qlc(LC, [], Env, St);
exp_predef([qlc,LC,Opts], Env, St) -> exp_qlc(LC, [Opts], Env, St);
%% This was not a call to a predefined macro.
exp_predef(_, _, _) -> no.

%% exp_qlc(LC, Opts, Env, State) -> {yes,Expansion,State}.
%% Expand a Query List Comprehension returning a call to qlc:q/2. We
%% first convert the LC into vanilla erlang AST, expand it using in
%% lfe_qlc.erl, which ql_pt.erl with a special interface, then convert
%% it back to LFE.

exp_qlc([lc,Qs|Es], Opts, Env, St0) ->
    %% Expand macros in the LC before translating it preserving
    %% structure.
    {Eqs,St1} = exp_qlc_quals(Qs, Env, St0),
    {Ees,St2} = expand_list(Es, Env, St1),
    %%lfe_io:format("Q0 = ~p\n", [[lc,Eqs|Ees]]),
    %% Now translate to vanilla AST, call qlc expand and then convert
    %% back to LFE.  lfe_qlc:expand/2 wants a list of conversions not
    %% a conversion of a list.
    Vlc = lfe_trans:to_vanilla([lc,Eqs|Ees], 42),
    Vos = map(fun (O) -> lfe_trans:to_vanilla(O, 42) end, Opts),
    %% io:put_chars([erl_pp:expr(Vlc),"\n"]),
    {ok,Vexp} = lfe_qlc:expand(Vlc, Vos),
    %%io:put_chars([erl_pp:expr(Vexp),"\n"]),
    Exp = lfe_trans:from_vanilla(Vexp),
    %%lfe_io:format("Q1 = ~p\n", [Exp]),
    {yes,Exp,St2}.

exp_qlc_quals(Qs, Env, St) ->
    mapfoldl(fun (Q, S) -> exp_qlc_qual(Q, Env, S) end, St, Qs).

exp_qlc_qual(['<-',P0,['when'|G0],E0], Env, St0) ->
    {P1,St1} = expand(P0, Env, St0),
    {G1,St2} = expand_tail(G0, Env, St1),
    {E1,St3} = expand(E0, Env, St2),
    {['<-',P1,['when'|G1],E1],St3};
exp_qlc_qual(['<-',P0,E0], Env, St0) ->
    {P1,St1} = expand(P0, Env, St0),
    {E1,St2} = expand(E0, Env, St1),
    {['<-',P1,E1],St2};
exp_qlc_qual(T, Env, St) -> expand(T, Env, St).

%% exp_bif(Bif, Args) -> Expansion.

exp_bif(B, As) -> [call,?Q(erlang),?Q(B)|As].

%% exp_args(Args, State) -> {LetBinds,State}.
%% Expand Args into a list of let bindings suitable for a let* or
%% nested lets to force sequential left-to-right evaluation.

exp_args(As, St) ->
    mapfoldl(fun (A, St0) -> {V,St1} = new_symb(St0), {[V,A],St1} end, St, As).

%% exp_arith(Args, Op, State) -> {Exp,State}.
%% Expand arithmetic call strictly forcing evaluation of all
%% arguments.  Note that single argument version may need special
%% casing.

exp_arith([A], Op, St) -> {exp_bif(Op, [A]),St};
exp_arith([A,B], Op, St) -> {exp_bif(Op, [A,B]),St};
exp_arith(As, Op, St0) ->
    {Ls,St1} = exp_args(As, St0),
    B = foldl(fun ([V,_], Acc) -> exp_bif(Op, [Acc,V]) end, hd(hd(Ls)), tl(Ls)),
    {['let*',Ls,B],St1}.

%% exp_comp(Args, Op, State) -> {Exp,State}.
%% Expand comparison test strictly forcing evaluation of all
%% arguments. Note that single argument version may need special
%% casing.

exp_comp([A], _, St) ->			%Force evaluation
    {['let',[['_',A]],?Q(true)],St};
exp_comp([A,B], Op, St) -> {exp_bif(Op, [A,B]),St};
exp_comp(As, Op, St0) ->
    {Ls,St1} = exp_args(As, St0),
    {Ts,_} = mapfoldl(fun ([V,_], Acc) -> {exp_bif(Op, [Acc,V]),V} end,
		      hd(hd(Ls)), tl(Ls)),
    {['let*',Ls,['andalso'|Ts]],St1}.

%% exp_append(Args) -> Expansion.
%%  Expand ++ in such a way as to allow its use in patterns. There are
%%  a lot of interesting cases here. Only be smart with proper forms.

exp_append(Args) ->
    case Args of
	%% Cases with quoted lists.
	[?Q([A|As])|Es] -> [cons,?Q(A),['++',?Q(As)|Es]];
	[?Q([])|Es] -> ['++'|Es];
	%% Cases with explicit cons/list/list*.
	[['list*',A]|Es] -> ['++',A|Es];
	[['list*',A|As]|Es] -> [cons,A,['++',['list*'|As]|Es]];
	[[list,A|As]|Es] -> [cons,A,['++',[list|As]|Es]];
	[[list]|Es] -> ['++'|Es];
	[[cons,H,T]|Es] -> [cons,H,['++',T|Es]];
	[[]|Es] -> ['++'|Es];
	%% Default cases with unquoted arg.
	[E] -> E;				%Last arg not checked
	[E|Es] -> exp_bif('++', [E,['++'|Es]]);
	[] -> []
    end.

%% exp_defun(Name, Def) -> Lambda | Match-Lambda.
%% Educated guess whether traditional (defun name (a1 a2 ...) ...)
%% or matching (defun name (patlist1 ...) (patlist2 ...))

exp_defun(Name, [Args|Rest]) ->
    case is_symb_list(Args) of
	true -> [Name,[lambda,Args|Rest]];
	false -> [Name,['match-lambda',Args|Rest]]
    end.

%% exp_defmacro(Name, Def) -> Lambda | MatchLambda.
%%  Educated guess whether traditional (defmacro name (a1 a2 ...) ...)
%%  or matching (defmacro name (patlist1 ...) (patlist2 ...)). Special
%%  case (defmacro name arg ...) to make arg be whole argument list.
%%  N.B. New macro definition is function of 2 arguments, the whole
%%  argument list of macro call, and the current macro environment.

exp_defmacro(Name, [Args|Rest]=Cls) ->
    case is_symb_list(Args) of
	true -> [Name,['match-lambda',[[[list|Args],'$ENV']|Rest]]];
	false ->
	    if is_atom(Args) ->			%Args is symbol
		    [Name,['match-lambda',[[Args,'$ENV']|Rest]]];
	       true ->
		    Mcls = map(fun ([Head|Body]) -> [[Head,'$ENV']|Body] end, Cls),
		    [Name,['match-lambda'|Mcls]]
	    end
    end.

%% exp_defrecord([Name|FieldDefs], Env, State) -> {Funs,Macs,Env,State}.
%% exp_defrecord(Name, FieldDefs, Env, State) -> {Funs,Macs,Env,State}.
%%  Define a VERY simple record by generating macros for all accesses.
%%  (define-record point x y)
%%    => make-point, is-point, match-point, set-point,
%%       point-x, set-point-x, point-y, set-point-y.

exp_defrecord([Name|Fdefs], Env, St) ->
    exp_defrecord(Name, Fdefs, Env, St).

exp_defrecord(Name, Fdefs, Env, St0) ->
    %% Get field names, default values and indices.
    Fields = map(fun ([F,_])when is_atom(F) -> F;
		     (F) when is_atom(F) -> F
		 end, Fdefs),
    Defs = map(fun ([F,D])when is_atom(F) -> ?Q(D);
		   (F) when is_atom(F) -> ?Q(?Q(undefined))
	       end, Fdefs),
    Findex = exp_defrec_indexes(Fields),
    %% Make names for helper functions.
    Fi = list_to_atom(concat([Name,'-',field,'-',index])),
    Fu = list_to_atom(concat([Name,'-',field,'-',update])),
    %% Build helper functions.
    Funs = [[defun,Fi|				%Get index of field
	     map(fun ({F,I}) -> [[?Q(F)],I] end, Findex) ++
	     [[[f],[':',erlang,error,[tuple,?Q(undefined_field),?Q(Name),f]]]]],
	    [defun,Fu,[is,def],			%Update field list
	     %% Convert default list to tuple to make setting easier.
	     [fletrec,[[l,
			[[[cons,f,[cons,v,is]],i],
			 [l,is,[setelement,['-',[Fi,f],1],i,v]]],
			[[[list,f],'_'],
			 [':',erlang,error,
			  [tuple,?Q(missing_value),?Q(Name),f]]],
			[[[],i],i]]],
	      ['let',[[i,[l,is,[list_to_tuple,def]]]],
	       [tuple_to_list,i]]]]
	   ],
    %% Make names for record creator/tester/matcher/setter.
    Make = list_to_atom(concat(['make','-',Name])),
    Match = list_to_atom(concat(['match','-',Name])),
    Test = list_to_atom(concat(['is','-',Name])),
    EMP = list_to_atom(concat(['emp','-',Name])),
    Set = list_to_atom(concat(['set','-',Name])),
    %% Make access macros.
    {Fdef,St1} = exp_defrec_fields(Fields, Name, St0), %Name is element 1!
    Macs = [['defmacro',Make,fds,
	     ['let',[[def,[list|Defs]]],
	      ?BQ([tuple,?Q(Name),?UQ_S([Fu,fds,def])])]],
	    ['defmacro',Match,fds,
	     ['let',[[def,[list|lists:duplicate(length(Fields),?Q('_'))]]],
	      ?BQ([tuple,?Q(Name),?UQ_S([Fu,fds,def])])]],
	    ['defmacro',Test,[rec],
	     ?BQ(['is_record',?UQ(rec),?Q(Name),length(Fields)+1])],
	    ['defmacro',Set,
	     [[cons,rec,fds],
	      [fletrec,[[l,
			 [[[cons,f,[cons,v,is]],r],
			  %% Force evaluation left-to-right.
			  [l,is,[list,[quote,setelement],[Fi,f],r,v]]],
			 [[[list,f],'_'],
			  [':',erlang,error,
			   [tuple,?Q(missing_value),?Q(Name),f]]],
			 [[[],i],i]]],
	       [l,fds,rec]]]],
	    ['defmacro',EMP,fds,
	      ['let',[[def,[list|lists:duplicate(length(Fields),?Q(?Q('_')))]]],
	       ?BQ([tuple,?Q(Name),?UQ_S([Fu,fds,def])])]]
	    |
	    Fdef],
    %% lfe_io:format("~p\n", [{Funs,Macs}]),
    {Funs,Macs,Env,St1}.

exp_defrec_indexes([F|Fs]) ->
    exp_defrec_indexes([F|Fs], 2).		%First element is record name

exp_defrec_indexes([F|Fs], N) ->
    [{F,N}|exp_defrec_indexes(Fs, N+1)];
exp_defrec_indexes([], _) -> [].

exp_defrec_fields(Fs, Name, St) ->
    Fis = exp_defrec_indexes(Fs),		%Calculate indexes
    {foldr(fun ({F,N}, Fas) ->
		   Get = list_to_atom(concat([Name,'-',F])),
		   Set = list_to_atom(concat(['set-',Name,'-',F])),
		   [[defmacro,Get,[rec],?BQ([element,N,?UQ(rec)])],
		    [defmacro,Set,[rec,new],
		     ?BQ([setelement,N,?UQ(rec),?UQ(new)])]|
		    Fas]
	   end, [], Fis), St}.

%% exp_syntax(Name, Def) -> Lambda | MatchLambda.
%%  N.B. New macro definition is function of 2 arguments, the whole
%%  argument list of macro call, and the current macro environment.

exp_syntax(Name, Def) ->
    case Def of
	[macro|Cls] ->
	    Mcls = map(fun ([Pat|Body]) -> [[Pat,'$ENV']|Body] end, Cls),
	    [Name,['match-lambda'|Mcls]];
	['syntax-rules'|Rules] ->
	    exp_rules(Name, [], Rules)
    end.

%% exp_rules(Name, Keywords, Rules) -> Lambda.
%%  Expand into call function which expands macro an invocation time,
%%  this saves much space and costs us nothing.
%%  N.B. New macro definition is function of 2 arguments, the whole
%%  argument list of macro call, and the current macro environment.

exp_rules(Name, Keywords, Rules) ->
    [Name,[lambda,[args,'$ENV'],
	   [':',lfe_macro,mbe_syntax_rules_proc,
	    [quote,Name],[quote,Keywords],[quote,Rules],args]]].

%%  By André van Tonder
%%  Unoptimized.  See Dybvig source for optimized version.
%%  Resembles one by Richard Kelsey and Jonathan Rees.
%%   (define-syntax quasiquote
%%     (lambda (s)
%%       (define (qq-expand x level)
%%         (syntax-case x (quasiquote unquote unquote-splicing)
%%           (`x   (quasisyntax (list 'quasiquote
%%                                    #,(qq-expand (syntax x) (+ level 1)))))
%%           (,x (> level 0)
%%                 (quasisyntax (cons 'unquote
%%                                    #,(qq-expand (syntax x) (- level 1)))))
%%           (,@x (> level 0)
%%                 (quasisyntax (cons 'unquote-splicing
%%                                    #,(qq-expand (syntax x) (- level 1)))))
%%           (,x (= level 0)
%%                 (syntax x))
%%           (((unquote x ...) . y)
%%            (= level 0)
%%                 (quasisyntax (append (list x ...)
%%                                      #,(qq-expand (syntax y) 0))))
%%           (((unquote-splicing x ...) . y)
%%            (= level 0)
%%                 (quasisyntax (append (append x ...)
%%                                      #,(qq-expand (syntax y) 0))))
%%           ((x . y)
%%                 (quasisyntax (cons  #,(qq-expand (syntax x) level)
%%                                     #,(qq-expand (syntax y) level))))
%%           (#(x ...)
%%                 (quasisyntax (list->vector #,(qq-expand (syntax (x ...))    
%%                                                         level))))
%%           (x    (syntax 'x)))) 
%%       (syntax-case s ()
%%         ((_ x) (qq-expand (syntax x) 0)))))

%% exp_backquote(Exp) -> Exp.
%%  Not very efficient quasiquote expander, but very compact code.  Is
%%  R6RS compliant and can handle unquote and unquote-splicing with
%%  more than one argument properly.  Actually with simple cons/append
%%  optimisers code now quite good.

exp_backquote(Exp) -> exp_backquote(Exp, 0).

exp_backquote([backquote,X], N) ->
    [list,[quote,backquote],exp_backquote(X, N+1)];
exp_backquote([unquote|X], N) when N > 0 ->
    exp_bq_cons([quote,unquote], exp_backquote(X, N-1));
exp_backquote([unquote,X], 0) -> X;
exp_backquote(['unquote-splicing'|X], N) when N > 0 ->
    exp_bq_cons([quote,'unquote-splicing'], exp_backquote(X, N-1));
%% Next 2 handle case of splicing into a list.
exp_backquote([[unquote|X]|Y], 0) ->
    exp_bq_append([list|X], exp_backquote(Y, 0));
exp_backquote([['unquote-splicing'|X]|Y], 0) ->
    exp_bq_append(['++'|X], exp_backquote(Y, 0));
exp_backquote([X|Y], N) ->			%The general list case
    exp_bq_cons(exp_backquote(X, N), exp_backquote(Y, N));
exp_backquote(X, N) when is_tuple(X) ->
    %% Straight [list_to_tuple,exp_backquote(tuple_to_list(X), N)] inefficient
    %% and [tuple|tl(exp_backquote(tuple_to_list(X), N))] can't handle splicing!
    case exp_backquote(tuple_to_list(X), N) of
	[list|Es] -> [tuple|Es];		%No splicing
	[cons|_]=E -> [list_to_tuple,E]		%Have splicing
    end;
exp_backquote(X, _) when is_atom(X) -> [quote,X];
exp_backquote(X, _) -> X.			%Self quoting

exp_bq_append(['++',L], R) ->			%Catch single unquote-splice
    exp_bq_append(L, R);
exp_bq_append([], R) -> R;
exp_bq_append(L, []) -> L;
%% Will these 2 cases move code errors illegally?
exp_bq_append([list,L], [list|R]) -> [list,L|R];
exp_bq_append([list,L], R) -> [cons,L,R];
%%exp_bq_append(['++'|L], R) -> ['++'|L ++ [R]];
%%exp_bq_append(L, ['++'|R]) -> ['++',L|R];
exp_bq_append(L, R) -> ['++',L,R].

exp_bq_cons([quote,L], [quote,R]) -> [quote,[L|R]];
exp_bq_cons(L, [list|R]) -> [list,L|R];
exp_bq_cons(L, []) -> [list,L];
exp_bq_cons(L, R) -> [cons,L,R].

new_symb(St) ->
    C = St#mac.vc,
    {list_to_atom("|-" ++ integer_to_list(C) ++ "-|"),St#mac{vc=C+1}}.

new_symbs(N, St) -> new_symbs(N, St, []).

new_symbs(N, St0, Vs) when N > 0 ->
    {V,St1} = new_symb(St0),
    new_symbs(N-1, St1, [V|Vs]);
new_symbs(0, St, Vs) -> {Vs,St}.    

new_fun_name(Pre, St) ->
    C = St#mac.fc,
    {list_to_atom(Pre ++ "$^" ++ integer_to_list(C)),St#mac{fc=C+1}}.

%% Macro by Example
%% Proper syntax-rules which can handle ... ellipsis by Dorai Sitaram.
%%
%% While we extend patterns to include tuples and binaries as in
%% normal LFE we leave the keyword handling in even though it is
%% subsumed by quotes and not really used.

%% To make it more lispy!
-define(car(L), hd(L)).
-define(cdr(L), tl(L)).
-define(cadr(L), hd(tl(L))).
-define(cddr(L), tl(tl(L))).

-define(mbe_ellipsis(Car, Cddr), [Car,'...'|Cddr]).

is_mbe_symbol(S) ->
    is_atom(S) andalso (S /= true) andalso (S /= false).

%% Tests if ellipsis pattern, (p ... . rest)
%% is_mbe_ellipsis(?mbe_ellipsis(_, _)) -> true;
%% is_mbe_ellipsis(_) -> false.

mbe_match_pat([quote,P], E, _) -> P =:= E;
mbe_match_pat([tuple|Ps], [tuple|Es], Ks) ->	%Match tuple constructor
    mbe_match_pat(Ps, Es, Ks);
mbe_match_pat([tuple|Ps], E, Ks) ->		%Match literal tuple
    case is_tuple(E) of
	true -> mbe_match_pat(Ps, tuple_to_list(E), Ks);
	false -> false
    end;
mbe_match_pat(?mbe_ellipsis(Pcar, _), E, Ks) ->
    case is_proper_list(E) of
	true ->
	    all(fun (X) -> mbe_match_pat(Pcar, X, Ks) end, E);
	false -> false
    end;
mbe_match_pat([Pcar|Pcdr], E, Ks) ->
    case E of
	[Ecar|Ecdr] ->
	    mbe_match_pat(Pcar, Ecar, Ks) andalso
		mbe_match_pat(Pcdr, Ecdr, Ks);
	_ -> false
    end;
mbe_match_pat(Pat, E, Ks) ->
    case is_mbe_symbol(Pat) of
	true ->
	    case member(Pat, Ks) of
		true -> Pat =:= E;
		false -> true
	    end;
	false -> Pat =:= E
    end.

mbe_get_ellipsis_nestings(Pat, Ks) ->
    m_g_e_n(Pat, Ks).

m_g_e_n([quote,_], _) -> [];
m_g_e_n([tuple|Ps], Ks) -> m_g_e_n(Ps, Ks);
m_g_e_n(?mbe_ellipsis(Pcar, Pcddr), Ks) ->
    [m_g_e_n(Pcar, Ks)|m_g_e_n(Pcddr, Ks)];
m_g_e_n([Pcar|Pcdr], Ks) ->
    m_g_e_n(Pcar, Ks) ++ m_g_e_n(Pcdr, Ks);
m_g_e_n(Pat, Ks) ->
    case is_mbe_symbol(Pat) of
	true ->
	    case member(Pat, Ks) of
		true -> [];
		false -> [Pat]
	    end;
	false -> []
    end.

mbe_ellipsis_sub_envs(Nestings, R) ->
    ormap(fun (C) ->
		  case mbe_intersect(Nestings, ?car(C)) of
		      true -> ?cdr(C);
		      false -> false
		  end end, R).

%% Return first value of F applied to elements in list which is not false.
ormap(F, [H|T]) ->
    case F(H) of
	false -> ormap(F, T);
	V -> V
    end;
ormap(_, []) -> false.
    
mbe_intersect(V, Y) ->
    case is_mbe_symbol(V) orelse is_mbe_symbol(Y) of
	true -> V =:= Y;
	false ->
	    any(fun (V0) ->
			any(fun (Y0) -> mbe_intersect(V0, Y0) end, Y)
		end, V)
    end.

%% mbe_get_bindings(Pattern, Expression, Keywords) -> Bindings.

mbe_get_bindings([quote,_], _, _) -> [];
mbe_get_bindings([tuple|Ps], [tuple|Es], Ks) ->	%Tuple constructor
    mbe_get_bindings(Ps, Es, Ks);
mbe_get_bindings([tuple|Ps], E, Ks) ->		%Literal tuple
    mbe_get_bindings(Ps, tuple_to_list(E), Ks);
mbe_get_bindings(?mbe_ellipsis(Pcar, _), E, Ks) ->
    [[mbe_get_ellipsis_nestings(Pcar, Ks) |
      map(fun (X) -> mbe_get_bindings(Pcar, X, Ks) end, E)]];
mbe_get_bindings([Pcar|Pcdr], [Ecar|Ecdr], Ks) ->
    mbe_get_bindings(Pcar, Ecar, Ks) ++
	mbe_get_bindings(Pcdr, Ecdr, Ks);
mbe_get_bindings(Pat, E, Ks) ->
    case is_mbe_symbol(Pat) of
	true ->
	    case member(Pat, Ks) of
		true -> [];
		false -> [[Pat|E]]
	    end;
	false -> []
    end.

%% mbe_expand_pattern(Pattern, Bindings, Keywords) -> Form.

mbe_expand_pattern([quote,P], R, Ks) ->
    [quote,mbe_expand_pattern(P, R, Ks)];
mbe_expand_pattern([tuple|Ps], R, Ks) ->
    [tuple|mbe_expand_pattern(Ps, R, Ks)];
mbe_expand_pattern(?mbe_ellipsis(Pcar, Pcddr), R, Ks) ->
    Nestings = mbe_get_ellipsis_nestings(Pcar, Ks),
    Rr = mbe_ellipsis_sub_envs(Nestings, R),
    map(fun (R0) -> mbe_expand_pattern(Pcar, R0 ++ R, Ks) end, Rr) ++
		mbe_expand_pattern(Pcddr, R, Ks);
mbe_expand_pattern([Pcar|Pcdr], R, Ks) ->
    [mbe_expand_pattern(Pcar, R, Ks)|
     mbe_expand_pattern(Pcdr, R, Ks)];
mbe_expand_pattern(Pat, R, Ks) ->
    case is_mbe_symbol(Pat) of
	true ->
	    case member(Pat, Ks) of
		true -> Pat;
		false ->
		    case lfe_lib:assoc(Pat, R) of
			[_|Cdr] -> Cdr;
			[] -> Pat
		    end
	    end;
	false -> Pat
    end.

%% mbe_syntax_rules_proc(Name, Keywords, Rules, Argsym, Keywordsym) ->
%%      Sexpr.
%%  Generate the sexpr to evaluate in a macro from Name and
%%  Rules. When the sexpr is applied to arguments (in Argsym) and
%%  evaluated then expansion is returned.

%% Return sexpr to evaluate.
mbe_syntax_rules_proc(Name, Ks0, Cls, Argsym, Ksym) ->
    Ks = [Name|Ks0],
    %% Don't prepend the macro name to the arguments!
    ['let',[[Ksym,[quote,Ks]]],
     ['cond'] ++
     map(fun (C) ->
		 Inpat = hd(C),
		 Outpat = hd(tl(C)),
		 [[':',lfe_macro,mbe_match_pat,[quote,Inpat], Argsym, Ksym],
		  ['let',
		   [[r,[':',lfe_macro,mbe_get_bindings,
			[quote,Inpat],Argsym,Ksym]]],
		   [':',lfe_macro,mbe_expand_pattern,[quote,Outpat],r,Ksym]]]
	 end, Cls) ++
    [[[quote,true],[':',erlang,error,
		    [tuple,
		     [quote,expand_macro],
		     [cons,[quote,Name],Argsym], %??? Must check this
		     [quote,macro_clause]]]]]].

%% Do it all directly.
mbe_syntax_rules_proc(Name, Ks0, Cls, Args) ->
    Ks = [Name|Ks0],
    case ormap(fun ([Pat,Exp]) ->
		       case mbe_match_pat(Pat, Args, Ks) of
			   true ->
			       R = mbe_get_bindings(Pat, Args, Ks),
			       [mbe_expand_pattern(Exp, R, Ks)];
			   false -> false
		       end
	       end, Cls) of
	[Res] -> Res;
	false -> erlang:error({expand_macro,[Name|Args],macro_clause})
    end.

%% lc_te(Exprs, Qualifiers, State) -> {Exp,State}.
%% bc_te(Exprs, Qualifiers, State) -> {Exp,State}.
%%  Expand a list/binary comprehension. Algorithm straight out of
%%  Simon PJs book.

%% lc_te(Es, Qs, St) -> lc_tq(Es, Qs, [], St).
lc_te(Es, Qs, St) -> lc_te(Es, Qs, [], St).

lc_te(Es, Qs, End, St) ->
    c_tq(fun (E, S) -> {[cons,['progn'|Es],E],S} end, Qs, End, St).

%%bc_te(Es, Qs, St) -> bc_tq(Es, Qs, <<>>, St).
bc_te(Es, Qs, St) ->
    c_tq(fun (E, S) ->
		 %% Separate last form to be binary segment.
		 case reverse(Es) of
		     [R] -> {[binary,R,[E,bitstring]],S};
		     [R|Rs] -> {['progn'|reverse(Rs)] ++
				[[binary,R,[E,bitstring]]],S};
		     [] -> {E,S}
		 end
	 end, Qs, <<>>, St).

%% c_tq(BuildExp, Qualifiers, End, State) -> {Exp,State}.

c_tq(Exp, [['<-',P,Gen]|Qs], End, St) ->		%List generator
    c_l_tq(Exp, P, [], Gen, Qs, End, St);
c_tq(Exp, [['<-',P,['when'|G],Gen]|Qs], End, St) ->	%List generator
    c_l_tq(Exp, P, G, Gen, Qs, End, St);
c_tq(Exp, [['<=',P,Gen]|Qs], End, St) ->		%Bits generator
    c_b_tq(Exp, P, [], Gen, Qs, End, St);
c_tq(Exp, [['<=',P,['when'|G],Gen]|Qs], End, St) ->	%Bits generator
    c_b_tq(Exp, P, G, Gen, Qs, End, St);
c_tq(Exp, [['?=',P,E]|Qs], End, St0) ->			%Test match
    {Rest,St1} = c_tq(Exp, Qs, End, St0),
    {['case',E,[P,Rest],['_',End]],St1};
c_tq(Exp, [['?=',P,['when'|_]=G,E]|Qs], End, St0) ->	%Test match
    {Rest,St1} = c_tq(Exp, Qs, End, St0),
    {['case',E,[P,G,Rest],['_',End]],St1};
c_tq(Exp, [T|Qs], End, St0) ->				%Test
    {Rest,St1} = c_tq(Exp, Qs, End, St0),
    {['if',T,Rest,End],St1};
c_tq(Exp, [], End, St) ->				%End of qualifiers
    Exp(End, St).

c_l_tq(Exp, P, G, Gen, Qs, End, St0) ->
    {H,St1} = new_fun_name("lc", St0),		%Function name
    {Us,St2} = new_symb(St1),			%Tail variable
    {Rest,St3} = c_tq(Exp, Qs, [H,Us], St2),	%Do rest of qualifiers
    %% Build the match, no match and end clauses, no nomatch clause if
    %% pattern and guard guaranteed to match. Keeps compiler quiet.
    Cs0 = [ [[[]],End] ],			%End of list
    Cs1 = case is_atom(P) and (G == []) of	%No match, skip
	      true -> Cs0;
	      false -> [ [[[cons,'_',Us]],[H,Us]] |Cs0]
	  end,
    Cs2 = [ [[[cons,P,Us]],['when'|G],Rest] |Cs1], %Matches pattern and guard
    {['letrec-function',
      [[H,['match-lambda'|Cs2]]],
      [H,Gen]],St3}.

c_b_tq(Exp, P, G, Gen, Qs, End, St0) ->
    {H,St1} = new_fun_name("bc", St0),		%Function name
    {B,St2} = new_symb(St1),			%Bin variable
    {Rest,St3} = c_tq(Exp, Qs, [H,B], St2),	%Do rest of qualifiers
    Brest = [B,bitstring,'big-endian',unsigned,[unit,1]], %,[size,all]
    %% Build the match and nomatch/end clauses.
    MatchC = [[[binary,P,Brest]],['when'|G],Rest], %Matches pattern and guard
    EndC = [[[binary,Brest]],End],		%No match
    {['letrec-function',
      [[H,['match-lambda',MatchC,EndC]]],
      [H,Gen]],St3}.

%% c_tq(Exp, [['<-',P,Gen]|Qs], End, St0) ->	%List generator
%%     {H,St1} = new_fun_name("lc", St0),		%Function name
%%     {Us,St2} = new_symb(St1),			%Tail variable
%%     {Rest,St3} = c_tq(Exp, Qs, [H,Us], St2),	%Do rest of qualifiers
%%     {['letrec-function',
%%       [[H,['match-lambda',
%% 	   [[[P|Us]],Rest],			%Matches pattern
%% 	   [[['_'|Us]],[H,Us]],			%No match
%% 	   [[[]],End]]]],			%End of list
%%       [H,Gen]],St3};

%% c_tq(Exp, [['<=',P,Gen]|Qs], End, St0) ->	%Bits generator
%%     {H,St1} = new_fun_name("bc", St0),		%Function name
%%     {B,St2} = new_symb(St1),			%Bin variable
%%     {Rest,St3} = c_tq(Exp, Qs, [H,B], St2),	%Do rest of qualifiers
%%     Brest = [B,bitstring,'big-endian',unsigned,[unit,1]], %,[size,all]
%%     {['letrec-function',
%%       [[H,['match-lambda',
%% 	   [[[binary,P,Brest]],Rest],		%Matches pattern
%% 	   [[[binary,Brest]],End]]]],		%No match
%%       [H,Gen]],St3};
