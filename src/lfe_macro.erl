%% Copyright (c) 2008-2016 Robert Virding
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

%% -compile(export_all).

%% These work on individual expressions.
-export([expand_expr/2,expand_expr_1/2,expand_expr_all/2]).

%% These work on list of forms in "file format".
-export([expand_forms/4]).
-export([expand_form_init/2,expand_form_init/3,
         expand_form/4,expand_fileform/3]).

%% For creating the macro expansion state.
-export([default_state/2,default_state/3]).

-export([format_error/1]).

-export([mbe_syntax_rules_proc/4,mbe_syntax_rules_proc/5,
         mbe_match_pat/3,mbe_get_bindings/3,mbe_expand_pattern/3]).

%% -compile([export_all]).

-import(lfe_env, [new/0,add_vbinding/3,is_vbound/2,
                  add_fbinding/4,is_fbound/3,
                  add_mbinding/3,is_mbound/2,get_mbinding/2]).

-import(lists, [any/2,all/2,map/2,foldl/3,foldr/3,mapfoldl/3,
                reverse/1,reverse/2,member/2,concat/1]).

-include("lfe_comp.hrl").
-include("lfe_macro.hrl").

%% Define IS_MAP/1 macro for is_map/1 bif.
-ifdef(HAS_MAPS).
-define(IS_MAP(T), is_map(T)).
-else.
-define(IS_MAP(T), false).
-endif.

%% Errors
format_error({bad_form,Type}) ->
    lfe_io:format1("bad form: ~w", [Type]);
format_error({bad_env_form,Type}) ->
    lfe_io:format1("bad environment form: ~w", [Type]);
format_error({expand_macro,Call,Error}) ->
    %% Can be very big so only print limited depth.
    lfe_io:format1("error expanding ~P: ~P", [Call,10,Error,10]).

%% expand_expr(Form, Env) -> {yes,Exp} | no.
%% expand_expr_1(Form, Env) -> {yes,Exp} | no.
%%  User functions for testing macro expansions, either one expansion
%%  or as far as it can go.

expand_expr_1([Name|_]=Call, Env) when is_atom(Name) ->
    St = default_state(false, false),
    case exp_macro(Call, Env, St) of
        {yes,Exp,_} -> {yes,Exp};
        no -> no
    end;
expand_expr_1(_, _) -> no.

expand_expr([Name|_]=Call, Env) when is_atom(Name) ->
    St0 = default_state(false, false),
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
    {Ef,_} = exp_form(F, Env, default_state(true, false)),
    Ef.

%% expand_forms(FileForms, Env, Deep, Keep) ->
%%     {ok,FileForms,Env,Warnings} | {error,Errors,Warnings}.
%%  Collect macro definitions in file forms, completely expand all
%%  macros and only keep all functions.

expand_forms(Fs, Env, Deep, Keep) ->
    St = default_state(Deep, Keep),
    do_forms(Fs, Env, St).

do_forms(Fs0, Env0, St0) ->
    {Fs1,Env1,St1} = pass_fileforms(Fs0, Env0, St0),
    case St1#mac.errors of
        [] -> {ok,Fs1,Env1,St1#mac.warnings};    %No errors
        Es -> {error,Es,St1#mac.warnings}
    end.

default_state(Deep, Keep) ->
    #mac{deep=Deep,keep=Keep,line=1,file="-no-file-",opts=[],ipath=["."]}.

default_state(#cinfo{file=File,opts=Os,ipath=Is}, Deep, Keep) ->
    #mac{deep=Deep,keep=Keep,line=1,file=File,opts=Os,ipath=Is}.

%% expand_form_init(Deep, Keep) -> State.
%% expand_form_init(CompInfo, Deep, Keep) -> State.
%% expand_form(Form, Line, Env, State) -> {Form,Env,State}.
%% expand_fileform(Form, Env, State) -> {Form,Env,State}.
%%  Collect macro definitions in a (file)form, completely expand all
%%  macros and only keep all functions.

expand_form_init(Deep, Keep) ->
    default_state(Deep, Keep).

expand_form_init(Ci, Deep, Keep) ->
    default_state(Ci, Deep, Keep).

expand_form(F0, L, E0, St0) ->
    {F1,E1,St1} = pass_form(F0, E0, St0#mac{line=L}),
    return_status(F1, E1, St1).

expand_fileform({F0,L}, E0, St0) ->
    {F1,E1,St1} = pass_form(F0, E0, St0#mac{line=L}),
    return_status({F1,L}, E1, St1).

return_status(Ret, Env, #mac{errors=[]}=St) ->
    {ok,Ret,Env,St};
return_status(_, _, #mac{errors=Es,warnings=Ws}=St) ->
    {error,Es,Ws,St}.

%% pass_fileforms(FileForms, Env, State) -> {FileForms,Env,State}.
%% pass_forms(Forms, Env, State) -> {Forms,Env,State}.
%%  Pass over a list of fileforms/forms collecting and removing all macro
%%  defintions. All forms must be expanded at top-level to check form,
%%  but all can be expanded to full depth. Nesting of forms by progn
%%  is preserved.

pass_fileforms(Ffs, Env, St) ->
    mapfoldl2(fun ({F0,L}, E0, S0) ->
                      {F1,E1,S1} = pass_form(F0, E0, S0#mac{line=L}),
                      {{F1,L},E1,S1}
              end, Env, St, Ffs).

pass_forms(Fs, Env, St) ->
    mapfoldl2(fun (F0, E0, S0) -> pass_form(F0, E0, S0) end, Env, St, Fs).

%% pass_form(Form, Env, State) -> {Form,Env,State}.
%%  Do a form collecting and removing all macro defintions. The form
%%  must be expanded at top-level to check it, but it can be expanded
%%  to full depth. Nesting of forms by progn is preserved.

pass_form(['progn'|Pfs0], Env0, St0) ->
    {Pfs1,Env1,St1} = pass_forms(Pfs0, Env0, St0),
    {['progn'|Pfs1],Env1,St1};
pass_form(['eval-when-compile'|Efs0], Env0, St0) ->
    {Efs1,Env1,St1} = pass_ewc(Efs0, Env0, St0),
    {['eval-when-compile'|Efs1],Env1,St1};
pass_form(['define-macro'|Def]=M, Env0, St0) ->
    case pass_define_macro(Def, Env0, St0) of
        {yes,Env1,St1} ->
            Ret = ?IF(St1#mac.keep, M, [progn]),
            {Ret,Env1,St1};                     %Must return a valid form
        no ->
            St1 = add_error({bad_form,macro}, St0),
            {['progn'],Env0,St1}                %Must return a valid form
    end;
pass_form(F, Env, St0) ->
    %% First expand enough to test top form, if so process again.
    case pass_expand_expr(F, Env, St0, St0#mac.deep) of
        {yes,Exp,St1} ->                        %Top form expanded
            pass_form(Exp, Env, St1);
        {no,F1,St1} ->                          %Expanded all if flag set
            {F1,Env,St1}
    end.

%% pass_ewc(Forms, Env, State) -> {Env,State}.
%%  Pass over the list of forms which evaluate at compile
%%  time. Function and macro definitions are collected in the
%%  environment and other experssions are evaluated. The shell set
%%  forms are also specially recognised and the variables are bound
%%  and kept in the environment as well. The functions and macrso
%%  behave as in the shell.

pass_ewc(Fs, Env, St) ->
    mapfoldl2(fun (F, E, S) -> pass_ewc_form(F, E, S) end, Env, St, Fs).

pass_ewc_form(['progn'|Pfs0], Env0, St0) ->
    {Pfs1,Env1,St1} = pass_ewc(Pfs0, Env0, St0),
    {['progn'|Pfs1],Env1,St1};
pass_ewc_form(['eval-when-compile'|Efs0], Env0, St0) ->
    {Efs1,Env1,St1} = pass_ewc(Efs0, Env0, St0),
    {['progn'|Efs1],Env1,St1};
pass_ewc_form(['define-macro'|Def]=M, Env0, St0) ->
    %% Do we really want this? It behaves as a top-level macro def.
    case pass_define_macro(Def, Env0, St0) of
        {yes,Env1,St1} ->
            Ret = ?IF(St1#mac.keep, M, [progn]),
            {Ret,Env1,St1};                     %Don't macro expand now
        no ->
            St1 = add_error({bad_env_form,macro}, St0),
            {[progn],Env0,St1}                  %Just throw it away
    end;
pass_ewc_form(['define-function',Name,_,Def]=F, Env0, St0) ->
    case function_arity(Def) of
        {yes,Ar} ->                             %Definition not too bad
            Env1 = lfe_eval:add_dynamic_func(Name, Ar, Def, Env0),
            Ret = ?IF(St0#mac.keep, F, [progn]),
            {Ret,Env1,St0};                     %Don't macro expand now
        no ->                                   %Definition really bad
            St1 = add_error({bad_env_form,function}, St0),
            {[progn],Env0,St1}                  %Just throw it away
    end;
pass_ewc_form([set|Args], Env, St) ->
    pass_eval_set(Args, Env, St);
pass_ewc_form(F0, Env, St0) ->
    %% First expand enough to test top form, if so process again.
    case pass_expand_expr(F0, Env, St0, false) of
        {yes,F1,St1} ->                         %Top form expanded
            pass_ewc_form(F1, Env, St1);
        {no,F1,St1} ->                          %Not expanded
            try
                lfe_eval:expr(F1, Env),
                {['progn'],Env,St1}             %Ignore the value
            catch
                _:_ ->
                    {['progn'],Env,add_error({bad_env_form,expression}, St1)}
            end
    end.

function_arity([lambda,Args|_]) ->
    ?IF(lfe_lib:is_symb_list(Args), {yes,length(Args)}, no);
function_arity(['match-lambda',[Pat|_]|_]) ->
    ?IF(lfe_lib:is_proper_list(Pat), {yes,length(Pat)}, no);
function_arity(_) -> no.

%% pass_eval_set(Args, Env, State) -> {Set,Env,State}.
%%  Evaluate the set form.

pass_eval_set(Args, Env, St) ->
    try
        pass_eval_set_1(Args, Env, St)
    catch
        _:_ ->                                  %Catch everything
            {[progn],Env,add_error({bad_env_form,'set'}, St)}
    end.

pass_eval_set_1(Args, Env, St0) ->
    case exp_form(['let'|Args], Env, St0) of
        {['let',Pat,G,Exp],St1} ->
            pass_eval_set_1(Pat, [G], Exp, Env, St1);
        {['let',Pat,Exp],St1} ->
            pass_eval_set_1(Pat, [], Exp, Env, St1)
    end.                                        %Just crash here

pass_eval_set_1(Pat, Guard, Exp, Env0, St) ->
    Val = lfe_eval:expr(Exp, Env0),
    {yes,_,Bs} = lfe_eval:match_when(Pat, Val, Guard, Env0),
    Env1 = foldl(fun ({N,V}, E) -> add_vbinding(N, V, E) end, Env0, Bs),
    Sets = ?IF(St#mac.keep, [ [set,N,V] || {N,V} <- Bs ], []),
    {['progn'|Sets],Env1,St}.

%% pass_expand_expr(Expr, Env, State, DeepFlag) ->
%%     {yes,Exp,State} | {no,State}.
%%  Try to macro expand Expr, catch errors and return them in State.
%%  Only try to expand list expressions.

pass_expand_expr([_|_]=E0, Env, St0, Deep) ->
    try
        case exp_macro(E0, Env, St0) of
            {yes,_,_}=Yes -> Yes;
            no when Deep ->                     %Deep expand if flag set.
                {E1,St1} = exp_form(E0, Env, St0),
                {no,E1,St1};
            no -> {no,E0,St0}
        end
    catch
        _:Error -> {no,E0,add_error(Error, St0)}
    end;
pass_expand_expr(E, _, St, _) -> {no,E,St}.

%% pass_define_macro([Name,Meta,Def], Env, State) ->
%%     {yes,Env,State} | no.
%%  Add the macro definition to the environment. We do a small format
%%  check.

pass_define_macro([Name,_,Def], Env, St) ->
    case Def of
        ['lambda'|_] -> {yes,add_mbinding(Name, Def, Env),St};
        ['match-lambda'|_] -> {yes,add_mbinding(Name, Def, Env),St};
        _ -> no
    end.

%% add_error(Error, State) -> State.
%% add_error(Line, Error, State) -> State.
%% add_warning(Warning, State) -> State.
%% add_warning(Line, Warning, State) -> State.

add_error(E, St) -> add_error(St#mac.line, E, St).

add_error(L, E, St) ->
    St#mac{errors=St#mac.errors ++ [{L,?MODULE,E}]}.

%% add_warning(W, St) -> add_warning(St#mac.line, W, St).
%% add_warning(L, W, St) ->
%%     St#mac{warnings=St#mac.warnings ++ [{L,?MODULE,W}]}.

%% exp_form(Form, Env, State) -> {Form,State}.
%%  Completely expand a form using expansions in Env and pre-defined
%%  macros.  N.B. builtin core forms cannot be overidden and are
%%  handled here first. Some core forms also are particular about how
%%  their bodies are to be expanded and we handle these specially
%%  here. The rest we just expand the tail at the end.

%% Known Core forms which need special handling.
exp_form([quote,_]=Q, _, St) -> {Q,St};
exp_form([cons,H0,T0], Env, St0) ->
    {H1,St1} = exp_form(H0, Env, St0),
    {T1,St2} = exp_form(T0, Env, St1),
    {[cons,H1,T1],St2};
exp_form([car,E0], Env, St0) ->                 %Catch these to prevent
    {E1,St1} = exp_form(E0, Env, St0),          %redefining them
    {[car,E1],St1};
exp_form([cdr,E0], Env, St0) ->
    {E1,St1} = exp_form(E0, Env, St0),
    {[cdr,E1],St1};
exp_form([list|As], Env, St) ->
    exp_normal_core(list, As, Env, St);
exp_form([tuple|As], Env, St) ->
    exp_normal_core(tuple, As, Env, St);
exp_form([tref|[_,_]=As], Env, St) ->
    exp_normal_core(tref, As, Env, St);
exp_form([tset|[_,_,_]=As], Env, St) ->
    exp_normal_core(tset, As, Env, St);
exp_form([binary|As], Env, St) ->
    exp_normal_core(binary, As, Env, St);
exp_form([map|As], Env, St) ->
    exp_normal_core(map, As, Env, St);
exp_form([mref|As], Env, St) ->
    exp_normal_core(mref, As, Env, St);
exp_form([mset|As], Env, St) ->
    exp_normal_core(mset, As, Env, St);
exp_form([mupd|As], Env, St) ->
    exp_normal_core(mupd, As, Env, St);
exp_form(['map-get'|As], Env, St) ->
    exp_normal_core('map-get', As, Env, St);
exp_form(['map-set'|As], Env, St) ->
    exp_normal_core('map-set', As, Env, St);
exp_form(['map-update'|As], Env, St) ->
    exp_normal_core('map-update', As, Env, St);
exp_form([function|_]=F, _, St) -> {F,St};
%% Core closure special forms.
exp_form([lambda,Head|B], Env, St) ->
    exp_head_tail(lambda, Head, B, Env, St);
exp_form(['match-lambda'|B0], Env, St0) ->
    {B1,St1} = exp_ml_clauses(B0, Env, St0),
    {['match-lambda'|B1],St1};
exp_form(['let',Vbs|B], Env, St) ->
    exp_let(Vbs, B, Env, St);
exp_form(['let-function',Fbs|B], Env, St) ->
    exp_let_function(Fbs, B, Env, St);
exp_form(['letrec-function',Fbs|B], Env, St) ->
    exp_letrec_function(Fbs, B, Env, St);
exp_form(['let-macro',Mbs|B], Env, St) ->
    exp_let_macro(Mbs, B, Env, St);
%% Core control special forms.
exp_form([progn|As], Env, St) ->
    exp_normal_core(progn, As, Env, St);
exp_form(['if'|As], Env, St) ->
    exp_normal_core('if', As, Env, St);
exp_form(['case',E0|Cls0], Env, St0) ->
    {E1,St1} = exp_form(E0, Env, St0),
    {Cls1,St2} = exp_clauses(Cls0, Env, St1),
    {['case',E1|Cls1],St2};
exp_form(['receive'|Cls0], Env, St0) ->
    {Cls1,St1} = exp_clauses(Cls0, Env, St0),
    {['receive'|Cls1],St1};
exp_form(['catch'|B0], Env, St0) ->
    {B1,St1} = exp_tail(B0, Env, St0),
    {['catch'|B1],St1};
exp_form(['try',E|B], Env, St) ->
    exp_try(E, B, Env, St);
exp_form([funcall|As], Env, St) ->
    exp_normal_core(funcall, As, Env, St);
exp_form([call|As], Env, St) ->
    exp_normal_core(call, As, Env, St);
%% Core definition special forms.
exp_form(['eval-when-compile'|B], Env, St) ->
    exp_normal_core('eval-when-compile', B, Env, St);
exp_form(['define-function',Head|B], Env, St) ->
    exp_head_tail('define-function', Head, B, Env, St);
exp_form(['define-macro',Head|B], Env, St) ->
    exp_head_tail('define-macro', Head, B, Env, St);
%% These don't expand at all as name clashes are allowed.
exp_form(['define-module',_Mod|_]=Form, _, St) -> {Form,St};
exp_form(['extend-module'|_]=Form, _, St) -> {Form,St};
exp_form(['define-type',_Type|_]=Form, _, St) -> {Form,St};
exp_form(['define-opaque-type',_Type|_]=Form, _, St) -> {Form,St};
exp_form(['define-function-spec',_Func|_]=Form, _, St) -> {Form,St};
%% And don't forget when.
exp_form(['when'|G], Env, St) ->
    exp_normal_core('when', G, Env, St);
%% Now the case where we can have macros.
exp_form([Fun|_]=Call, Env, St0) when is_atom(Fun) ->
    %% Expand top macro as much as possible.
    case exp_macro(Call, Env, St0) of
        {yes,Exp,St1} -> exp_form(Exp, Env, St1);
        no -> exp_tail(Call, Env, St0)
    end;
exp_form([_|_]=Form, Env, St) -> exp_tail(Form, Env, St);
exp_form(Tup, _, St) when is_tuple(Tup) ->
    %% Should we expand this? We assume implicit quote here.
    {Tup,St};
%% Everything else is atomic.
exp_form(F, _, St) -> {F,St}.                   %Atomic

exp_normal_core(Name, As0, Env, St0) ->
    {As1,St1} = exp_tail(As0, Env, St0),
    {[Name|As1],St1}.

exp_head_tail(Name, Head, B0, Env, St0) ->
    {B1,St1} = exp_tail(B0, Env, St0),
    {[Name,Head|B1],St1}.

%% exp_list(Exprs, Env, State) -> {Exps,State}.
%%  Expand a proper list of exprs.

exp_list(Es, Env, St) ->
    mapfoldl(fun (E, S) -> exp_form(E, Env, S) end, St, Es).

%% exp_tail(Tail, Env, State) -> {Etail,State}.
%% exp_tail(ExpFun, Tail, Env, State) -> {Etail,State}.
%%  Expand the tail of a list, need not be a proper list.

exp_tail(Tail, Env, St) ->
    exp_tail(fun exp_form/3, Tail, Env, St).

exp_tail(Fun, [E0|Es0], Env, St0) ->
    {E1,St1} = Fun(E0, Env, St0),
    {Es1,St2} = exp_tail(Fun, Es0, Env, St1),
    {[E1|Es1],St2};
exp_tail(_, [], _, St) -> {[],St};
exp_tail(Fun, E, Env, St) -> Fun(E, Env, St).   %Same on improper tail.

%% exp_clauses(Clauses, Env, State) -> {ExpCls,State}.
%% exp_ml_clauses(Clauses, Env, State) -> {ExpCls,State}.
%%  Expand macros in clause patterns, guards and body. Must handle
%%  match-lambda clauses differently as pattern is an explicit list of
%%  patterns *NOT* a pattern which is a list. This will affect what is
%%  detected a macro call.

exp_clauses(Cls, Env, St) ->
    exp_tail(fun exp_clause/3, Cls, Env, St).

exp_clause([P0,['when'|G0]|B0], Env, St0) ->
    {P1,St1} = exp_form(P0, Env, St0),
    {G1,St2} = exp_tail(G0, Env, St1),
    {B1,St3} = exp_tail(B0, Env, St2),
    {[P1,['when'|G1]|B1],St3};
exp_clause([P0|B0], Env, St0) ->
    {P1,St1} = exp_form(P0, Env, St0),
    {B1,St2} = exp_tail(B0, Env, St1),
    {[P1|B1],St2};
exp_clause(Other, Env, St) -> exp_form(Other, Env, St).

exp_ml_clauses(Cls, Env, St) ->
    exp_tail(fun exp_ml_clause/3, Cls, Env, St).

exp_ml_clause([Ps0,['when'|G0]|B0], Env, St0) ->
    {Ps1,St1} = exp_tail(Ps0, Env, St0),
    {G1,St2} = exp_tail(G0, Env, St1),
    {B1,St3} = exp_tail(B0, Env, St2),
    {[Ps1,['when'|G1]|B1],St3};
exp_ml_clause([Ps0|B0], Env, St0) ->
    {Ps1,St1} = exp_tail(Ps0, Env, St0),
    {B1,St2} = exp_tail(B0, Env, St1),
    {[Ps1|B1],St2};
exp_ml_clause(Other, Env, St) -> exp_form(Other, Env, St).

%% exp_let(VarBindings, Body, Env, State) -> {Expansion,State}.
%%  We only do limited syntax checking here.

exp_let(Vbs0, B0, Env, St0) ->
    {Vbs1,St1} = exp_clauses(Vbs0, Env, St0),
    {B1,St2} = exp_tail(B0, Env, St1),
    {['let',Vbs1|B1],St2}.

%% exp_let_function(FuncBindings, Body, Env, State) -> {Expansion,State}.
%% exp_letrec_function(FuncBindings, Body, Env, State) -> {Expansion,State}.
%%  Expand a let/letrec-function. We do add them to the environment as
%%  they might be used when expanding macros.

exp_let_function(Fbs0, B0, Env, St0) ->
    {Fbs1,B1,St1} = do_exp_let_function('let-function', Fbs0, B0, Env, St0),
    {['let-function',Fbs1|B1],St1}.

exp_letrec_function(Fbs0, B0, Env, St0) ->
    {Fbs1,B1,St1} = do_exp_let_function('letrec-function', Fbs0, B0, Env, St0),
    {['letrec-function',Fbs1|B1],St1}.

do_exp_let_function(Type, Fbs0, B0, Env0, St0) ->
    %% Only very limited syntax checking here (see above).
    Efun = fun ([V,Def], {Env,St}) when is_atom(V) ->
                   case function_arity(Def) of
                       {yes,Ar} ->
                           {lfe_eval:add_dynamic_func(V, Ar, Def, Env),St};
                       no ->
                           {Env,add_error(St#mac.line, {bad_form,Type}, St)}
                   end;
               (_, {Env,St}) ->
                   {Env,add_error(St#mac.line, {bad_form,Type}, St)}
           end,
    {Env1,St1} = foldl(Efun, {Env0,St0}, Fbs0),
    {Fbs1,St2} = exp_clauses(Fbs0, Env1, St1),
    {B1,St3} = exp_tail(B0, Env1, St2),
    {Fbs1,B1,St3}.

%% exp_let_macro(MacroBindings, Body, Env, State) -> {Expansion,State}.
%%  Expand a let_syntax. We add the actual macro binding to the env as
%%  we may need them while expanding the body.

exp_let_macro(Mbs, B0, Env0, St0) ->
    %% Add the macro defs from expansion and return body in a progn.
    Env1 = foldl(fun ([Name,['lambda'|_]=Def], Env) when is_atom(Name) ->
                         add_mbinding(Name, Def, Env);
                     ([Name,['match-lambda'|_]=Def], Env) when is_atom(Name) ->
                         add_mbinding(Name, Def, Env);
                     (_, Env) -> Env            %Ignore mistakes
                 end, Env0, Mbs),
    {B1,St1} = exp_tail(B0, Env1, St0),         %Expand the body
    {['progn'|B1],St1}.

exp_try(E0, B0, Env, St0) ->
    {E1,St1} = exp_form(E0, Env, St0),
    {B1,St2} = exp_tail(fun (['case'|Cls0], E, Sta) ->
                                {Cls1,Stb} = exp_clauses(Cls0, E, Sta),
                                {['case'|Cls1],Stb};
                            (['catch'|Cls0], E, Sta) ->
                                {Cls1,Stb} = exp_clauses(Cls0, E, Sta),
                                {['catch'|Cls1],Stb};
                            (['after'|A0], E, Sta) ->
                                {A1,Stb} = exp_tail(A0, E, Sta),
                                {['after'|A1],Stb};
                            (Other, _, St) -> {Other,St}
                        end, B0, Env, St1),
    {['try',E1|B1],St2}.

%% exp_macro(Call, Env, State) -> {yes,Exp,State} | no.
%%  Expand the macro in top call, but not if it is a core form.

exp_macro([Name|_]=Call, Env, St) ->
    case lfe_internal:is_core_form(Name) of
        true -> no;                             %Never expand core forms
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
        {Def1,St1} = exp_form(Def0, Env, St0),  %Expand definition
        Exp = lfe_eval:apply(Def1, [Args,Env], Env),
        {yes,Exp,St1}
    catch
        %% error:no_Error -> boom
        %% error:Error ->
        %%     Stack = erlang:get_stacktrace(),
        %%     erlang:error({expand_macro,[Mac|Args],{Error,Stack}})
        error:Error ->
            Stack = erlang:get_stacktrace(),
            erlang:raise(error, {expand_macro,[Mac|Args],Error}, Stack)
        %% error:Error ->
        %%     Stack0 = erlang:get_stacktrace(),
        %%     Stack1 = trim_stacktrace(Stack0),
        %%     erlang:error({expand_macro,[Mac|Args],{Error,Stack1}})
    end.

%% exp_predef_macro(Call, Env, State) -> {yes,Exp,State} | no.
%%  Evaluate predefined macro definition catching errors.

exp_predef_macro(Call, Env, St) ->
    %%lfe_io:format("pdef: ~p\n", [Call]),
    try
        exp_predef(Call, Env, St)
    catch
        %% error:Error ->
        %%     Stack = erlang:get_stacktrace(),
        %%     erlang:raise({expand_macro,Call,{Error,Stack}})
        error:Error ->
            Stack = erlang:get_stacktrace(),
            erlang:raise(error, {expand_macro,Call,Error}, Stack)
        %% error:Error ->
        %%     Stack0 = erlang:get_stacktrace(),
        %%     Stack1 = trim_stacktrace(Stack0),
        %%     erlang:error({expand_macro,Call,{Error,Stack1}})
    end.

%% trim_stacktrace([{lfe_macro,_,_,_}=S|_]) -> [S];    %R15 and later
%% trim_stacktrace([{lfe_macro,_,_}|_]=S) -> [S];      %Pre R15
%% trim_stacktrace([S|Stk]) -> [S|trim_stacktrace(Stk)];
%% trim_stacktrace([]) -> [].

%% exp_predef(Form, Env, State) -> {yes,Form,State} | no.
%%  Expand the built-in predefined macros completely at top-level
%%  without returning a new predefined top-level macro. This make the
%%  macros "safe" even if they have been redefined as it is this
%%  definition which is used.

%% Builtin default macro expansions.
exp_predef([caar,E], _, St) -> {yes,[car,[car,E]],St};
exp_predef([cadr,E], _, St) -> {yes,[car,[cdr,E]],St};
exp_predef([cdar,E], _, St) -> {yes,[cdr,[car,E]],St};
exp_predef([cddr,E], _, St) -> {yes,[cdr,[cdr,E]],St};
%% More c*r macros, a la CL HyperSpec.
exp_predef([caaar,E], _, St) -> {yes,[car,[car,[car,E]]],St};
exp_predef([caadr,E], _, St) -> {yes,[car,[car,[cdr,E]]],St};
exp_predef([cadar,E], _, St) -> {yes,[car,[cdr,[car,E]]],St};
exp_predef([caddr,E], _, St) -> {yes,[car,[cdr,[cdr,E]]],St};
exp_predef([cdaar,E], _, St) -> {yes,[cdr,[car,[car,E]]],St};
exp_predef([cdadr,E], _, St) -> {yes,[cdr,[car,[cdr,E]]],St};
exp_predef([cddar,E], _, St) -> {yes,[cdr,[cdr,[car,E]]],St};
exp_predef([cdddr,E], _, St) -> {yes,[cdr,[cdr,[cdr,E]]],St};
%% Six-letter c*r macros from the CL HyperSpec.
exp_predef([caaaar,E], _, St) -> {yes,[car,[car,[car,[car,E]]]],St};
exp_predef([caaadr,E], _, St) -> {yes,[car,[car,[car,[cdr,E]]]],St};
exp_predef([caadar,E], _, St) -> {yes,[car,[car,[cdr,[car,E]]]],St};
exp_predef([caaddr,E], _, St) -> {yes,[car,[car,[cdr,[cdr,E]]]],St};
exp_predef([cadaar,E], _, St) -> {yes,[car,[cdr,[car,[car,E]]]],St};
exp_predef([cadadr,E], _, St) -> {yes,[car,[cdr,[car,[cdr,E]]]],St};
exp_predef([caddar,E], _, St) -> {yes,[car,[cdr,[cdr,[car,E]]]],St};
exp_predef([cadddr,E], _, St) -> {yes,[car,[cdr,[cdr,[cdr,E]]]],St};
exp_predef([cdaaar,E], _, St) -> {yes,[cdr,[car,[car,[car,E]]]],St};
exp_predef([cdaadr,E], _, St) -> {yes,[cdr,[car,[car,[cdr,E]]]],St};
exp_predef([cdadar,E], _, St) -> {yes,[cdr,[car,[cdr,[car,E]]]],St};
exp_predef([cdaddr,E], _, St) -> {yes,[cdr,[car,[cdr,[cdr,E]]]],St};
exp_predef([cddaar,E], _, St) -> {yes,[cdr,[cdr,[car,[car,E]]]],St};
exp_predef([cddadr,E], _, St) -> {yes,[cdr,[cdr,[car,[cdr,E]]]],St};
exp_predef([cdddar,E], _, St) -> {yes,[cdr,[cdr,[cdr,[car,E]]]],St};
exp_predef([cddddr,E], _, St) -> {yes,[cdr,[cdr,[cdr,[cdr,E]]]],St};

%% Arithmetic operations and comparison operations.
%% Be careful to make these behave as if they were a function and
%% strictly evalated all their arguments.
exp_predef(['+'|Es], _, St0) ->
    case Es of
        [] -> {yes,0,St0};                      %Identity
        _ ->
            {Exp,St1} = exp_arith(Es, '+', St0),
            {yes,Exp,St1}
    end;
exp_predef(['-'|Es], _, St0) ->
    case Es of
        [_|_] ->                                %Non-empty argument list
            {Exp,St1} = exp_arith(Es, '-', St0),
            {yes,Exp,St1}
    end;
exp_predef(['*'|Es], _, St0) ->
    case Es of
        [] -> {yes,1,St0};                      %Identity
        [_] -> {yes,exp_bif('*', [1|Es]),St0};  %Check if number
        _ ->
            {Exp,St1} = exp_arith(Es, '*', St0),
            {yes,Exp,St1}
    end;
exp_predef(['/'|Es], _, St0) ->
    case Es of
        [_] -> {yes,exp_bif('/', [1|Es]),St0};  %According to definition
        _ ->
            {Exp,St1} = exp_arith(Es, '/', St0),
            {yes,Exp,St1}
    end;
%% Logical operators.
exp_predef([Op|Es], _, St0)
  when Op =:= 'and'; Op =:= 'or'; Op =:= 'xor' ->
    {Exp,St1} = exp_logical(Es, Op, St0),
    {yes,Exp,St1};
%% Comparison operators.
exp_predef(['!='|Es], Env, St) -> exp_predef(['/='|Es], Env, St);
exp_predef(['==='|Es], Env, St) -> exp_predef(['=:='|Es], Env, St);
exp_predef(['!=='|Es], Env, St) -> exp_predef(['=/='|Es], Env, St);
exp_predef([Op|Es], _, St0) when Op == '/=' ; Op == '=/=' ->
    {Exp,St1} = exp_nequal(Es, Op, St0),
    {yes,Exp,St1};
exp_predef([Op|Es], _, St0)
  when Op =:= '>'; Op =:= '>='; Op =:= '<'; Op =:= '=<';
       Op =:= '=='; Op =:= '=:=' ->
    case Es of
        [_|_] ->
            {Exp,St1} = exp_comp(Es, Op, St0),
            {yes,Exp,St1}
    end;
exp_predef([backquote,Bq], _, St) ->            %We do this here.
    {yes,exp_backquote(Bq),St};
exp_predef(['++'|Abody], _, St) ->
    Exp = exp_append(Abody),
    {yes,Exp,St};
exp_predef(['++*'|Abody], _, St) ->
    Exp = exp_prefix(Abody),
    {yes,Exp,St};
exp_predef(['?'|As], _, St) ->
    Omega = [omega,omega],
    Exp = case As of
              [To,Def] -> ['receive',Omega,['after',To,Def]];
              [To] -> ['receive',Omega,['after',To,[exit,?Q(timeout)]]];
              [] -> ['receive',Omega]
          end,
    {yes,Exp, St};
exp_predef(['list*'|As], _, St) ->
    Exp = exp_list_star(As),
    {yes,Exp,St};
exp_predef(['let*'|Lbody], _, St) ->
    Exp = exp_let_star(Lbody),
    {yes,Exp,St};
exp_predef(['flet*'|Lbody], _, St) ->
    Exp = exp_flet_star(Lbody),
    {yes,Exp,St};
exp_predef(['cond'|Cbody], _, St) ->
    Exp = exp_cond(Cbody),
    {yes,Exp,St};
exp_predef(['do'|Dbody], _, St0) ->
    {Exp,St1} = exp_do(Dbody, St0),
    {yes,Exp,St1};
exp_predef([lc|Lbody], _, St0) ->
    %% (lc (qual ...) e ...)
    [Qs|Es] = Lbody,
    {Exp,St1} = lc_te(Es, Qs, St0),
    {yes,Exp,St1};
%% Add an alias for lc.
exp_predef(['list-comp'|Lbody], _, St0) ->
    [Qs|Es] = Lbody,
    {Exp,St1} = lc_te(Es, Qs, St0),
    {yes,Exp,St1};
exp_predef([bc|Bbody], _, St0) ->
    %% (bc (qual ...) e ...)
    [Qs|Es] = Bbody,
    {Exp,St1} = bc_te(Es, Qs, St0),
    {yes,Exp,St1};
%% Add an alias for bc.
exp_predef(['binary-comp'|Bbody], _, St0) ->
    [Qs|Es] = Bbody,
    {Exp,St1} = bc_te(Es, Qs, St0),
    {yes,Exp,St1};
exp_predef(['andalso'|Abody], _, St) ->
    Exp = exp_andalso(Abody),
    {yes,Exp,St};
exp_predef(['orelse'|Obody], _, St) ->
    Exp = exp_orelse(Obody),
    {yes,Exp,St};
%% The fun forms assume M, F and Ar are atoms and integer. We leave
%% them as before for backwards compatibility.
exp_predef(['fun',F,Ar], _, St0) ->
    {Vs,St1} = new_symbs(Ar, St0),
    {yes,['lambda',Vs,[F|Vs]],St1};
exp_predef(['fun',M,F,Ar], _, St0) ->
    {Vs,St1} = new_symbs(Ar, St0),
    {yes,['lambda',Vs,['call',?Q(M),?Q(F)|Vs]],St1};
exp_predef(['defrecord'|Def], Env, St) ->
    lfe_macro_record:define(Def, Env, St);
%% Include-XXX as macros for now. Move to top-level forms?
exp_predef(['include-file'|Ibody], Env, St) ->
    lfe_macro_include:file(Ibody, Env, St);
exp_predef(['include-lib'|Ibody], Env, St) ->
    lfe_macro_include:lib(Ibody, Env, St);
%% Compatibility macros for the older Scheme like syntax.
exp_predef(['begin'|Body], _, St) ->
    {yes,['progn'|Body],St};
exp_predef(['define',Head|Body], _, St) ->
    %% Let the lint catch errors here.
    Exp = case lfe_lib:is_symb_list(Head) of
              true ->
                  ['define-function',hd(Head),[],[lambda,tl(Head)|Body]];
              false ->
                  ['define-function',Head,[],Body]
          end,
    {yes,Exp,St};
exp_predef(['define-record'|Def], _, St) ->
    {yes,[defrecord|Def],St};
exp_predef(['define-syntax',Name,Def], _, St) ->
    {Meta,Mdef} = exp_syntax(Name, Def),
    {yes,['define-macro',Name,Meta,Mdef],St};
exp_predef(['let-syntax',Defs|Body], _, St) ->
    Fun = fun ([Name,Def]) ->
                  {_,Def} = exp_syntax(Name, Def),
                  [Name,Def]
          end,
    Mdefs = map(Fun, Defs),
    {yes,['let-macro',Mdefs|Body],St};
%% Common Lisp inspired macros.
exp_predef([defmodule,Name|Rest], _, St) ->
    %% Need to handle parametrised module defs here. Limited checking.
    Mname = case Name of
                [Mod|_] -> Mod;                 %Parametrised module
                Mod -> Mod                      %Normal module
            end,
    MODULE = [defmacro,'MODULE',[],?BQ(?Q(Mname))],
    {Meta,Atts} = exp_defmodule(Rest),
    {yes,[progn,['define-module',Name,Meta,Atts],MODULE],St#mac{module=Mname}};
exp_predef([deftype,Type0|Def0], _, St) ->
    {Type1,Def1} = exp_deftype(Type0, Def0),
    {yes,['define-type',Type1,Def1],St};
exp_predef([defopaque,Type0|Def0], _, St) ->
    {Type1,Def1} = exp_deftype(Type0, Def0),
    {yes,['define-opaque-type',Type1,Def1],St};
exp_predef([defspec,Func0|Spec0], _, St) ->
    {Func1,Spec1} = exp_defspec(Func0, Spec0),
    {yes,['define-function-spec',Func1,Spec1],St};
exp_predef([defun,Name|Rest], _, St) ->
    %% Educated guess whether traditional (defun name (a1 a2 ...) ...)
    %% or matching (defun name (patlist1 ...) (patlist2 ...))
    {Meta,Def} = exp_defun(Rest),
    {yes,['define-function',Name,Meta,Def],St};
exp_predef([defmacro,Name|Rest], _, St) ->
    %% Educated guess whether traditional (defmacro name (a1 a2 ...) ...)
    %% or matching (defmacro name (patlist1 ...) (patlist2 ...))
    {Meta,Def} = exp_defmacro(Rest),
    {yes,['define-macro',Name,Meta,Def],St};
exp_predef([defsyntax,Name|Rules], _, St) ->
    {Meta,Def} = exp_rules(Name, [], Rules),
    {yes,['define-macro',Name,Meta,Def],St};
exp_predef([flet,Defs|Body], _, St) ->
    Fun = fun ([Name|Rest]) ->
                  {_,Def} = exp_defun(Rest),    %Ignore meta data
                  [Name,Def]
          end,
    Fdefs = map(Fun, Defs),
    {yes,['let-function',Fdefs|Body], St};
exp_predef([fletrec,Defs|Body], _, St) ->
    Fun = fun ([Name|Rest]) ->
                  {_,Def} = exp_defun(Rest),    %Ignore meta data
                  [Name,Def]
          end,
    Fdefs = map(Fun, Defs),
    {yes,['letrec-function',Fdefs|Body], St};
exp_predef([macrolet,Defs|Body], _, St) ->
    Fun = fun ([Name|Rest]) ->
                  {_,Def} = exp_defmacro(Rest), %Ignore meta data
                  [Name,Def]
          end,
    Mdefs = map(Fun, Defs),
    {yes,['let-macro',Mdefs|Body],St};
exp_predef([syntaxlet,Defs|Body], _, St) ->
    Fun = fun ([Name|Rest]) ->
                  {_,Def} = exp_rules(Name, [], Rest),
                  [Name,Def]
          end,
    Mdefs = map(Fun, Defs),
    {yes,['let-macro',Mdefs|Body],St};
exp_predef([prog1|Body], _, St0) ->
    %% We do a simple optimisation here.
    case Body of                                %Catch bad form here
        [Expr] -> {yes,Expr,St0};
        [First|Rest] ->
            {V,St1} = new_symb(St0),
            {yes,['let',[[V,First]]|Rest ++ [V]],St1}
    end;
exp_predef([prog2|Body], _, St) ->
    [First|Rest] = Body,                        %Catch bad form here
    {yes,[progn,First,[prog1|Rest]],St};
%% This has to go here for the time being so as to be able to macro
%% expand body.
exp_predef(['match-spec'|Body], Env, St0) ->
    %% Expand it like a match-lambda.
    {Exp,St1} = exp_ml_clauses(Body, Env, St0),
    MS = lfe_ms:expand(Exp),
    {yes,MS,St1};
%% (qlc (lc (qual ...) e ...) opts)
exp_predef([qlc,LC], Env, St) -> exp_qlc(LC, [], Env, St);
exp_predef([qlc,LC,Opts], Env, St) -> exp_qlc(LC, [Opts], Env, St);
%% Some predefined file macros.
exp_predef(['MODULE'], _, St) ->
    {yes,?Q(St#mac.module),St};
exp_predef(['LINE'], _, St) ->
    {yes,?Q(St#mac.line),St};
exp_predef([':',M,F|As], Env, St0) when is_atom(M), is_atom(F) ->
    case exp_call_macro(M, F, As, Env, St0) of
        {yes,_,_}=Yes -> Yes;                   %{yes,Exp,St}
        {no,St1} ->                             %Use the default expansion
            {yes,['call',?Q(M),?Q(F)|As], St1}
    end;
exp_predef([':',M,F|As], _, St) ->
    %% Catch the other junk here.
    {yes,['call',?Q(M),?Q(F)|As], St};
exp_predef([Fun|As], _, St) when is_atom(Fun) ->
    case string:tokens(atom_to_list(Fun), ":") of
        [M,F] ->
            {yes,[':',list_to_atom(M),list_to_atom(F)|As],St};
        _ -> no                                 %This will also catch a:b:c
    end;
%% This was not a call to a predefined macro.
exp_predef(_, _, _) -> no.

%% exp_call_macro(Module, Name, Args, Env, State) ->
%%     {yes,From,State} | {no,State}.
%%  Expand macro in Module if it exists. Try to be smart and avoid
%%  loading a module, and trying to load a module, unneccessarily.

exp_call_macro(M, F, As, Env, St) ->
    case erlang:function_exported(M, 'LFE-EXPAND-EXPORTED-MACRO', 3) of
        true ->
            case M:'LFE-EXPAND-EXPORTED-MACRO'(F, As, Env) of
                {yes,Exp} -> {yes,Exp,St};
                no -> {no,St}
            end;
        false ->
            %% Slightly faster code:ensure_loaded/1.
            case erlang:module_loaded(M) of
                true -> {no,St};                %Module loaded but no macros
                false ->
                    Unl = St#mac.unloadable,
                    case lists:member(M, Unl) of
                        true -> {no,St};        %Can't load this module
                        false ->
                            %% Try loading file and try again.
                            case code:load_file(M) of
                                {module,_} -> exp_call_macro(M, F, As, Env, St);
                                {error,_} ->
                                    %% Echo modules we couldn't load
                                    %%lfe_io:format("ecp: ~p\n", [{M,Unl}]),
                                    St1 = St#mac{unloadable=[M|Unl]},
                                    {no,St1}
                            end
                    end
            end
    end.

%% exp_qlc(LC, Opts, Env, State) -> {yes,Expansion,State}.
%% Expand a Query List Comprehension returning a call to qlc:q/2. We
%% first convert the LC into vanilla erlang AST, expand it using in
%% lfe_qlc.erl, which ql_pt.erl with a special interface, then convert
%% it back to LFE.

exp_qlc([lc,Qs|Es], Opts, Env, St0) ->
    %% Expand macros in the LC before translating it preserving
    %% structure.
    {Eqs,St1} = exp_qlc_quals(Qs, Env, St0),
    {Ees,St2} = exp_list(Es, Env, St1),
    %% lfe_io:format("Q0 = ~p\n", [[lc,Eqs|Ees]]),
    %% Now translate to vanilla AST, call qlc expand and then convert
    %% back to LFE.  lfe_qlc:expand/2 wants a list of conversions not
    %% a conversion of a list.
    Vlc = lfe_trans:to_expr([lc,Eqs|Ees], 42),
    %% lfe_io:format("~w\n", [Vlc]),
    Vos = map(fun (O) -> lfe_trans:to_expr(O, 42) end, Opts),
    %% io:put_chars(["E0 = ",erl_pp:expr(Vlc, 5, []),"\n"]),
    {ok,Vexp} = lfe_qlc:expand(Vlc, Vos),
    %% io:put_chars([erl_pp:expr(Vexp),"\n"]),
    Exp = lfe_trans:from_expr(Vexp),
    %% lfe_io:format("Q1 = ~p\n", [Exp]),
    {yes,Exp,St2}.

exp_qlc_quals(Qs, Env, St) ->
    mapfoldl(fun (Q, S) -> exp_qlc_qual(Q, Env, S) end, St, Qs).

exp_qlc_qual(['<-',P0,['when'|G0],E0], Env, St0) ->
    {P1,St1} = exp_form(P0, Env, St0),
    {G1,St2} = exp_tail(G0, Env, St1),
    {E1,St3} = exp_form(E0, Env, St2),
    {['<-',P1,['when'|G1],E1],St3};
exp_qlc_qual(['<-',P0,E0], Env, St0) ->
    {P1,St1} = exp_form(P0, Env, St0),
    {E1,St2} = exp_form(E0, Env, St1),
    {['<-',P1,E1],St2};
exp_qlc_qual(T, Env, St) -> exp_form(T, Env, St).

%% exp_bif(Bif, Args) -> Expansion.

exp_bif(B, As) -> [call,?Q(erlang),?Q(B)|As].

%% exp_args(Args, State) -> {LetBinds,State}.
%%  Expand Args into a list of let bindings suitable for a let* or
%%  nested lets to force sequential left-to-right evaluation.

exp_args(As, St) ->
    mapfoldl(fun (A, St0) -> {V,St1} = new_symb(St0), {[V,A],St1} end, St, As).

%% exp_arith(Args, Op, State) -> {Exp,State}.
%%  Expand arithmetic call strictly forcing evaluation of all
%%  arguments.  Note that single argument version may need special
%%  casing.

exp_arith([A], Op, St) -> {exp_bif(Op, [A]),St};
exp_arith([A,B], Op, St) -> {exp_bif(Op, [A,B]),St};
exp_arith(As, Op, St0) ->
    {Ls,St1} = exp_args(As, St0),
    B = foldl(fun ([V,_], Acc) -> exp_bif(Op, [Acc,V]) end, hd(hd(Ls)), tl(Ls)),
    {exp_let_star([Ls,B]),St1}.

%% exp_logical(Args, Op State) -> {Exp,State}.
%%  Expand logical call forcing evaluation of all arguments but not
%%  strictly; this guarantees expansion is hygenic.  Note that single
%%  argument version may need special casing.

exp_logical([A], Op, St) -> {exp_bif(Op, [A,?Q(true)]),St};
exp_logical([A,B], Op, St) -> {exp_bif(Op, [A,B]),St};
exp_logical(As, Op, St0) ->
    {Ls,St1} = exp_args(As, St0),
    B = foldl(fun ([V,_], Acc) -> exp_bif(Op, [Acc,V]) end, hd(hd(Ls)), tl(Ls)),
    {['let',Ls,B],St1}.

%% exp_comp(Args, Op, State) -> {Exp,State}.
%%  Expand comparison test strictly forcing evaluation of all
%%  arguments. Note that single argument version may need special
%%  casing.

exp_comp([A], _, St) ->            %Force evaluation
    {[progn,A,?Q(true)],St};
exp_comp([A,B], Op, St) -> {exp_bif(Op, [A,B]),St};
exp_comp(As, Op, St0) ->
    {Ls,St1} = exp_args(As, St0),
    Ts = op_pairs(Ls, Op),
    {exp_let_star([Ls,exp_andalso(Ts)]),St1}.

op_pairs([[V0,_]|Ls], Op) ->
    element(1, mapfoldl(fun ([V1,_], Acc) -> {exp_bif(Op, [Acc,V1]),V1} end,
                        V0, Ls)).

%% exp_nequal(Args, Op, State) -> {Exp,State}.
%%  Expand not equal test strictly forcing evaluation of all
%%  arguments. We need to compare all the arguments with each other.

exp_nequal([A], _, St) ->            %Force evaluation
    {[progn,A,?Q(true)],St};
exp_nequal([A,B], Op, St) -> {exp_bif(Op, [A,B]),St};
exp_nequal(As, Op, St0) ->
    {Ls,St1} = exp_args(As, St0),
    Ts = op_all_pairs(Ls, Op),
    {exp_let_star([Ls,exp_andalso(Ts)]),St1}.

op_all_pairs([], _) -> [];
op_all_pairs([[V,_]|Ls], Op) ->
    [ exp_bif(Op, [V,V1]) || [V1,_] <- Ls] ++ op_all_pairs(Ls, Op).

%% exp_append(Args) -> Expansion.
%%  Expand ++ in such a way as to allow its use in patterns. There are
%%  a lot of interesting cases here. Only be smart with proper forms.

exp_append(Args) ->
    case Args of
        %% Cases with quoted lists.
        [?Q([A|As])|Es] -> [cons,?Q(A),exp_append([?Q(As)|Es])];
        [?Q([])|Es] -> exp_append(Es);
        %% Cases with explicit cons/list/list*.
        [['list*',A]|Es] -> exp_append([A|Es]);
        [['list*',A|As]|Es] -> [cons,A,exp_append([['list*'|As]|Es])];
        [[list,A|As]|Es] -> [cons,A,exp_append([[list|As]|Es])];
        [[list]|Es] -> exp_append(Es);
        [[cons,H,T]|Es] -> [cons,H,exp_append([T|Es])];
        [[]|Es] -> exp_append(Es);
        %% Cases with lists of numbers (strings).
        %% [[N|Ns]|Es] when is_number(N) -> [cons,N,exp_append([Ns|Es])];
        %% Default cases with unquoted arg.
        [E] -> E;                %Last arg not checked
        [E|Es] -> exp_bif('++', [E,exp_append(Es)]);
        [] -> []
    end.

%% exp_prefix(Args) -> Expansion.
%%  Expand ++* in such a way as to allow its use in patterns.
%%  Handle lists of numbers (strings) explicitly, otherwise
%%  default to exp_append/1.

exp_prefix([['list*',A]|Es]) -> exp_prefix([A|Es]);
exp_prefix([['list*',A|As]|Es]) -> [cons,A,exp_prefix([['list*'|As]|Es])];
exp_prefix([[list,A|As]|Es]) -> [cons,A,exp_prefix([[list|As]|Es])];
exp_prefix([[list]|Es]) -> exp_prefix(Es);
exp_prefix([[cons,H,T]|Es]) -> [cons,H,exp_prefix([T|Es])];
exp_prefix([[N|Ns]|Es]) when is_number(N) -> [cons,N,exp_prefix([Ns|Es])];
exp_prefix([[]|Es]) -> exp_prefix(Es);
exp_prefix(Args) -> exp_append(Args).

%% exp_list_star(ListBody) -> Cons.

exp_list_star([E]) -> E;
exp_list_star([E|Es]) ->
    [cons,E,exp_list_star(Es)];
exp_list_star([]) -> [].

%% exp_let_star(FletBody) -> Flets.

exp_let_star([[Vb|Vbs]|B]) ->
    ['let',[Vb],exp_let_star([Vbs|B])];
exp_let_star([[]|B]) -> [progn|B];
exp_let_star([Vb|B]) -> ['let',Vb|B].           %Pass error to let for lint.

%% exp_flet_star(FletBody) -> Flets.

exp_flet_star([[Fb|Fbs]|B]) ->
    [flet,[Fb],exp_flet_star([Fbs|B])];
exp_flet_star([[]|B]) -> [progn|B];
exp_flet_star([Fb|B]) -> [flet,Fb|B].           %Pass error to flet for lint

%% exp_cond(CondBody) -> Tests.
%%  Expand a cond body to a sequence of if/case tests.

exp_cond([['else'|B]]) -> [progn|B];
exp_cond([[['?=',P,E]|B]|Cond]) ->
    ['case',E,[P|B],['_',exp_cond(Cond)]];
exp_cond([[['?=',P,['when'|_]=G,E]|B]|Cond]) ->
    ['case',E,[P,G|B],['_',exp_cond(Cond)]];
exp_cond([[Test|B]|Cond]) ->                    %Test and body
    ['if',Test,[progn|B],exp_cond(Cond)];
exp_cond([Test|Cond]) ->                        %Naked test
    ['if',Test,?Q(true),exp_cond(Cond)];
exp_cond([]) -> ?Q(false).

%% exp_do(DoBody) -> DoLoop.
%%  Expand a do body into a loop. Add a variable 'do-state' which is
%%  the value of the do body which can be used when setting new values
%%  to do vars.

exp_do([Pars,[Test,Ret]|Body], St0) ->
    {Vs,Is,Cs} = foldr(fun ([V,I,C], {Vs,Is,Cs}) -> {[V|Vs],[I|Is],[C|Cs]} end,
                       {[],[],[]}, Pars),
    {Fun,St1} = new_fun_name("do", St0),
    Exp = ['letrec-function',
           [[Fun,[lambda,Vs,
                  ['if',Test,Ret,
		   ['let',[['do-state',
			    ['progn'] ++ Body]],
		    [Fun|Cs]]]]]],
	   [Fun|Is]],
    {Exp,St1}.

%% exp_andalso(AndAlsoBody) -> Ifs.
%% exp_orelse(OrElseBody) -> Ifs.

exp_andalso([E]) -> E;                          %Let user check last call
exp_andalso([E|Es]) ->
    ['if',E,exp_andalso(Es),?Q(false)];
exp_andalso([]) -> ?Q(true).

exp_orelse([E]) -> E;                           %Let user check last call
exp_orelse([E|Es]) -> ['if',E,?Q(true),exp_orelse(Es)];
exp_orelse([]) -> ?Q(false).

%% exp_defmodule(Rest) -> {Meta,Attributes}.
%%  Extract the comment string either if it is first. Ignore 'doc'
%%  attributes. Allow empty module definition.

exp_defmodule([]) -> {[],[]};
exp_defmodule([Doc|Atts]=Rest) ->
    ?IF(lfe_lib:is_doc_string(Doc), {[[doc,Doc]],Atts}, {[],Rest}).

%% exp_deftype(Type, Def) -> {Type,Def}.
%%  Paramterless types to be written as just type name and default
%%  type is any.

exp_deftype(T, D) ->
    Type = if is_list(T) -> T; true -> [T] end,
    Def = if D =:= [] -> [any]; true -> hd(D) end,
    {Type,Def}.

%% exp_defspec(Func, Def) -> {Func,Def}.
%%  Do very little here, leave it to lint

exp_defspec([_,_]=Func, Def) -> {Func,Def};
exp_defspec(Name, Def) ->
    {[Name,defspec_arity(Def)],Def}.

%% defspec_arity(Spec) -> Arity.
%%  Just return the length of the first arg list and let lint check
%%  properly later.

defspec_arity([[Args|_]|_]) ->
    case lfe_lib:is_proper_list(Args) of
        true -> length(Args);
        false -> 0
    end;
defspec_arity(_) -> 0.

%% exp_defun(Rest) -> {Meta,Lambda | MatchLambda}.
%%  Educated guess whether traditional (defun name (a1 a2 ...) ...)
%%  or matching (defun name (patlist1 ...) (patlist2 ...)) and whether
%%  there is a comment string.

exp_defun([Args|Body]=Rest) ->
    case lfe_lib:is_symb_list(Args) of
        true  -> exp_lambda_defun(Args, Body);
        false -> exp_match_defun(Rest)
    end.

exp_lambda_defun(Args, Body) ->
    {Meta,Def} = exp_meta(Body, []),
    {Meta,['lambda',Args|Def]}.

exp_match_defun(Rest) ->
    {Meta,Cls} = exp_meta(Rest, []),
    {Meta,['match-lambda'|Cls]}.

exp_meta([[spec|Spec]|Rest], Meta) ->
    exp_meta(Rest, Meta ++ [[spec|Spec]]);
exp_meta([Doc|Rest], Meta) ->
    %% The untagged doc string but not at the end.
    ?IF(lfe_lib:is_doc_string(Doc) and (Rest =/= []),
        exp_meta(Rest, Meta ++ [[doc,Doc]]),
        {Meta,[Doc|Rest]});
exp_meta([], Meta) -> {Meta,[]}.

%% exp_defmacro(Rest) -> {Meta,MatchLambda}.
%%  Educated guess whether traditional (defmacro name (a1 a2 ...) ...)
%%  or matching (defmacro name (patlist1 ...) (patlist2 ...)). Special
%%  case (defmacro name arg ...) to make arg be whole argument list.
%%  N.B. Macro definition is function of 2 arguments: the whole
%%  argument list of macro call; and $ENV, the current macro
%%  environment.

exp_defmacro([Args|Body]=Rest) ->
    {Meta,Cls} = case lfe_lib:is_symb_list(Args) of
                     true -> exp_lambda_defmacro([list|Args], Body);
                     false ->
                         if is_atom(Args) ->
                                 exp_lambda_defmacro(Args, Body);
                            true ->
                                 exp_match_defmacro(Rest)
                         end
                 end,
    {Meta,['match-lambda'|Cls]}.

exp_lambda_defmacro(Args, Body) ->
    {Meta,Def} = exp_meta(Body, []),
    {Meta,[[[Args,'$ENV']|Def]]}.

exp_match_defmacro(Rest) ->
    {Meta,Cls} = exp_meta(Rest, []),
    {Meta,map(fun ([Head|Body]) -> [[Head,'$ENV']|Body] end, Cls)}.

%% exp_syntax(Name, Def) -> {Meta,Lambda | MatchLambda}.
%%  N.B. New macro definition is function of 2 arguments, the whole
%%  argument list of macro call, and the current macro environment.

exp_syntax(Name, Def) ->
    case Def of
        [macro|Cls] ->
            Mcls = map(fun ([Pat|Body]) -> [[Pat,'$ENV']|Body] end, Cls),
            {[],['match-lambda'|Mcls]};
        ['syntax-rules'|Rules] ->
            exp_rules(Name, [], Rules)
    end.

%% exp_rules(Name, Keywords, Rules) -> {Meta,Lambda}.
%%  Expand into call function which expands macro an invocation time,
%%  this saves much space and costs us nothing.
%%  N.B. New macro definition is function of 2 arguments, the whole
%%  argument list of macro call, and the current macro environment.

exp_rules(Name, Keywords, Rules) ->
    {[],[lambda,[args,'$ENV'],
         [':',lfe_macro,mbe_syntax_rules_proc,
          [quote,Name],[quote,Keywords],[quote,Rules],args]]}.

%%  By Andr� van Tonder
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
%%  R6RS compliant and can handle comma (unquote) and comma-at
%%  (unquote-splicing) with more than one argument properly.  Actually
%%  with simple cons/append optimisers code now quite good.

exp_backquote(Exp) -> exp_backquote(Exp, 0).

exp_backquote([backquote,X], N) ->
    [list,[quote,backquote],exp_backquote(X, N+1)];
exp_backquote([comma|X], N) when N > 0 ->
    exp_bq_cons([quote,comma], exp_backquote(X, N-1));
exp_backquote([comma,X], 0) -> X;
exp_backquote(['comma-at'|X], N) when N > 0 ->
    exp_bq_cons([quote,'comma-at'], exp_backquote(X, N-1));
%% Next 2 handle case of splicing into a list.
exp_backquote([[comma|X]|Y], 0) ->
    exp_bq_append([list|X], exp_backquote(Y, 0));
exp_backquote([['comma-at'|X]|Y], 0) ->
    exp_bq_append(['++'|X], exp_backquote(Y, 0));
exp_backquote([X|Y], N) ->                      %The general list case
    exp_bq_cons(exp_backquote(X, N), exp_backquote(Y, N));
exp_backquote(X, N) when is_tuple(X) ->
    %% Straight [list_to_tuple,exp_backquote(tuple_to_list(X), N)]
    %% inefficient and [tuple|tl(exp_backquote(tuple_to_list(X), N))]
    %% can't handle splicing!
    case exp_backquote(tuple_to_list(X), N) of
        [list|Es] -> [tuple|Es];                %No splicing
        [cons|_]=E -> [list_to_tuple,E];        %Have splicing
        [] -> [tuple]                           %The empty tuple
    end;
exp_backquote(X, N) when ?IS_MAP(X) ->
    %% Splicing at top-level almost meaningless here, with [list|...]
    %% we have no splicing, while with [cons|...] we have splicing
    case exp_bq_map_pairs(maps:to_list(X), N) of
        [list|KVs] -> [map|KVs];                %No splicing
        %% [cons|_]=E ->                        %Have splicing
        %%      [call,?Q(maps),?Q(from_list)|E];
        [] -> [map]                             %The empty map
    end;
exp_backquote(X, _) when is_atom(X) -> [quote,X];
exp_backquote(X, _) -> X.                       %Self quoting

exp_bq_append(['++',L], R) ->                   %Catch single comma-at
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

-ifdef(HAS_MAPS).
exp_bq_map_pairs(Ps, N) ->
    KVs = foldr(fun ({K,V}, Acc) -> [K,V|Acc] end, [], Ps),
    exp_backquote(KVs, N).
-else.
exp_bq_map_pairs(_, _) -> [list].
-endif.

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
    is_atom(S) andalso not is_boolean(S).

%% Tests if ellipsis pattern, (p ... . rest)
%% is_mbe_ellipsis(?mbe_ellipsis(_, _)) -> true;
%% is_mbe_ellipsis(_) -> false.

mbe_match_pat([quote,P], E, _) -> P =:= E;
mbe_match_pat([tuple|Ps], [tuple|Es], Ks) ->    %Match tuple constructor
    mbe_match_pat(Ps, Es, Ks);
mbe_match_pat([tuple|Ps], E, Ks) ->             %Match literal tuple
    case is_tuple(E) of
        true -> mbe_match_pat(Ps, tuple_to_list(E), Ks);
        false -> false
    end;
mbe_match_pat(?mbe_ellipsis(Pcar, _), E, Ks) ->
    case lfe_lib:is_proper_list(E) of
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
mbe_get_bindings([tuple|Ps], [tuple|Es], Ks) ->    %Tuple constructor
    mbe_get_bindings(Ps, Es, Ks);
mbe_get_bindings([tuple|Ps], E, Ks) ->        %Literal tuple
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
                    case lfe:assoc(Pat, R) of
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
                       [':',lfe_macro,mbe_expand_pattern,
                        [quote,Outpat],r,Ksym]]]
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

c_tq(Exp, [['<-',P,Gen]|Qs], End, St) ->                %List generator
    c_l_tq(Exp, P, [], Gen, Qs, End, St);
c_tq(Exp, [['<-',P,['when'|G],Gen]|Qs], End, St) ->     %List generator
    c_l_tq(Exp, P, G, Gen, Qs, End, St);
c_tq(Exp, [['<=',P,Gen]|Qs], End, St) ->                %Bits generator
    c_b_tq(Exp, P, [], Gen, Qs, End, St);
c_tq(Exp, [['<=',P,['when'|G],Gen]|Qs], End, St) ->     %Bits generator
    c_b_tq(Exp, P, G, Gen, Qs, End, St);
c_tq(Exp, [['?=',P,E]|Qs], End, St0) ->                 %Test match
    {Rest,St1} = c_tq(Exp, Qs, End, St0),
    {['case',E,[P,Rest],['_',End]],St1};
c_tq(Exp, [['?=',P,['when'|_]=G,E]|Qs], End, St0) ->    %Test match
    {Rest,St1} = c_tq(Exp, Qs, End, St0),
    {['case',E,[P,G,Rest],['_',End]],St1};
c_tq(Exp, [T|Qs], End, St0) ->                          %Test
    {Rest,St1} = c_tq(Exp, Qs, End, St0),
    {['if',T,Rest,End],St1};
c_tq(Exp, [], End, St) ->                               %End of qualifiers
    Exp(End, St).

c_l_tq(Exp, P, G, Gen, Qs, End, St0) ->
    {H,St1} = new_fun_name("lc", St0),          %Function name
    {Us,St2} = new_symb(St1),                   %Tail variable
    {Rest,St3} = c_tq(Exp, Qs, [H,Us], St2),    %Do rest of qualifiers
    %% Build the match, no match and end clauses, no nomatch clause if
    %% pattern and guard guaranteed to match. Keeps compiler quiet.
    Cs0 = [ [[[]],End] ],                       %End of list
    Cs1 = case is_atom(P) and (G == []) of      %No match, skip
              true -> Cs0;
              false -> [ [[[cons,'_',Us]],[H,Us]] |Cs0]
          end,
    Cs2 = [ [[[cons,P,Us]],['when'|G],Rest] |Cs1], %Matches pattern and guard
    {['letrec-function',
      [[H,['match-lambda'|Cs2]]],
      [H,Gen]],St3}.

c_b_tq(Exp, P, G, Gen, Qs, End, St0) ->
    {H,St1} = new_fun_name("bc", St0),          %Function name
    {B,St2} = new_symb(St1),                    %Bin variable
    {Rest,St3} = c_tq(Exp, Qs, [H,B], St2),     %Do rest of qualifiers
    Brest = [B,bitstring,'big-endian',unsigned,[unit,1]], %,[size,all]
    %% Build the match and nomatch/end clauses.
    MatchC = [[[binary,P,Brest]],['when'|G],Rest],  %Matches pattern and guard
    EndC = [[[binary,Brest]],End],                  %No match
    {['letrec-function',
      [[H,['match-lambda',MatchC,EndC]]],
      [H,Gen]],St3}.

%% c_tq(Exp, [['<-',P,Gen]|Qs], End, St0) ->        %List generator
%%     {H,St1} = new_fun_name("lc", St0),           %Function name
%%     {Us,St2} = new_symb(St1),                    %Tail variable
%%     {Rest,St3} = c_tq(Exp, Qs, [H,Us], St2),     %Do rest of qualifiers
%%     {['letrec-function',
%%       [[H,['match-lambda',
%%        [[[P|Us]],Rest],                          %Matches pattern
%%        [[['_'|Us]],[H,Us]],                      %No match
%%        [[[]],End]]]],                            %End of list
%%       [H,Gen]],St3};

%% c_tq(Exp, [['<=',P,Gen]|Qs], End, St0) ->        %Bits generator
%%     {H,St1} = new_fun_name("bc", St0),           %Function name
%%     {B,St2} = new_symb(St1),                     %Bin variable
%%     {Rest,St3} = c_tq(Exp, Qs, [H,B], St2),      %Do rest of qualifiers
%%     Brest = [B,bitstring,'big-endian',unsigned,[unit,1]], %,[size,all]
%%     {['letrec-function',
%%       [[H,['match-lambda',
%%        [[[binary,P,Brest]],Rest],                %Matches pattern
%%        [[[binary,Brest]],End]]]],                %No match
%%       [H,Gen]],St3};

%% mapfoldl2(Fun, Acc1, Acc2, List) -> {List,Acc1,Acc2}.
%%  Like normal mapfoldl but with 2 accumulators.

mapfoldl2(Fun, A0, B0, [E0|Es0]) ->
    {E1,A1,B1} = Fun(E0, A0, B0),
    {Es1,A2,B2} = mapfoldl2(Fun, A1, B1, Es0),
    {[E1|Es1],A2,B2};
mapfoldl2(_, A, B, []) -> {[],A,B}.
