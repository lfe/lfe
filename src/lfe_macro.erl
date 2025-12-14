%% Copyright (c) 2008-2026 Robert Virding
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
-export([expand_form_init/2,expand_form_init/3,
         expand_form/4,expand_fileform/3]).
-export([expand_fileforms/3,expand_fileforms/4]).

%% For creating the macro expansion state.
-export([default_state/2,default_state/3]).

-export([format_error/1]).

%% -compile([export_all]).

-include("lfe.hrl").
-include("lfe_comp.hrl").
-include("lfe_macro.hrl").

%% Errors we get, generally in the predefined macros.
format_error({bad_form,Type}) ->
    lfe_io:format1(<<"bad ~w form">>, [Type]);
format_error({bad_ewc_form,Type}) ->
    lfe_io:format1(<<"bad eval-when-compile ~w form">>, [Type]);
format_error({defining_core_form,Name}) ->
    lfe_io:format1(<<"defining core form ~w as macro">>, [Name]);
format_error({expand_macro,Call,Error}) ->
    %% Can be very big so only print limited depth.
    lfe_io:format1(<<"error expanding ~P:\n    ~P">>, [Call,10,Error,10]);
format_error(Error) ->
    lfe_io:format1(<<"macro expansion error: ~P\n">>, [Error,10]).

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

%% expand_form_init(Deep, Keep) -> State.
%% expand_form_init(CompInfo, Deep, Keep) -> State.

expand_form_init(Deep, Keep) ->
    default_state(Deep, Keep).

expand_form_init(Ci, Deep, Keep) ->
    default_state(Ci, Deep, Keep).

default_state(Deep, Keep) ->
    #mac{deep=Deep,keep=Keep,line=1,file="-no-file-",opts=[],ipath=["."]}.

default_state(#cinfo{file=File,opts=Os,ipath=Is}, Deep, Keep) ->
    #mac{deep=Deep,keep=Keep,line=1,file=File,opts=Os,ipath=Is}.

%% expand_form(Form, Line, Env, MacState) ->
%%      {ok,Form,Env,MacState} | {error,Errors,Warnings,MacState}.
%% expand_fileform(FileForm, Env, MacState) ->
%%      {ok,FileForm,Env,MacState} | {error,Errors,Warnings,MacState}.
%%  Collect macro definitions in a (file)form, completely expand all
%%  macros and only keep all functions.

expand_form(F0, L, E0, St0) ->
    %% io:format("ef ~p\n", [{F0,L}]),
    {F1,E1,St1} = pass_form(F0, E0, St0#mac{line=L}),
    return_status(F1, E1, St1).

expand_fileform({F0,L}, E0, St0) ->
    %% io:format("eff ~p\n", [{F0,L}]),
    {F1,E1,St1} = pass_form(F0, E0, St0#mac{line=L}),
    return_status({F1,L}, E1, St1).

return_status(Ret, Env, #mac{errors=[]}=St) ->
    {ok,Ret,Env,St};
return_status(_, _, #mac{errors=Es,warnings=Ws}=St) ->
    {error,Es,Ws,St}.

%% expand_fileforms(FileForms, Env, MacState) ->
%% expand_fileforms(FileForms, Env, Deep, Keep) ->
%%     {ok,FileForms,Env,Warnings} | {error,Errors,Warnings}.
%%  Collect macro definitions in file forms, completely expand all
%%  macros and only keep all functions. This is intended to process a
%%  whole file so the end macro state is not returned.

expand_fileforms(Fs, Env, St) ->
    do_fileforms(Fs, Env, St).

expand_fileforms(Fs, Env, Deep, Keep) ->
    St = default_state(Deep, Keep),
    do_fileforms(Fs, Env, St).

do_fileforms(Fs0, Env0, St0) ->
    {Fs1,Env1,St1} = pass_fileforms(Fs0, Env0, St0),
    case St1#mac.errors of
        [] -> {ok,Fs1,Env1,St1#mac.warnings};    %No errors
        Es -> {error,Es,St1#mac.warnings}
    end.

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
    {Efs1,Env1,St1} = ewc_forms(Efs0, Env0, St0),
    {['eval-when-compile'|Efs1],Env1,St1};
pass_form(['include-file',File], Env, St0) ->
    case lfe_macro_include:file(File, Env, St0) of
        {yes,Exp,St1} -> pass_form(Exp, Env, St1);
        {error,St1} ->
            {['progn'],Env,St1}
    end;
pass_form(['include-lib',Lib], Env, St0) ->
    case lfe_macro_include:lib(Lib, Env, St0) of
        {yes,Exp,St1} -> pass_form(Exp, Env, St1);
        {error,St1} ->
            {['progn'],Env,St1}
    end;
pass_form(['define-macro'|Def]=M, Env0, St0) ->
    case pass_define_macro(Def, Env0, St0) of
        {yes,Env1,St1} ->
            Ret = ?IF(St1#mac.keep, M, [progn]),
            {Ret,Env1,St1};                     %Must return a valid form
        {no,St1} ->
            {['progn'],Env0,St1}                %Must return a valid form
    end;
%% Define 'function' at this level where it will not collide with
%% core form. And 'macro' as well where it is actually legal.
%% pass_form(['function',Name,Body], Env, St) ->
%%     {Meta,Def} = exp_defun([Body]),             %Need a list of the rest
%%     FuncDef = ['define-function',Name,Meta,Def],
%%     pass_form(FuncDef, Env, St);
%% pass_form(['macro',Name,Body], Env, St) ->
%%     {Meta,Def} = exp_defmacro([Body]),          %Need a list of the rest
%%     MacDef = ['define-macro',Name,Meta,Def],
%%     pass_form(MacDef, Env, St);
pass_form(F, Env, St0) ->
    %% First expand enough to test top form, if so process again.
    case pass_expand_expr(F, Env, St0, St0#mac.deep) of
        {yes,Exp,St1} ->                        %Top form expanded
            pass_form(Exp, Env, St1);
        {no,F1,St1} ->                          %Expanded all if flag set
            {F1,Env,St1}
    end.

%% ewc_forms(Forms, Env, State) -> {Forms,Env,State}.
%% ewc_form(Form, Env, State) -> {Form,Env,State}.
%%  Pass over the of eval-when-compile forms. Function and macro
%%  definitions are collected in the environment and other experssions
%%  are evaluated. The shell 'set' forms are also specially recognised
%%  and the variables are bound and kept in the environment as
%%  well. The functions and macros behave as in the shell.

ewc_forms(Fs, Env, St) ->
    mapfoldl2(fun (F, E, S) -> ewc_form(F, E, S) end, Env, St, Fs).

ewc_form(['progn'|Pfs0], Env0, St0) ->
    {Pfs1,Env1,St1} = ewc_forms(Pfs0, Env0, St0),
    {['progn'|Pfs1],Env1,St1};
ewc_form(['eval-when-compile'|Efs0], Env0, St0) ->
    {Efs1,Env1,St1} = ewc_forms(Efs0, Env0, St0),
    {['progn'|Efs1],Env1,St1};
ewc_form(['define-macro'|Def]=M, Env0, St0) ->
    %% Do we really want this? It behaves as a top-level macro def.
    case pass_define_macro(Def, Env0, St0) of
        {yes,Env1,St1} ->
            Ret = ?IF(St1#mac.keep, M, [progn]),
            {Ret,Env1,St1};                     %Don't macro expand now
        {no,St1} ->
            {[progn],Env0,St1}                  %Just throw it away
    end;
ewc_form(['define-function',Name,_,Def]=F, Env0, St0) ->
    case function_arity(Def) of
        {yes,Ar} ->                             %Definition not too bad
            Env1 = lfe_eval:add_dynamic_func(Name, Ar, Def, Env0),
            Ret = ?IF(St0#mac.keep, F, [progn]),
            {Ret,Env1,St0};                     %Don't macro expand now
        no ->                                   %Definition really bad
            St1 = add_error({bad_ewc_form,function}, St0),
            {[progn],Env0,St1}                  %Just throw it away
    end;
%% Define 'function' at this level where it will not collide with
%% core form. And 'macro' as well where it is actually legal.
%% ewc_form(['function',Name,Body], Env, St) ->
%%     {Meta,Def} = exp_defun([Body]),             %Need a list of the rest
%%     FuncDef = ['define-function',Name,Meta,Def],
%%     ewc_form(FuncDef, Env, St);
%% ewc_form(['macro',Name,Body], Env, St) ->
%%     {Meta,Def} = exp_defmacro([Body]),          %Need a list of the rest
%%     MacDef = ['define-macro',Name,Meta,Def],
%%     ewc_form(MacDef, Env, St);
ewc_form([set|Args], Env, St) ->
    ewc_eval_set(Args, Env, St);
ewc_form(F0, Env, St0) ->
    %% First expand enough to test top form, if so process again.
    case pass_expand_expr(F0, Env, St0, false) of
        {yes,F1,St1} ->                         %Top form expanded
            ewc_form(F1, Env, St1);
        {no,F1,St1} ->                          %Not expanded
            try
                lfe_eval:expr(F1, Env),
                {['progn'],Env,St1}             %Ignore the value
            catch
                _:_ ->
                    {['progn'],Env,add_error({bad_ewc_form,expression}, St1)}
            end
    end.

function_arity([lambda,Args|_]) ->
    ?IF(lfe_lib:is_symb_list(Args), {yes,length(Args)}, no);
function_arity(['match-lambda',[Pat|_]|_]) ->
    ?IF(lfe_lib:is_proper_list(Pat), {yes,length(Pat)}, no);
function_arity(_) -> no.

%% ewc_eval_set(Args, Env, State) -> {Set,Env,State}.
%%  Evaluate the 'set' form in an eval-when-compile

ewc_eval_set(Args, Env, St) ->
    try
        ewc_eval_set_1(Args, Env, St)
    catch
        _:_ ->                                  %Catch everything
            {[progn],Env,add_error({bad_ewc_form,'set'}, St)}
    end.

ewc_eval_set_1(Args, Env, St0) ->
    case exp_form(['let'|Args], Env, St0) of
        {['let',Pat,G,Exp],St1} ->
            ewc_eval_set_1(Pat, [G], Exp, Env, St1);
        {['let',Pat,Exp],St1} ->
            ewc_eval_set_1(Pat, [], Exp, Env, St1)
    end.                                        %Just crash here

ewc_eval_set_1(Pat, Guard, Exp, Env0, St) ->
    Val = lfe_eval:expr(Exp, Env0),
    {yes,_,Bs} = lfe_eval:match_when(Pat, Val, Guard, Env0),
    Env1 = lists:foldl(fun ({N,V}, E) -> lfe_env:add_vbinding(N, V, E) end,
                       Env0, Bs),
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
                %% {E1,St1} = exp_mod_form(E0, Env, St0),
                {E1,St1} = exp_form(E0, Env, St0),
                {no,E1,St1};
            no -> {no,E0,St0}
        end
    catch
        _:Error -> {no,E0,add_error(Error, St0)}
    end;
pass_expand_expr(E, _, St, _) -> {no,E,St}.

%% pass_define_macro([Name,Meta,Def], Env, State) ->
%%     {yes,Env,State} | {no,State}.
%%  Add the macro definition to the environment. We do a small name
%%  and format check.

pass_define_macro([Name,_,Def], Env, St) when is_atom(Name) ->
    case lfe_internal:is_core_form(Name) of
        true ->
            {no,add_warning({defining_core_form,Name}, St)};
        false ->
            case Def of
                ['lambda'|_] -> {yes,lfe_env:add_mbinding(Name, Def, Env),St};
                ['match-lambda'|_] ->
                    {yes,lfe_env:add_mbinding(Name, Def, Env),St};
                _ -> {no,add_error({bad_ewc_form,macro}, St)}
            end
    end;
pass_define_macro(_Macro, _Env, St) ->
    {no,add_error({bad_ewc_form,macro}, St)}.

%% add_error(Error, State) -> State.
%% add_error(Line, Error, State) -> State.
%% add_warning(Warning, State) -> State.
%% add_warning(Line, Warning, State) -> State.

add_error(E, St) -> add_error(St#mac.line, E, St).

add_error(L, E, St) ->
    St#mac{errors=St#mac.errors ++ [{L,?MODULE,E}]}.

add_warning(W, St) -> add_warning(St#mac.line, W, St).

add_warning(L, W, St) ->
    St#mac{warnings=St#mac.warnings ++ [{L,?MODULE,W}]}.

%% exp_form(Form, Env, State) -> {Form,State}.
%%  Completely expand a form using expansions in Env and pre-defined
%%  macros.  N.B. builtin core forms cannot be overidden and are
%%  handled here first. Some core forms also are particular about how
%%  their bodies are to be expanded and we handle these specially
%%  here. The rest we just expand the tail at the end.

%% The new defined forms which aren't macros. We put them first to
%% give them higher priority. These basically just pass their
%% arguments. This means that they also work in normal forms.
exp_form([Attr|Args0], Env, St0)
  when Attr =:= 'module'       ; Attr =:= 'export' ;
       Attr =:= 'import'       ; Attr =:= 'rename' ;
       Attr =:= 'moduledoc'    ; Attr =:= 'compile' ;
       Attr =:= 'vsn'          ; Attr =:= 'on_load' ;
       Attr =:= 'nifs'         ; Attr =:= 'doc' ;
       Attr =:= 'file'         ; Attr =:= 'alias' ;
       Attr =:= 'behaviour'    ; Attr =:= 'feature' ;
       Attr =:= 'export-macro' ->
    {Args1,St1} = exp_list(Args0, Env, St0),
    %%e io:format("emf ~p\n", [[Attr|Args1]]),
    {[Attr|Args1],St1};
%% 'attribute' and '-' are more Erlangy way of defining attributes. We
%% need to leave them as they are here.
exp_form(['attribute',Name,Value0], Env, St0) ->
    {Value1,St1} = exp_form(Value0, Env, St0),
    {['attribute',Name,Value1],St1};
exp_form(['-',Name,Value0], Env, St0) ->
    {Value1,St1} = exp_form(Value0, Env, St0),
    {['-',Name,Value1],St1};
%% 'type', 'opaque', 'spec', 'record' and 'struct' need to be macros.
%% Known Core forms which need special handling.
exp_form([quote,_]=Q, _Env, St) -> {Q,St};
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
    exp_normal_form(list, As, Env, St);
exp_form([tuple|As], Env, St) ->
    exp_normal_form(tuple, As, Env, St);
exp_form([tref|[_,_]=As], Env, St) ->
    exp_normal_form(tref, As, Env, St);
exp_form([tset|[_,_,_]=As], Env, St) ->
    exp_normal_form(tset, As, Env, St);
exp_form([binary|As], Env, St) ->
    exp_normal_form(binary, As, Env, St);
exp_form([map|As], Env, St) ->
    exp_normal_form(map, As, Env, St);
exp_form([msiz|As], Env, St) ->
    exp_normal_form(msiz, As, Env, St);
exp_form([mref|As], Env, St) ->
    exp_normal_form(mref, As, Env, St);
exp_form([mset|As], Env, St) ->
    exp_normal_form(mset, As, Env, St);
exp_form([mupd|As], Env, St) ->
    exp_normal_form(mupd, As, Env, St);
exp_form([mrem|As], Env, St) ->
    exp_normal_form(mrem, As, Env, St);
exp_form(['map-size'|As], Env, St) ->
    exp_normal_form('map-size', As, Env, St);
exp_form(['map-get'|As], Env, St) ->
    exp_normal_form('map-get', As, Env, St);
exp_form(['map-set'|As], Env, St) ->
    exp_normal_form('map-set', As, Env, St);
exp_form(['map-update'|As], Env, St) ->
    exp_normal_form('map-update', As, Env, St);
exp_form(['map-remove'|As], Env, St) ->
    exp_normal_form('map-remove', As, Env, St);
%% Record special forms. Note that these are used for both the
%% compiler as well as the evaluator so we can't do too much here.
exp_form(['define-record',Name,Fds], Env, St0) ->
    {Efds,St1} = exp_record_fields(Name, Fds, Env, St0),
    {['define-record',Name,Efds],St1};
exp_form(['record',Name|Args], Env, St0) ->
    {Eas,St1} = exp_tail(Args, Env, St0),
    {['record',Name|Eas],St1};
%% make-record has been deprecated but we sill accept it for now.
exp_form(['make-record',Name|Args], Env, St0) ->
    {Eas,St1} = exp_tail(Args, Env, St0),
    {['make-record',Name|Eas],St1};
exp_form(['is-record',E,Name], Env, St0) ->
    {Ee,St1} = exp_form(E, Env, St0),
    {['is-record',Ee,Name],St1};
exp_form(['record-index',Name,F], _, St) ->
    {['record-index',Name,F],St};
exp_form(['record-field',E,Name,F], Env, St0) ->
    {Ee,St1} = exp_form(E, Env, St0),
    {['record-field',Ee,Name,F],St1};
exp_form(['record-update',E,Name|Args], Env, St0) ->
    {Ee,St1} = exp_form(E, Env, St0),
    {Eas,St2} = exp_tail(Args, Env, St1),
    {['record-update',Ee,Name|Eas],St2};
%% Struct special forms. Note that these are used for both the
%% compiler as well as the evaluator so we can't do too much here.
exp_form(['define-struct',Fds], Env, St0) ->
    {Efds,St1} = exp_struct_fields(Fds, Env, St0),
    {['define-struct',Efds],St1};
exp_form(['struct',Name|Args], Env, St0) ->
    {Eas,St1} = exp_tail(Args, Env, St0),
    {['struct',Name|Eas],St1};
exp_form(['is-struct',E], Env, St0) ->
    {Ee,St1} = exp_form(E, Env, St0),
    {['is-struct',Ee],St1};
exp_form(['is-struct',E,Name], Env, St0) ->
    {Ee,St1} = exp_form(E, Env, St0),
    {['is-struct',Ee,Name],St1};
exp_form(['struct-field',E,Name,F], Env, St0) ->
    {Ee,St1} = exp_form(E, Env, St0),
    {['struct-field',Ee,Name,F],St1};
exp_form(['struct-update',E,Name|Args], Env, St0) ->
    {Ee,St1} = exp_form(E, Env, St0),
    {Eas,St2} = exp_tail(Args, Env, St1),
    {['struct-update',Ee,Name|Eas],St2};
%% Function new form for defining functions.
%% Note that this can be used to make a function reference.
exp_form([function,Name|Def0], Env, St0) ->
    {Def1,St1} = exp_tail(Def0, Env, St0),
    {[function,Name|Def1],St1};
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
    exp_normal_form(progn, As, Env, St);
exp_form([prog1|As], Env, St) ->
    exp_normal_form(prog1, As, Env, St);
exp_form([prog2|As], Env, St) ->
    exp_normal_form(prog2, As, Env, St);
exp_form(['if'|As], Env, St) ->
    exp_normal_form('if', As, Env, St);
exp_form(['case',E0|Cls0], Env, St0) ->
    {E1,St1} = exp_form(E0, Env, St0),
    {Cls1,St2} = exp_clauses(Cls0, Env, St1),
    {['case',E1|Cls1],St2};
exp_form(['cond'|Body], Env, St) ->
    exp_cond(Body, Env, St);
exp_form(['maybe'|Body], Env, St) ->
    exp_maybe(Body, Env, St);
exp_form(['receive'|Cls0], Env, St0) ->
    {Cls1,St1} = exp_clauses(Cls0, Env, St0),
    {['receive'|Cls1],St1};
exp_form(['catch'|B0], Env, St0) ->
    {B1,St1} = exp_tail(B0, Env, St0),
    {['catch'|B1],St1};
exp_form(['try',E|B], Env, St) ->
    exp_try(E, B, Env, St);
exp_form([funcall|As], Env, St) ->
    exp_normal_form(funcall, As, Env, St);
exp_form([call|As], Env, St) ->
    exp_normal_form(call, As, Env, St);
%% List/binary comprehensions.
exp_form([lc,Qs,Exp], Env, St0) ->
    exp_list_comprehension('lc', Qs, Exp, Env, St0);
exp_form(['list-comp',Qs,Exp], Env, St) ->
    exp_list_comprehension('list-comp', Qs, Exp, Env, St);
exp_form([bc,Qs,Exp], Env, St) ->
    exp_binary_comprehension('bc', Qs, Exp, Env, St);
exp_form(['binary-comp',Qs,Exp], Env, St) ->
    exp_binary_comprehension('binary-comp', Qs, Exp, Env, St);
%% And don't forget when.
exp_form(['when'|G], Env, St) ->
    exp_normal_form('when', G, Env, St);
%% Core definition special forms.
exp_form(['eval-when-compile'|B], Env, St) ->
    exp_normal_form('eval-when-compile', B, Env, St);
exp_form(['define-function',Name,Meta,Def], Env, St) ->
    exp_define_function(Name, Meta, Def, Env, St);
exp_form(['define-macro',Name,_Meta,Def], Env, St) ->
    exp_head_tail('define-macro', Name, Def, Env, St);
%% Only worry about the module forms of the right size to expand and
%% don't touch the rest.
exp_form(['define-module',Mod,Metas,Attrs], Env, St) ->
    exp_define_module(Mod, Metas, Attrs, Env, St);
exp_form(['extend-module',Metas,Attrs], Env, St) ->
    exp_extend_module(Metas, Attrs, Env, St);
exp_form(['define-module',_Mod|_]=Form, _, St) -> {Form,St};
exp_form(['extend-module'|_]=Form, _, St) -> {Form,St};
%% These aren't expanded at all and just passed on as is.
exp_form(['define-type',_Type|_]=Form, _, St) -> {Form,St};
exp_form(['define-opaque-type',_Type|_]=Form, _, St) -> {Form,St};
exp_form(['define-function-spec',_Func|_]=Form, _, St) -> {Form,St};
%% Now the case where we can have macros.
exp_form([Fun|Args]=Call, Env, St0) when is_atom(Fun) ->
    %% Expand top macro as much as possible.
    case exp_macro(Call, Env, St0) of
        {yes,Exp,St1} -> exp_form(Exp, Env, St1);
        no -> exp_normal_form(Fun, Args, Env, St0)
    end;
exp_form([_|_]=Form, Env, St) -> exp_tail(Form, Env, St);
exp_form(Tup, _, St) when is_tuple(Tup) ->
    %% Should we expand this? We assume implicit quote here.
    {Tup,St};
%% Everything else is atomic.
exp_form(F, _, St) -> {F,St}.                   %Atomic

exp_normal_form(Name, As0, Env, St0) ->
    {As1,St1} = exp_tail(As0, Env, St0),
    {[Name|As1],St1}.

exp_head_tail(Name, Head, B0, Env, St0) ->
    {B1,St1} = exp_tail(B0, Env, St0),
    {[Name,Head|B1],St1}.

%% exp_list(Exprs, Env, State) -> {Exps,State}.
%%  Expand a proper list of exprs.

exp_list(Es, Env, St) ->
    lists:mapfoldl(fun (E, S) -> exp_form(E, Env, S) end, St, Es).

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

%% exp_record_fields(Name, Fields, Env, State) -> {ExpArgs,State}.
%%  Expand the field definitions for the record.

exp_record_fields(_, Fields, Env, St) ->
    lists:mapfoldl(fun (F, S) -> exp_record_field(F, Env, S) end, St, Fields).

exp_record_field([_|_]=Fdef, Env, St) ->
    exp_list(Fdef, Env, St);
exp_record_field(Fdef, Env, St) ->
    exp_form(Fdef, Env, St).

%% exp_struct_fields(Fields, Env, State) -> {ExpArgs,State}.
%%  Expand the field definitions for the struct.

exp_struct_fields(Fields, Env, St) ->
    lists:mapfoldl(fun (F, S) -> exp_struct_field(F, Env, S) end, St, Fields).

exp_struct_field([_|_]=Fdef, Env, St) ->
    exp_list(Fdef, Env, St);
exp_struct_field(Fdef, Env, St) ->
    exp_form(Fdef, Env, St).

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
    {Env1,St1} = lists:foldl(Efun, {Env0,St0}, Fbs0),
    {Fbs1,St2} = exp_clauses(Fbs0, Env1, St1),
    {B1,St3} = exp_tail(B0, Env1, St2),
    {Fbs1,B1,St3}.

%% exp_let_macro(MacroBindings, Body, Env, State) -> {Expansion,State}.
%%  Expand a let_syntax. We add the actual macro binding to the env as
%%  we may need them while expanding the body.

exp_let_macro(Mbs, B0, Env0, St0) ->
    %% Add the macro defs from expansion and return body in a progn.
    LetFun = fun ([Name,['lambda'|_]=Def], Env) when is_atom(Name) ->
                     lfe_env:add_mbinding(Name, Def, Env);
                 ([Name,['match-lambda'|_]=Def], Env) when is_atom(Name) ->
                     lfe_env:add_mbinding(Name, Def, Env);
                 (_, Env) -> Env            %Ignore mistakes
             end,
    Env1 = lists:foldl(LetFun, Env0, Mbs),
    {B1,St1} = exp_tail(B0, Env1, St0),         %Expand the body
    {['progn'|B1],St1}.

%% exp_cond(Body, Env, State) -> {Expansion,State}.

exp_cond(Cls, Env, St0) ->
    {Ecls,St1} = exp_cond_clauses(Cls, Env, St0),
    {['cond'|Ecls],St1}.

exp_cond_clauses(Cls, Env, St) ->
    exp_tail(fun exp_cond_clause/3, Cls, Env, St).

exp_cond_clause([['?='|TestPat]|Body], Env, St0) ->
    {Et,St1} = exp_clause(TestPat, Env, St0),
    {Eb,St2} = exp_tail(Body, Env, St1),
    {[['?='|Et]|Eb],St2};
exp_cond_clause(['else'|Body], Env, St0) ->
    {Eb,St1} = exp_tail(Body, Env, St0),
    {['else'|Eb],St1};
exp_cond_clause(TestBody, Env, St) ->
    exp_tail(TestBody, Env, St).

%% exp_maybe(Body, Env, State) -> {Expansion,State}.

exp_maybe(Body, Env, St0) ->
    {Eb,St1} = exp_maybe_body(Body, Env, St0),
    {['maybe'|Eb],St1}.

exp_maybe_body(Es, Env, St) ->
    exp_tail(fun exp_maybe_expr/3, Es, Env, St).

exp_maybe_expr(['else'|Cls], Env, St0) ->
    {Ecls,St1} = exp_clauses(Cls, Env, St0),
    {['else'|Ecls],St1};
exp_maybe_expr(['?='|Cond], Env, St0) ->
    {Econd,St1} = exp_clause(Cond, Env, St0),
    {['?='|Econd],St1};
exp_maybe_expr(['let',Vbs|Body], Env, St0) ->
    {Evbs,St1} = exp_clauses(Vbs, Env, St0),
    {Eb,St2} = exp_maybe_body(Body, Env, St1),
    {['let',Evbs|Eb],St2};
exp_maybe_expr(E, Env, St) ->
    exp_form(E, Env, St).

%% exp_try(Expression, Body, Env, State) -> {Expansion,State}.
%%  Expand a try.

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

%% exp_list_comprehension(Comp, Qualifiers, Expr, Env, State) ->
%%     {Qualifiers,Exp,State}.
%% exp_binary_comprehension(Comp, Qualifiers, BitStringExpr, Env, State) ->
%%     {Qualifiers,BitStringExpr,State}.
%%  Don't do much yet.

exp_list_comprehension(Comp, Qs0, Expr0, Env, St0) ->
    {Expr1,St1} = exp_form(Expr0, Env, St0),
    %% io:format("lml ~p\n    ~p\n", [Expr0,Expr1]),
    {Qs1,St2} = exp_comprehension_quals(Qs0, Env, St1),
    {[Comp,Qs1,Expr1],St2}.

exp_binary_comprehension(Comp, Qs0, BitExpr0, Env, St0) ->
    {BitExpr1,St1} = exp_form(BitExpr0, Env, St0),
    %% io:format("lmb ~p\n   ~p\n", [BitExpr0,BitExpr1]),
    {Qs1,St2} = exp_comprehension_quals(Qs0, Env, St1),
    {[Comp,Qs1,BitExpr1],St2}.

%% exp_comprehension_quals(Qualifiers, Env, State) -> {Qualifiers,State}.
%%  We accept improper qualifier list here as the tail might expand
%%  into a proper list. We will let the linter catch any errors.

exp_comprehension_quals([Qual0|Qs0], Env, St0) ->
    {Qual1,St1} = exp_comprehension_qual(Qual0, Env, St0),
    {Qs1,St2} = exp_comprehension_quals(Qs0, Env, St1),
    {[Qual1|Qs1],St2};
exp_comprehension_quals(Other0, Env, St0) ->
    %% This also catches [].
    {Other1,St1} = exp_form(Other0, Env, St0),
    {Other1,St1}.

exp_comprehension_qual(['<-'|Cls0], Env, St0) ->
    {Cls1,St1} = exp_clause(Cls0, Env, St0),
    {['<-'|Cls1],St1};
exp_comprehension_qual(['<='|Cls0], Env, St0) ->
    {Cls1,St1} = exp_clause(Cls0, Env, St0),
    {['<='|Cls1],St1};
exp_comprehension_qual(Test0, Env, St0) ->
    {Test1,St1} = exp_form(Test0, Env, St0),
    {Test1,St1}.

%% exp_define_function(Name, Metq, Def, Env, State) -> {Expansion,State}.
%%  Expand a function definition adding the function local macros:
%%  (defmacro FUNCTION_NAME () `'name)
%%  (defmacro FUNCTION_ARITY () arity)

exp_define_function(Name, Meta0, Def0, Env0, St0) ->
    %% Just get an arity, a bad def will crash later anyway.
    Arity = case function_arity(Def0) of
                {yes,A} -> A;
                no -> 0
            end,
    {Meta1,St1} = exp_form(Meta0, Env0, St0),
    Fun = fun ([Mname|Rest], E) ->
                  {_,Mdef} = exp_defmacro(Rest),
                  lfe_env:add_mbinding(Mname, Mdef, E)
          end,
    Env1 = lists:foldl(Fun, Env0, [['FUNCTION_NAME',[],?BQ(?Q(Name))],
                                   ['FUNCTION_ARITY',[],Arity]]),
    {Def1,St2} = exp_form(Def0, Env1, St1),
    {['define-function',Name,Meta1,Def1],St2}.

%% exp_macro(Call, Env, State) -> {yes,Exp,State} | no.
%%  Expand the macro in top call, but not if it is a core form.

exp_macro([Name|_]=Call, Env, St) ->
    %% io:format("em ~p\n", [Call]),
    case is_atom(Name) andalso lfe_internal:is_core_form(Name) of
        true -> no;                             %Never expand core forms
        false ->
            case lfe_env:get_mbinding(Name, Env) of
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
    %% lfe_io:format("udef: ~p\n", [[Mac|Args]]),
    %% lfe_io:format("macro: ~p\n", [Def0]),
    try
        {Def1,St1} = exp_form(Def0, Env, St0),  %Expand definition
        Exp = lfe_eval:apply(Def1, [Args,Env], Env),
        {yes,Exp,St1}
    catch
        %% error:no_Error -> boom
        ?CATCH(error, Error, Stack)
            %% io:format("Userdef stack ~p\n", [Stack]),
            erlang:raise(error, {expand_macro,[Mac|Args],Error}, Stack)
        %% ?CATCH(error, Error, Stack0)
        %%     Stack1 = trim_stacktrace(Stack0),
        %%     erlang:error({expand_macro,[Mac|Args],{Error,Stack1}})
    end.

%% exp_predef_macro(Call, Env, State) -> {yes,Exp,State} | no.
%%  Evaluate predefined macro definition catching errors.

exp_predef_macro(Call, Env, St) ->
    %% lfe_io:format("pdef: ~p\n", [Call]),
    try
        exp_predef(Call, Env, St)
    catch
        ?CATCH(error, Error, Stack)
            %% io:format("Predef stack ~p\n", [Stack]),
            erlang:raise(error, {expand_macro,Call,Error}, Stack)
        %% ?CATCH(error, Error, Stack0)
        %%     Stack1 = trim_stacktrace(Stack0),
        %%     erlang:error({expand_macro,Call,{Error,Stack1}})
    end.

%% exp_define_module(Name, Metas, Attrs, Env, State) -> {Expansion,State}.
%% exp_extend_module(Rest, Env, State) -> {Expansion,State}.
%%  As record definitions are allowed in module definitions in the
%%  meta data and the default values contain code these must be
%%  macroexpanded. We try and be lenient and pass syntactic errors on
%%  to the linter.

exp_define_module(Name, Metas, Attrs, Env, St0) ->
    Fun = fun (Meta, S) -> exp_module_meta(Meta, Env, S) end,
    {Emetas,St1} = lists:mapfoldl(Fun, St0, Metas),
    {['define-module',Name,[],Attrs ++ Emetas],St1}.

exp_extend_module(Metas, Attrs, Env, St0) ->
    Fun = fun (Meta, S) -> exp_module_meta(Meta, Env, S) end,
    {Emetas,St1} = lists:mapfoldl(Fun, St0, Metas),
    {['extend-module',[],Attrs ++ Emetas],St1}.

exp_module_meta([record|Recs], Env, St0) ->
    {Erecs,St1} = lists:mapfoldl(fun (R, S) -> exp_module_rec(R, Env, S) end,
                                 St0, Recs),
    {[record|Erecs],St1};
exp_module_meta([struct|Fds], Env, St0) ->
    {Efds,St1} = exp_struct_fields(Fds, Env, St0),
    {[struct|Efds],St1};
exp_module_meta(Meta, _Env, St) ->
    {Meta,St}.

exp_module_rec([Name,Fds], Env, St0) ->
    {Efds,St1} = exp_record_fields(Name, Fds, Env, St0),
    {[Name,Efds],St1};
exp_module_rec(Other, _Env, St) -> {Other,St}.

%% trim_stacktrace([{lfe_macro,_,_,_}=S|_]) -> [S];    %R15 and later
%% trim_stacktrace([{lfe_macro,_,_}|_]=S) -> [S];      %Pre R15
%% trim_stacktrace([S|Stk]) -> [S|trim_stacktrace(Stk)];
%% trim_stacktrace([]) -> [].

%% exp_predef(Form, Env, State) -> {yes,Form,State} | no.
%%  Expand the built-in predefined macros completely at top-level
%%  without returning a new predefined top-level macro. This make the
%%  macros "safe" even if they have been redefined as it is this
%%  definition which is used.

%% Now for the new defined forms which basically go to themselves. We
%% can't do it explicitly here as we would go into a loop so we go to
%% define-***.
exp_predef(['module',Name], _Env, St) ->
    %% Define the MODULE macro.
    MODULE = [defmacro,'MODULE',[],?BQ(?Q(Name))],
    {yes,[progn,['define-module',Name,[],[]],MODULE],St#mac{module=Name}};
exp_predef(['type',Type0,Def0], _Env, St) ->
    {Type1,Def1} = exp_deftype(Type0, [Def0]),  %Type one element.
    {yes,['define-type',Type1,Def1],St};
exp_predef(['opaque',Type0,Def0], _Env, St) ->
    {Type1,Def1} = exp_deftype(Type0, [Def0]),  %Type one element.
    {yes,['define-opaque-type',Type1,Def1],St};
exp_predef(['spec',Func0,Spec0], _Env, St) ->
    {Func1,Spec1} = exp_defspec(Func0, Spec0),
    {yes,['define-function-spec',Func1,Spec1],St};
exp_predef(['record',Name,Fds], Env, St) ->
    %%e lfe_macro_record:define([Name|Fds], Env, St);
    {yes,[progn,Def|_Macs],St1} = lfe_macro_record:define([Name|Fds], Env, St),
    {yes,Def,St1};
exp_predef(['struct',Fds], Env, St) ->
    lfe_macro_struct:define(Fds, Env, St);
%% Unfortunately we can't expand a (function ...) as 'function' is a
%% core form.
%% exp_predef(['function',Name,Def], _Env, St) ->
%%     {yes,['define-function',Name,[],Def],St};
exp_predef(['macro',Name|Rest], _Env, St) ->
    {Meta,Def} = exp_defmacro(Rest),
    {yes,['define-macro',Name,Meta,Def],St};
%% export-macro needs to be in extend-module for now.
exp_predef(['export-macro'|_]=ExpMac, _, St) ->
    {yes,['extend-module',[],[ExpMac]],St};
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

%% Comparison operators.
exp_predef(['!='|Es], _Env, St) -> {yes,['/='|Es],St};
exp_predef(['==='|Es], _Env, St) -> {yes,['=:='|Es], St};
exp_predef(['!=='|Es], _Env, St) -> {yes,['=/='|Es], St};
%% The backquote macro.
exp_predef([backquote,Bq], Env, St) ->          %We do this here.
    lfe_macro_backquote:expand(Bq, Env, St);
exp_predef(['?'|As], _, St) ->
    Omega = [omega,omega],                      %Match anything and return it
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
exp_predef(['do'|Dbody], _, St0) ->
    {Exp,St1} = exp_do(Dbody, St0),
    {yes,Exp,St1};
exp_predef(['fun',F,Ar], _, St0) ->
    {Vs,St1} = new_symbs(Ar, St0),
    {yes,['lambda',Vs,[F|Vs]],St1};
exp_predef(['fun',M,F,Ar], _, St0) ->
    {Vs,St1} = new_symbs(Ar, St0),
    {yes,['lambda',Vs,['call',?Q(M),?Q(F)|Vs]],St1};
exp_predef(['defrecord'|Def], Env, St) ->
    lfe_macro_record:define(Def, Env, St);
exp_predef(['defstruct'|Def], Env, St) ->
    lfe_macro_struct:define(Def, Env, St);
%% Common Lisp inspired macros.
%% Note these module forms MUST expand to define-xxxx to be handled
%% without looping.
exp_predef([defmodule,Name|Rest], _, St) ->
    %% Define the MODULE macro.
    MODULE = [defmacro,'MODULE',[],?BQ(?Q(Name))],
    {Meta,Atts} = exp_defmodule(Rest),
    {yes,[progn,['define-module',Name,Meta,Atts],MODULE],St#mac{module=Name}};
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
    %% io:format("defun ~p ~p\n", [Name,Rest]),
    {Meta,Def} = exp_defun(Rest),
    %% io:format("defun -> ~p ~p\n", [Meta,Def]),
    {yes,['define-function',Name,Meta,Def],St};
exp_predef([defmacro,Name|Rest], _, St) ->
    %% Educated guess whether traditional (defmacro name (a1 a2 ...) ...)
    %% or matching (defmacro name (patlist1 ...) (patlist2 ...))
    {Meta,Def} = exp_defmacro(Rest),
    {yes,['define-macro',Name,Meta,Def],St};
exp_predef([flet,Defs|Body], _, St) ->
    Fun = fun ([Name|Rest]) ->
                  {_,Def} = exp_defun(Rest),    %Ignore meta data
                  [Name,Def]
          end,
    Fdefs = lists:map(Fun, Defs),
    {yes,['let-function',Fdefs|Body], St};
exp_predef([fletrec,Defs|Body], _, St) ->
    Fun = fun ([Name|Rest]) ->
                  {_,Def} = exp_defun(Rest),    %Ignore meta data
                  [Name,Def]
          end,
    Fdefs = lists:map(Fun, Defs),
    {yes,['letrec-function',Fdefs|Body], St};
exp_predef([macrolet,Defs|Body], _, St) ->
    Fun = fun ([Name|Rest]) ->
                  {_,Def} = exp_defmacro(Rest), %Ignore meta data
                  [Name,Def]
          end,
    Mdefs = lists:map(Fun, Defs),
    {yes,['let-macro',Mdefs|Body],St};
%% Handle match specifications both ets and tracing (dbg).
%% This has to go here so as to be able to macro expand body.
exp_predef(['match-spec'|Cls], Env, St) ->      %The old interface.
    exp_predef(['ets-ms'|Cls], Env, St);
exp_predef(['table-ms'|Body], Env, St0) ->
    {Exp,St1} = exp_ml_clauses(Body, Env, St0),
    MS = lfe_ms:expand(table, Exp),
    {yes,MS,St1};
exp_predef(['trace-ms'|Body], Env, St0) ->
    {Exp,St1} = exp_ml_clauses(Body, Env, St0),
    MS = lfe_ms:expand(trace, Exp),
    {yes,MS,St1};
exp_predef(['ets-ms'|Body], Env, St) ->
    exp_predef(['table-ms'|Body], Env, St);
exp_predef(['dbg-ms'|Body], Env, St) ->         %Just a synonym
    exp_predef(['trace-ms'|Body], Env, St);
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
exp_predef([Fun|As], _, St) when is_atom(Fun), Fun =/= '=:=' ->
    %% =:= is an operator.
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
    Vlc = lfe_translate:to_expr([lc,Eqs|Ees], 42),
    %% lfe_io:format("~w\n", [Vlc]),
    Vos = lists:map(fun (O) -> lfe_translate:to_expr(O, 42) end, Opts),
    %% io:put_chars(["E0 = ",erl_pp:expr(Vlc, 5, []),"\n"]),
    {ok,Vexp} = lfe_qlc:expand(Vlc, Vos),
    %% io:put_chars([erl_pp:expr(Vexp),"\n"]),
    Exp = lfe_translate:from_expr(Vexp),
    %% lfe_io:format("Q1 = ~p\n", [Exp]),
    {yes,Exp,St2}.

exp_qlc_quals(Qs, Env, St) ->
    lists:mapfoldl(fun (Q, S) -> exp_qlc_qual(Q, Env, S) end, St, Qs).

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

%% exp_do(DoBody) -> DoLoop.
%%  Expand a do body into a loop. Add a variable 'do-state' which is
%%  the value of the do body which can be used when setting new values
%%  to do vars.

exp_do([Pars,[Test,Ret]|Body], St0) ->
    Foldr = fun ([V,I,C], {Vs,Is,Cs}) -> {[V|Vs],[I|Is],[C|Cs]} end,
    {Vs,Is,Cs} = lists:foldr(Foldr, {[],[],[]}, Pars),
    {Fun,St1} = new_fun_name("do", St0),
    Exp = ['letrec-function',
           [[Fun,[lambda,Vs,
                  ['if',Test,Ret,
                   ['let',[['do-state',
                            ['progn'] ++ Body]],
                    [Fun|Cs]]]]]],
           [Fun|Is]],
    {Exp,St1}.

%% exp_defmodule(Rest) -> {Meta,Attributes}.
%%  Extract the comment string if it is first and make it the 'doc' or
%%  'moduledoc' depending on version, then move the rest to
%%  attributes. The order is preserved. We still return both metas and
%%  attributes as this is the defined form.

exp_defmodule([Doc|More]=Rest0) ->
    Rest1 = ?IF(lfe_lib:is_doc_string(Doc),
                [defmodule_doc(Doc)|More],
                Rest0),
    Mfun = fun ([spec|Specs0], {Me,As}) ->
                   Sfun = fun ([Func|Spec]) ->
                                  {Sfunc,Def} = exp_defspec(Func, Spec),
                                  [Sfunc,Def]
                          end,
                   Specs1 = lists:map(Sfun, Specs0),
                   {Me,As ++ [[spec|Specs1]]};
               (R, {Me,As}) ->
                   {Me,As ++ [R]}
           end,
    lists:foldl(Mfun, {[],[]}, Rest1);
exp_defmodule([]) -> {[],[]}.

-ifdef(OTP27_DOCS).
defmodule_doc(Doc) -> [moduledoc,Doc].
-else.
defmodule_doc(Doc) -> [doc,Doc].
-endif.

%% is_meta_tag(doc) -> true;
%% is_meta_tag(spec) -> true;
%% is_meta_tag(record) -> true;
%% is_meta_tag(Tag) -> lfe_types:is_type_decl(Tag).

%% exp_deftype(Type, Def) -> {Type,Def}.
%%  Paramterless types to be written as just type name and default
%%  type is any.

exp_deftype(T, D) ->
    Type = if is_list(T) -> T; true -> [T] end,
    Def = if D =:= [] -> [any]; true -> hd(D) end,
    {Type,Def}.

%% exp_defspec(Func, Def) -> {Func,Def}.
%%  Do very little here, leave it to lint.

exp_defspec([_,_]=Func, Def) -> {Func,Def};
exp_defspec(Name, Def) ->
    {[Name,defspec_arity(Def)],Def}.

%% defspec_arity(Spec) -> Arity.
%%  Just return the length of the first arg list and let lint check
%%  properly later.

defspec_arity([#{'arg-types' := Args}|_]) ->
    case lfe_lib:is_proper_list(Args) of
        true -> length(Args);
        false -> 0
    end;
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
    {Meta,Def} = exp_function_meta(Body, []),
    {Meta,['lambda',Args|Def]}.

exp_match_defun(Rest) ->
    {Meta,Cls} = exp_function_meta(Rest, []),
    {Meta,['match-lambda'|Cls]}.

exp_function_meta([[spec|Spec]|Rest], Meta) ->
    exp_function_meta(Rest, Meta ++ [[spec|Spec]]);
exp_function_meta([String|Rest], Meta) ->
    %% The untagged doc string but not at the end.
    ?IF(lfe_lib:is_doc_string(String) and (Rest =/= []),
        exp_function_meta(Rest, Meta ++ [[doc,String]]),
        {Meta,[String|Rest]});
exp_function_meta([], Meta) -> {Meta,[]}.

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
    {Meta,Def} = exp_macro_meta(Body),
    {Meta,[[[Args,'$ENV']|Def]]}.

exp_match_defmacro(Rest) ->
    {Meta,Cls} = exp_macro_meta(Rest),
    {Meta,lists:map(fun ([Head|Body]) -> [[Head,'$ENV']|Body] end, Cls)}.

exp_macro_meta([String|Rest]) ->
    %% The untagged doc string but not at the end.
    ?IF(lfe_lib:is_doc_string(String) and (Rest =/= []),
        {[[doc,String]],Rest},
        {[],[String|Rest]}).

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

%% mapfoldl2(Fun, Acc1, Acc2, List) -> {List,Acc1,Acc2}.
%%  Like normal mapfoldl but with 2 accumulators and it 

mapfoldl2(Fun, A0, B0, [E0|Es0]) ->
    {E1,A1,B1} = Fun(E0, A0, B0),
    {Es1,A2,B2} = mapfoldl2(Fun, A1, B1, Es0),
    {[E1|Es1],A2,B2};
mapfoldl2(_, A, B, []) -> {[],A,B}.

%% The new module input forms
%% (module name)
%% (export funcs|'all')
%% (import module imports) ?
%% (rename module renames) ?
%% (alias module alias)
%% (moduledoc doc)
%% (compile options)
%% (vsn vsn)
%% (on_load func)
%% (nifs funcs)
%%
%% (export-type types)
%% (export-macro macros|'all')
%% (macro name definition)
%% (function name definition)
%% (eval-when-compile forms)
%%
%% (doc doc)
%% (type name def)
%% (opaque name def)
%% (spec func specs)
%% (record name fields)
%% (struct fields)
%%
%% (attribute attr-name attr-value)
%% (- attr-name attr-value)
