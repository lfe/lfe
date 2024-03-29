%% Copyright (c) 2016-2022 Robert Virding
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

%% File    : lfe_macro_export.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang macro export function builder.

%% Build the LFE-EXPAND-EXPORTED-MACRO (L-E-E-M) function which
%% exports macros so they can be found by the macro expander without
%% needing to inlcude them. If the module foo exports macro bar then
%% it can be called by doing (foo:bar ...).
%%
%% This version does not actually expand any macros but just collects
%% them. Macros that are to be exported are entered into the L-E-E-M
%% function so they can be exported and called from the outside. We
%% also enter all the functions and variables defined inside ewc so
%% they can be reached from these macros when the L-E-E-M function is
%% compiled. We do NOT need to save the ewc macros as they will
%% accessible when the module is compiled.
%%
%% The macros will be expanded in the context of the module when it is
%% later compiled and not in the context of the calling module. This
%% is more consistent and is easier.
%%
%% The matching is done in two steps: first we test whether the call
%% name is one of our known macros; if so we test whether the
%% arguments match against the argument patterns in the macro
%% definition. Doing it like this gives us the same failure handling
%% as when expanding local calls to macros.

%% (defun LFE-EXPAND-EXPORTED-MACRO (name args $ENV)
%%   (let ((var-1 val-1)                   ;Eval-when-compile variables
%%         ...)
%%     (fletrec ((fun-1 ...)               ;Eval-when-compile functions
%%               ...)
%%       (case name                        ;Macro name without module
%%         ('mac-1 ...)                    ;Already exported local macros
%%          (case args                     ;Match against args
%%           (arg-pat ...)
%%           ...))
%%         ('mac-2 ...)
%%         ...
%%         (_ 'no)))))

-module(lfe_macro_export).

%%-compile(export_all).

-include("lfe.hrl").

-export([module/2]).

-import(lists, [reverse/1,reverse/2,member/2,filter/2]).

-define(NOMETA, []).                            %Empty documentation

%% We need these variables to have a funny name.
-define(NAMEVAR, '|- MACRO NAME -|').
-define(ARGSVAR, '|- CALL ARGS -|').

%% Define the macro data.
-record(umac, {mline=[],                        %expand-macro on line
               expm=[],                         %Macros to export
               env=[],                          %LFE env to store macros
               uleem=false                      %Do we have user defined LEEM?
              }).

%% module(ModuleForms, CompState) -> {ModuleForms,CompState}.
%% module(ModuleDef, ModuleForms, UmacState, CompState) ->
%%     {ModuleForms,CompState}.

module([Mdef|Fs], Cst) ->
    Mst = collect_macros(Fs, #umac{env=lfe_env:new()}),
    module(Mdef, Fs, Mst, Cst).

module({['define-module',Name,Meta,Atts],L}, Fs, Mst0, Cst) ->
    Mst1 = collect_attrs(Atts, Mst0#umac{mline=L}),
    Emac = build_exported_macro(Mst1),
    %% We need to export the expansion function but leave the rest.
    Exp = [export,['LFE-EXPAND-EXPORTED-MACRO',3]],
    Md1 = {['define-module',Name,Meta,[Exp|Atts]],L},
    %% io:format("m: ~p\n", [{Md1,Fs,Emac}]),
    %% Put export-macro last so it can find all macros.
    {[Md1|Fs ++ Emac],Cst}.

collect_macros(Fs, Mst) ->
    lists:foldl(fun collect_macro/2, Mst, Fs).

collect_macro({['define-macro',Name,_,Def],_}, #umac{env=Env0}=Mst) ->
    Env1 = lfe_env:add_mbinding(Name, Def, Env0),
    Mst#umac{env=Env1};
collect_macro({['eval-when-compile'|Fs],_}, Mst) ->
    lists:foldl(fun collect_ewc_macro/2, Mst, Fs);
collect_macro({['extend-module',_,Atts],_}, Mst) ->
    collect_attrs(Atts, Mst);
collect_macro({['define-function',Name,_,Def],_}, Mst) ->
    %% Check for user defined LFE-EXPAND-EXPORTED-MACRO.
    case {Name,function_arity(Def)} of
        {'LFE-EXPAND-EXPORTED-MACRO',3} ->
            Mst#umac{uleem=true};
        _ -> Mst                                %Ignore other functions
    end;
collect_macro(_, Mst) -> Mst.                   %Ignore everything else

collect_ewc_macro([set,Name,Val], #umac{env=Env0}=Mst) ->
    Env1 = lfe_env:add_vbinding(Name, Val, Env0),
    Mst#umac{env=Env1};
collect_ewc_macro(['define-function',Name,_,Def], #umac{env=Env0}=Mst) ->
    Ar = function_arity(Def),
    Env1 = lfe_env:add_fbinding(Name, Ar, Def, Env0),
    Mst#umac{env=Env1};
collect_ewc_macro(['define-macro'|_], Mst) ->
    %% We ignore ewc macros here as they are not exportable.
    Mst;
collect_ewc_macro([progn|Fs], Mst) ->
    lists:foldl(fun collect_ewc_macro/2, Mst, Fs).

%% function_arity(FuncDef) -> Arity.
%%  Don't crash on bad function just return illegal arity.

function_arity([lambda,As|_]) -> safe_length(As, 0);
function_arity(['match-lambda',[Pats|_]|_]) -> safe_length(Pats, 0);
function_arity(_) -> -1.

safe_length([_|Es], L) -> safe_length(Es, L+1);
safe_length([], L) -> L;
safe_length(_, _) -> -1.

%% collect_attrs(Attributes, MacroState) -> MacroState.
%%  We are only interested in which macros are exported.

collect_attrs([['export-macro'|Ms]|Atts], #umac{expm=Expm0}=Mst) ->
    Expm1 = add_exports(Expm0, Ms),
    collect_attrs(Atts, Mst#umac{expm=Expm1});
collect_attrs([_|Atts], Mst) -> collect_attrs(Atts, Mst);
collect_attrs([], Mst) -> Mst.

%% add_exports(Old, More) -> New.
%% exported_macro(Name, State) -> true | false.

add_exports(all, _) -> all;
add_exports(_, [all]) -> all;                   %Note we get a list of macros!
add_exports(Old, More) ->
    ordsets:union(Old, lists:usort(More)).

exported_macro(_, #umac{expm=all}) -> true;     %All are exported
exported_macro(Name, #umac{expm=Expm}) ->
    member(Name, Expm).

%% build_exported_macro(MacroState) -> ExportedMacFunc.
%%  Take the forms in the eval-when-compile and build the
%%  LFE-EXPAND-EXPORTED-MACRO function. In this version we expand the
%%  macros are compile time.

build_exported_macro(#umac{uleem=true}) -> [];  %Already have user defined LEEM
build_exported_macro(#umac{mline=L,expm=[]}) -> %No macros to export
    [{empty_leem(),L}];
build_exported_macro(#umac{mline=ModLine,env=Env}=Mst) ->
    Vfun = fun (N, V, Acc) -> [[N,V]|Acc] end,
    Sets = lfe_env:fold_vars(Vfun, [], Env),
    %% Collect the local functions.
    Ffun = fun (N, _, Def, Acc) ->
                   %% [[N,lfe_macro:expand_expr_all(Def, Env)]|Acc]
                   [[N,Def]|Acc]
           end,
    Funs = lfe_env:fold_funs(Ffun, [], Env),
    %% Collect the local macros.
    Mfun = fun (N, Def0, Acc) ->
                   case exported_macro(N, Mst) of
                       true ->
                           %% Def1 = lfe_macro:expand_expr_all(Def0, Env),
                           [macro_case_clause(N, Def0)|Acc];
                       false -> Acc
                   end
           end,
    %% Get the macros to export as case clauses.
    LEEM = case lfe_env:fold_macros(Mfun, [], Env) of
               [] -> empty_leem();              %No macros to export
               Macs ->
                   %% Build case, flet and let.
                   Case = ['case',?NAMEVAR|Macs ++ [['_',?Q(no)]]],
                   Flr = ['letrec-function',Funs,Case],
                   Fl = ['let',Sets,Flr],
                   ['define-function','LFE-EXPAND-EXPORTED-MACRO',?NOMETA,
                    [lambda,[?NAMEVAR,?ARGSVAR,'$ENV'],Fl]]
           end,
    [{LEEM,ModLine}].

empty_leem() ->
    ['define-function','LFE-EXPAND-EXPORTED-MACRO',?NOMETA,
     [lambda,['_','_','_'],?Q(no)]].

%% macro_case_clause(Name, Def) -> CaseClause.
%%  Build a case clause for expanding macr Name.

macro_case_clause(Name, Def) ->
    Cls = get_macro_cls(Def),
    Ccls = [ macro_clause(Args, B) || {Args,B} <- Cls ],
    [?Q(Name),['case',?ARGSVAR|Ccls]].          %Don't catch errors

%% get_macro_cls(MacroDef) -> [{ArgPat,Body}].
%%  Build a list of arg pattern and body for each clause. In the
%%  definition arguments the first is the argument pattern, the second
%%  is the environment variable $ENV. Be nice.

get_macro_cls(['lambda',[Arg|_]|B]) ->
    [{Arg,B}];                                  %Only one clause here
get_macro_cls(['match-lambda'|Cls]) ->
    [ {Arg,B} || [[Arg|_]|B] <- Cls ];
get_macro_cls(_) -> [].                         %Ignore bad formed macros

macro_clause(Args, [['when'|_]=W|Body]) ->
    [Args,W,[tuple,?Q(yes),[progn|Body]]];
macro_clause(Args, Body) ->
    [Args,[tuple,?Q(yes),[progn|Body]]].
