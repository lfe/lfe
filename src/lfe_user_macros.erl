%% Copyright (c) 2015 Robert Virding
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

%% File    : lfe_user_macros.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang user macro function builder.

%% Build the LFE-EXPAND-USER-MACRO function which exports macros so
%% they can be found by the macro expander without needing to inlcude
%% them. If the module foo exports macro bar then it can be called by
%% doing (foo:bar ...).
%%
%% This version expands the macros when the defining module is
%% compiled so they are expanded in the context when that module is
%% compiled, not when they are called. This is easy and makes it easy
%% to access all macros in the defining module.
%%
%% An alternative would be to expand the macros when they are called
%% but then it becomes difficult to access all the macros within the
%% defining module. This might be easy if we accept exporting all
%% macros not just specific ones.

%% (defun LFE-EXPAND-USER-MACRO (call $ENV)
%%   (let ((var-1 val-1)                   ;Eval-when-compile variables
%%         ...)
%%     (fletrec ((fun-1 ...)               ;Eval-when-compile functions
%%               ...)
%%       (case call                        ;Macro call without module
%%         (`(mac-1 ...) ...)              ;Already exported local macros
%%         (`(mac-2 ...) ...)
%%         ...
%%         (_ 'no)))))

-module(lfe_user_macros).

-compile(export_all).

-include("lfe_comp.hrl").

-import(lists, [reverse/1,reverse/2,member/2,filter/2]).

%% We do a lot of quoting!
-define(Q(E), [quote,E]).
-define(BQ(E), [backquote,E]).
-define(C(E), [comma,E]).
-define(C_A(E), ['comma-at',E]).

-record(umac, {mline=[],expm=[]}).

%% module(ModuleForms, CompInfo) -> {ModuleName,ModuleForms}.
%%  Expand the forms to handle parameterised modules if necessary,
%%  otherwise just pass forms straight through.

module([{['define-module',Name|Mdef],L}|Fs0], _Ci) ->
    St0 = collect_mdef(Mdef, #umac{mline=L}),
    {Ffs,Ewcs,St1} = extract_ewcs(Fs0, St0),
    Umac = build_user_macro(Ewcs, St1),
    Md1 = {['define-module',Name,[export,['LFE-EXPAND-USER-MACRO',2]]|Mdef],L},
    %% lfe_io:format("~p\n", [{['eval-when-compile'|Ewcs],{Umac,L},St1}]),
    {Name,[Md1,{Umac,L}|Ffs]};
module(Fs, _) -> {[],Fs}.                       %Not a module, do nothing

%% collect_mdef(ModuleDef, State) -> State.
%%  We are only interested in which macros are exported.

collect_mdef([['export-macro'|Ms]|Mdef], #umac{expm=Expm0}=St) ->
    Expm1 = add_exports(Expm0, Ms),
    collect_mdef(Mdef, St#umac{expm=Expm1});
collect_mdef([_|Mdef], St) -> collect_mdef(Mdef, St);
collect_mdef([], St) -> St.

%% add_exports(Old, More) -> New.

add_exports(all, _) -> all;
add_exports(Old, More) ->
    ordsets:union(Old, lists:usort(More)).

exported_macro(_, #umac{expm=all}) -> true;     %All are exported
exported_macro(Name, #umac{expm=Expm}) ->
    member(Name, Expm).

%% extract_ewcs(ModuleForms, State) -> {FunctionForms,EwcForms,State).
%%  Extract out the EvalWhenCompile forms from the module.

extract_ewcs(Fs, St) ->
    extract_ewcs(Fs, [], [], St).

extract_ewcs([{['eval-when-compile'|Es],_}|Fs], Ffs, Ewcs, St) ->
    extract_ewcs(Fs, Ffs, reverse(Es, Ewcs), St);
extract_ewcs([F|Fs], Ffs, Ewcs, St) ->          %Everything else
    extract_ewcs(Fs, [F|Ffs], Ewcs, St);
extract_ewcs([], Ffs, Ewcs, St) ->
    {reverse(Ffs),reverse(Ewcs),St}.

%% build_user_macro(EvalWhenComps, State) -> UserMacFunc.
%%  Take the forms in the eval-when-compile and build the
%%  LFE-EXPAND-USER-MACRO function. In this version we expand the
%%  macros are compile time.

build_user_macro(_, #umac{expm=[]}) ->          %Nothing to export
    ['define-function','LFE-EXPAND-USER-MACRO',[lambda,[call,'$ENV'],?Q(no)]];
build_user_macro(Ewcs, St) ->
    %% Extract the intersting bits.
    Funs = [ {Name,function_arity(Def),Def} ||
               ['define-function',Name,Def] <- Ewcs ],
    Macs = [ {Name,Def} || ['define-macro',Name,Def] <- Ewcs ],
    Sets = [ {Pat,Val} || [set,Pat,Val] <- Ewcs ],
    %% Build an environment to expand local macros.
    Env0 = lfe_env:new(),
    Env1 = lfe_env:add_vbindings(Sets, Env0),
    Env2 = lfe_env:add_fbindings(Funs, Env1),
    Env3 = lfe_env:add_mbindings(Macs, Env2),
    %% Expand the macros and functions.
    Efuns = [ [Name,lfe_macro:expand_expr_all(Def, Env3)] ||
                {Name,_,Def} <- Funs ],
    Emacs = [ [Name,lfe_macro:expand_expr_all(Def, Env3)] ||
                {Name,Def} <- Macs ],
    Esets = [ [Name,Val] || {Name,Val} <- Sets ],
    %% Build the macro case, function fletrec and variable let.
    Ccs = [ macro_clause(Name, Arg, B) ||
              [Name,Def] <- Emacs,
              exported_macro(Name, St),
              {Arg,B} <- get_macro_cls(Def) ],
    CallVar = '|- call -|',                     %Give this a funny name
    Case = ['case',CallVar|Ccs ++ [['_',?Q(no)]]],
    Flr = ['letrec-function',Efuns,Case],
    Fl = ['let',Esets,Flr],
    %% Now put it all together.
    ['define-function','LFE-EXPAND-USER-MACRO',[lambda,[CallVar,'$ENV'],Fl]].

function_arity([lambda,As|_]) -> length(As);
function_arity(['match-lambda',[Pats|_]|_]) ->
    length(Pats).

%% get_macro_cls(MacroDef) -> [{ArgPat,Body}].
%%  Build a list of arg pattern and body for each clause. In the
%%  arguments the first arg is valid here, the second is the
%%  environment variable $ENV. Be nice.

get_macro_cls(['lambda',[Arg|_]|B]) ->
    [{Arg,B}];                                  %Only one here
get_macro_cls(['match-lambda'|Cls]) ->
    [ {Arg,B} || [[Arg|_]|B] <- Cls ];
get_macro_cls(_) -> [].                         %Ignore bad formed macros

%% macro_args(Name, ArgsPat) -> CallPat.
%%  Build a pattern which matches against a call the macro. The call includes

macro_args(Name, []) -> [cons,Name,[]];
macro_args(Name, [list|List]) -> [list,Name|List];
macro_args(Name, [cons,H,T]) -> [cons,Name,[cons,H,T]];
macro_args(Name, Arg) -> [cons,Name,Arg].

macro_clause(Name, Arg, Body) ->
    B = [tuple,?Q(yes),[progn|Body]],
    [macro_args(?Q(Name), Arg),B].
