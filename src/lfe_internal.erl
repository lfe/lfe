%% Copyright (c) 2016 Robert Virding
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

%% File    : lfe_internal.erl
%% Author  : Robert Virding
%% Purpose : Define Lisp Flavoured Erlang internal bifs, guards.

-module(lfe_internal).

%% General library functions.
-export([is_bif/2,is_guard_bif/2,is_erl_bif/2,is_lfe_bif/2]).
-export([is_core_form/1,is_core_func/2]).

%% -compile([export_all]).

%% is_bif(Name, Arity) -> bool().
%% is_guard_bif(Name, Arity) -> bool().
%% is_erl_bif(Name, Arity) -> bool().
%%  Collected tests for valid BIFs in expressions and guards.

is_bif(Name, Ar) ->
    is_core_func(Name, Ar)
%%        orelse is_lfe_bif(Name, Ar)
        orelse is_erl_bif(Name, Ar).

is_guard_bif(Op ,Ar) ->
    erl_internal:guard_bif(Op, Ar)
    orelse erl_internal:arith_op(Op, Ar)
    orelse erl_internal:bool_op(Op, Ar)
    orelse erl_internal:comp_op(Op, Ar).

is_erl_bif(Op, Ar) ->
    erl_internal:bif(Op, Ar)
    orelse erl_internal:arith_op(Op, Ar)
    orelse erl_internal:bool_op(Op, Ar)
    orelse erl_internal:comp_op(Op, Ar)
    orelse erl_internal:list_op(Op, Ar)
    orelse erl_internal:send_op(Op, Ar).

%% is_core_form(Form) -> bool().
%%  Return true if Form (name) is one of the LFE core forms, else false.

%% Core data special forms.
is_core_form(quote) -> true;
is_core_form(cons) -> true;
is_core_form(car) -> true;
is_core_form(cdr) -> true;
is_core_form(list) -> true;
is_core_form(tuple) -> true;
is_core_form(tref) -> true;
is_core_form(tset) -> true;
is_core_form(binary) -> true;
is_core_form(map) -> true;
is_core_form(mref) -> true;
is_core_form(mset) -> true;
is_core_form(mupd) -> true;
is_core_form('map-get') -> true;
is_core_form('map-set') -> true;
is_core_form('map-update') -> true;
is_core_form(function) -> true;
%% Core closure special forms.
is_core_form(lambda) -> true;
is_core_form('match-lambda') -> true;
is_core_form('let') -> true;
is_core_form('let-function') -> true;
is_core_form('letrec-function') -> true;
is_core_form('let-macro') -> true;
%% Core control special forms.
is_core_form('progn') -> true;
is_core_form('if') -> true;
is_core_form('case') -> true;
is_core_form('receive') -> true;
is_core_form('catch') -> true;
is_core_form('try') -> true;
is_core_form('funcall') -> true;
is_core_form(call) -> true;
%% Core definition special forms.
is_core_form('eval-when-compile') -> true;
is_core_form('define-module') -> true;
is_core_form('extend-module') -> true;
is_core_form('define-type') -> true;
is_core_form('define-opaque-type') -> true;
is_core_form('define-function-spec') -> true;
is_core_form('define-function') -> true;
is_core_form('define-macro') -> true;
%% And don't forget when.
is_core_form('when') -> true;
%% Everything else is not a core form.
is_core_form(_) -> false.

%% is_core_func(Name, Arity) -> bool().
%%  Return true if Name/Arity is one of the LFE core functions, else
%%  false. For those which can take multiple arguments we accept any
%%  number and push checking to run time.

is_core_func(cons, 2) -> true;
is_core_func(car, 1) -> true;
is_core_func(cdr, 1) -> true;
is_core_func(list, Ar) when Ar >= 0 -> true;
is_core_func(tuple, Ar) when Ar >= 0  -> true;
is_core_func(tref, 2) -> true;
is_core_func(tset, 3) -> true;
is_core_func(binary, Ar) when Ar >= 0  -> true;
is_core_func(map, Ar) when Ar >= 0, (Ar rem 2) =:= 0 -> true;
is_core_func(mref, 2) -> true;
is_core_func(mset, Ar) when Ar >= 0, (Ar rem 2) =:= 1 -> true;
is_core_func(mupd, Ar) when Ar >= 0, (Ar rem 2) =:= 1 -> true;
is_core_func('map-get', 2) -> true;
is_core_func('map-set', Ar) when Ar >= 0, (Ar rem 2) =:= 1 -> true;
is_core_func('map-upd', Ar) when Ar >= 0, (Ar rem 2) =:= 1 -> true;
is_core_func(funcall, Ar) when Ar >= 1 -> true;
is_core_func(call, Ar) when Ar >= 2 -> true;
is_core_func(_, _) -> false.

%% is_lfe_bif(Name, Arity) -> bool().
%%  Return true if Name/Arity is one of the standard LFE bifs defined
%%  in the lfe module.

is_lfe_bif(eval, 1) -> true;
is_lfe_bif(eval, 2) -> true;
is_lfe_bif('macro-function', 1) -> true;
is_lfe_bif('macro-function', 2) -> true;
is_lfe_bif(macroexpand, 1) -> true;
is_lfe_bif(macroexpand, 2) -> true;
is_lfe_bif('macroexpand-1', 1) -> true;
is_lfe_bif('macroexpand-1', 2) -> true;
is_lfe_bif('macroexpand-all', 1) -> true;
is_lfe_bif('macroexpand-all', 2) -> true;
is_lfe_bif(_, _) -> false.
