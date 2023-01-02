%% Copyright (c) 2016-2021 Robert Virding
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
%% Purpose : Define Lisp Flavoured Erlang internals.

%%% Define LFE internal bifs, guards and other internal stuff.

-module(lfe_internal).

%% General library functions.
-export([is_bif/2,is_guard_bif/2,is_erl_bif/2,is_lfe_bif/2]).
-export([is_core_form/1,is_core_func/2]).
-export([is_type/2]).

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
is_core_form(msiz) -> true;
is_core_form(mref) -> true;
is_core_form(mset) -> true;
is_core_form(mupd) -> true;
is_core_form(mrem) -> true;
is_core_form('map-size') -> true;
is_core_form('map-get') -> true;
is_core_form('map-set') -> true;
is_core_form('map-update') -> true;
is_core_form('map-remove') -> true;
%% Core record special forms.
is_core_form('record') -> true;
%% make-record has been deprecated but we sill accept it for now.
is_core_form('make-record') -> true;
is_core_form('is-record') -> true;
is_core_form('record-index') -> true;
is_core_form('record-field') -> true;
is_core_form('record-update') -> true;
%% Core struct special forms.
is_core_form('struct') -> true;
is_core_form('is-struct') -> true;
is_core_form('struct-field') -> true;
is_core_form('struct-update') -> true;
%% Function forms.
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
%% List/binary comprehensions.
is_core_form('lc') -> true;
is_core_form('list-comp') -> true;
is_core_form('bc') -> true;
is_core_form('binary-comp') -> true;
%% Core definition special forms.
is_core_form('eval-when-compile') -> true;
is_core_form('define-module') -> true;
is_core_form('extend-module') -> true;
is_core_form('define-type') -> true;
is_core_form('define-opaque-type') -> true;
is_core_form('define-function-spec') -> true;
is_core_form('define-function') -> true;
is_core_form('define-macro') -> true;
is_core_form('define-record') -> true;
is_core_form('define-struct') -> true;
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
is_core_func(msiz, 1) -> true;
is_core_func(mref, 2) -> true;
is_core_func(mset, Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_core_func(mupd, Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_core_func(mrem, Ar) when Ar >= 1 -> true;
is_core_func('map-size', 1) -> true;
is_core_func('map-get', 2) -> true;
is_core_func('map-set', Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_core_func('map-update', Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_core_func('map-remove', Ar) when Ar >= 1 -> true;
%% Core record special functions.
is_core_func('record', Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
%% make-record has been deprecated but we sill accept it for now.
is_core_func('make-record', Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_core_func('is-record', 2) -> true;
is_core_func('record-index', 2) -> true;
is_core_func('record-field', 3) -> true;
is_core_func('record-update', Ar) when Ar >= 2, (Ar rem 2) =:= 0 -> true;
%% Core struct special functions.
is_core_func('struct', Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_core_func('is-struct', Ar) when Ar =:= 1; Ar =:= 2 -> true;
is_core_func('struct-field', 3) -> true;
is_core_func('struct-update', Ar) when Ar >= 2, (Ar rem 2) =:= 0 -> true;
%% List/binary comprehensions.
is_core_func('lc', 2) -> true;
is_core_func('list-comp', 2) -> true;
is_core_func('bc', 2) -> true;
is_core_func('binary-comp', 2) -> true;
%% Core control special functions.
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

%% is_type(NAme, Arity) -> bool().
%%  Return true if Name/Arity is a predefined type.

is_type('UNION', Ar) -> is_integer(Ar) and (Ar >= 0);
is_type(call, Ar) -> is_integer(Ar) and (Ar >= 0);
is_type(lambda, Ar) -> is_integer(Ar) and (Ar >= 0);
is_type(map, Ar) -> is_integer(Ar) and (Ar >= 0);
is_type(range, 2) -> true;
is_type(bitstring, 2) -> true;
is_type(tuple, Ar) -> is_integer(Ar) and (Ar >= 0);
is_type(Name, Arity) ->
    erl_internal:is_type(Name, Arity).
