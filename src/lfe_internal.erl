%% -*- mode: erlang; indent-tabs-mode: nil -*-
%% Copyright (c) 2016-2024 Robert Virding
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
-export([is_core_form/1,is_core_func/2,is_guard_func/2]).
-export([is_arith_func/2,is_bit_func/2,is_bool_func/2,is_comp_func/2,
         is_map_func/2,is_record_func/2,is_struct_func/2,is_list_func/2]).
-export([is_type/2]).

%% -compile([export_all]).

%% is_bif(Name, Arity) -> bool().
%% is_guard_bif(Name, Arity) -> bool().
%% is_erl_bif(Name, Arity) -> bool().
%%  Collected tests for valid BIFs in expressions and guards.

is_bif(Name, Ar) ->
    is_core_func(Name, Ar)
        %% orelse is_lfe_bif(Name, Ar)
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
is_core_form('prog1') -> true;
is_core_form('prog2') -> true;
is_core_form('if') -> true;
is_core_form('case') -> true;
is_core_form('cond') -> true;
is_core_form('maybe') -> true;
is_core_form('receive') -> true;
is_core_form('catch') -> true;
is_core_form('try') -> true;
is_core_form('funcall') -> true;
is_core_form('call') -> true;
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
%% And don't forget when and else.
is_core_form('when') -> true;
is_core_form('else') -> true;
%% Everything else is not a core form.
is_core_form(Name) when is_atom(Name) -> false.

%% is_arith_func(Name, Arity) -> bool().
%% is_bit_func(Name, Arityy) -> bool().
%% is_bool_func(Name, Arity) -> bool().
%% is_record_func(Name, Arity) -> bool().
%% is_struct_func(Name, Arity) -> bool().
%% is_list_func(Name, Arity) -> bool().
%%  Return true if Name/Arity is one of the LFE core functions, else false.

%% Arithmetic functions.
is_arith_func('+', Ar) when Ar >= 1 -> true;
is_arith_func('-', Ar) when Ar >= 1 -> true;
is_arith_func('*', Ar) when Ar >= 2 -> true;
is_arith_func('/', Ar) when Ar >= 2 -> true;
is_arith_func(Name, Ar) when is_atom(Name), is_integer(Ar) -> false.

%% Bit functions.
is_bit_func('bnot', 1) -> true;
is_bit_func('div', 2) -> true;
is_bit_func('rem', 2) -> true;
is_bit_func('band', 2) -> true;
is_bit_func('bor', 2) -> true;
is_bit_func('bxor', 2) -> true;
is_bit_func('bsl', 2) -> true;
is_bit_func('bsr', 2) -> true;
is_bit_func(Name, Ar) when is_atom(Name), is_integer(Ar) -> false.

%% Boolean functions.
is_bool_func('and', Ar) when Ar >= 2 -> true;
is_bool_func('or', Ar) when Ar >= 2 -> true;
is_bool_func('xor', Ar) when Ar >= 2 -> true;
is_bool_func('not', 1) -> true;
is_bool_func(Name, Ar) when is_atom(Name), is_integer(Ar) -> false.

%% Comparison functions.
is_comp_func('>', Ar) when Ar >= 2 -> true;
is_comp_func('>=', Ar) when Ar >= 2 -> true;
is_comp_func('<', Ar) when Ar >= 2 -> true;
is_comp_func('=<', Ar) when Ar >= 2 -> true;
is_comp_func('==', Ar) when Ar >= 2 -> true;
is_comp_func('=:=', Ar) when Ar >= 2 -> true;
is_comp_func('/=', Ar) when Ar >= 2 -> true;
is_comp_func('=/=', Ar) when Ar >= 2 -> true;
is_comp_func(Name, Ar) when is_atom(Name), is_integer(Ar) -> false.

%% Map functions, both the normal and more CL based.
is_map_func(map, Ar) when Ar >= 0, (Ar rem 2) =:= 0 -> true;
is_map_func(msiz, 1) -> true;
is_map_func(mref, 2) -> true;
is_map_func(mset, Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_map_func(mupd, Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_map_func(mrem, Ar) when Ar >= 1 -> true;
is_map_func('map-size', 1) -> true;
is_map_func('map-get', 2) -> true;
is_map_func('map-set', Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_map_func('map-update', Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_map_func('map-remove', Ar) when Ar >= 1 -> true;
is_map_func(Name, Ar) when is_atom(Name), is_integer(Ar) -> false.

%% Record functions.
is_record_func('record', Ar) when Ar >= 1 -> true;
is_record_func('make-record', Ar) when Ar >= 1 -> true;
is_record_func('is-record', 2) -> true;
is_record_func('record-index', 2) -> true;
is_record_func('record-field', 3) -> true;
is_record_func('record-update', Ar) when Ar >= 2, (Ar rem 2) =:= 0 -> true;
is_record_func(Name, Ar) when is_atom(Name), is_integer(Ar) -> false.

%% Struct functions.
is_struct_func('struct', Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_struct_func('is-struct', Ar) when Ar =:= 1; Ar =:= 2 -> true;
is_struct_func('struct-field', 3) -> true;
is_struct_func('struct-update', Ar) when Ar >= 2, (Ar rem 2) =:= 0 -> true;
is_struct_func(Name, Ar) when is_atom(Name), is_integer(Ar) -> false.

%% List functions.
is_list_func('++', Ar) when Ar >= 1 -> true;
is_list_func('--', Ar) when Ar >= 1 -> true;
is_list_func(Name, Ar) when is_atom(Name), is_integer(Ar) -> false.

%% is_core_func(Name, Arity) -> bool().
%%  Return true if Name/Arity is one of the LFE core functions, else
%%  false. For those which can take multiple arguments we accept any
%%  number and push checking to run time.

%% Core data special functions.
is_core_func(quote, 1) -> true;
is_core_func(cons, 2) -> true;
is_core_func(car, 1) -> true;
is_core_func(cdr, 1) -> true;
is_core_func(list, Ar) when Ar >= 0 -> true;
is_core_func(tuple, Ar) when Ar >= 0  -> true;
is_core_func(tref, 2) -> true;
is_core_func(tset, 3) -> true;
is_core_func(binary, Ar) when Ar >= 0  -> true;
%% Core arithmetic functions.
is_core_func('+', Ar) when Ar >= 1 -> true;
is_core_func('-', Ar) when Ar >= 1 -> true;
is_core_func('*', Ar) when Ar >= 1 -> true;
is_core_func('/', Ar) when Ar >= 1 -> true;
%% Core bit functions.
is_core_func('bnot', 1) -> true;
is_core_func('div', 2) -> true;
is_core_func('rem', 2) -> true;
is_core_func('band', 2) -> true;
is_core_func('bor', 2) -> true;
is_core_func('bxor', 2) -> true;
is_core_func('bsl', 2) -> true;
is_core_func('bsr', 2) -> true;
%% Core boolean functions.
is_core_func('and', Ar) when Ar >= 1 -> true;
is_core_func('or', Ar) when Ar >= 1 -> true;
is_core_func('xor', Ar) when Ar >= 1 -> true;
is_core_func('not', 1) -> true;
%% Core comparison functions.
is_core_func('>', Ar) when Ar >= 1 -> true;
is_core_func('>=', Ar) when Ar >= 1 -> true;
is_core_func('<', Ar) when Ar >= 1 -> true;
is_core_func('=<', Ar) when Ar >= 1 -> true;
is_core_func('==', Ar) when Ar >= 1 -> true;
is_core_func('=:=', Ar) when Ar >= 1 -> true;
is_core_func('/=', Ar) when Ar >= 1 -> true;
is_core_func('=/=', Ar) when Ar >= 1 -> true;
%% Core map functions, both the normal and more CL based.
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
%% Core record functions.
is_core_func('record', Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
%% make-record has been deprecated but we sill accept it for now.
is_core_func('make-record', Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_core_func('is-record', 2) -> true;
is_core_func('record-index', 2) -> true;
is_core_func('record-field', 3) -> true;
is_core_func('record-update', Ar) when Ar >= 2, (Ar rem 2) =:= 0 -> true;
%% Core struct functions.
is_core_func('struct', Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_core_func('is-struct', Ar) when Ar =:= 1; Ar =:= 2 -> true;
is_core_func('struct-field', 3) -> true;
is_core_func('struct-update', Ar) when Ar >= 2, (Ar rem 2) =:= 0 -> true;
%% List/binary comprehensions.
is_core_func('lc', 2) -> true;
is_core_func('list-comp', 2) -> true;
is_core_func('bc', 2) -> true;
is_core_func('binary-comp', 2) -> true;
%% List functions.
is_core_func('++', Ar) when Ar >= 1 -> true;
is_core_func('--', Ar) when Ar >= 1 -> true;
%% Core control special functions.
is_core_func('funcall', Ar) when Ar >= 1 -> true;
is_core_func('call', Ar) when Ar >= 2 -> true;
%% Everything else is not a core function.
is_core_func(Name, Ar) when is_atom(Name), is_integer(Ar) -> false.

%% is_guard_func(Name, Arity) -> bool().
%%  Return true if Name/Arity is one of the LFE core functions, else
%%  false. For those which can take multiple arguments we accept any
%%  number and push checking to run time.

%% Data special functions.
is_guard_func(quote, 1) -> true;
is_guard_func(cons, 2) -> true;
is_guard_func(car, 1) -> true;
is_guard_func(cdr, 1) -> true;
is_guard_func(list, Ar) when Ar >= 0 -> true;
is_guard_func(tuple, Ar) when Ar >= 0  -> true;
is_guard_func(tref, 2) -> true;
is_guard_func(tset, 3) -> true;
is_guard_func(binary, Ar) when Ar >= 0  -> true;
%% Arithmetic functions.
is_guard_func('+', Ar) when Ar >= 1 -> true;
is_guard_func('-', Ar) when Ar >= 1 -> true;
is_guard_func('*', Ar) when Ar >= 1 -> true;
is_guard_func('/', Ar) when Ar >= 1 -> true;
%% Bit functions.
is_guard_func('bnot', 1) -> true;
is_guard_func('div', 2) -> true;
is_guard_func('rem', 2) -> true;
is_guard_func('band', 2) -> true;
is_guard_func('bor', 2) -> true;
is_guard_func('bxor', 2) -> true;
is_guard_func('bsl', 2) -> true;
is_guard_func('bsr', 2) -> true;
%% Boolean functions.
is_guard_func('and', Ar) when Ar >= 1 -> true;
is_guard_func('or', Ar) when Ar >= 1 -> true;
is_guard_func('xor', Ar) when Ar >= 1 -> true;
is_guard_func('not', 1) -> true;
%% Comparison functions.
is_guard_func('>', Ar) when Ar >= 1 -> true;
is_guard_func('>=', Ar) when Ar >= 1 -> true;
is_guard_func('<', Ar) when Ar >= 1 -> true;
is_guard_func('=<', Ar) when Ar >= 1 -> true;
is_guard_func('==', Ar) when Ar >= 1 -> true;
is_guard_func('=:=', Ar) when Ar >= 1 -> true;
is_guard_func('/=', Ar) when Ar >= 1 -> true;
is_guard_func('=/=', Ar) when Ar >= 1 -> true;
%% Map functions, both the normal and more CL based.
is_guard_func(map, Ar) when Ar >= 0, (Ar rem 2) =:= 0 -> true;
is_guard_func(msiz, 1) -> true;
is_guard_func(mref, 2) -> true;
is_guard_func(mset, Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_guard_func(mupd, Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_guard_func(mrem, Ar) when Ar >= 1 -> true;
is_guard_func('map-size', 1) -> true;
is_guard_func('map-get', 2) -> true;
is_guard_func('map-set', Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_guard_func('map-update', Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_guard_func('map-remove', Ar) when Ar >= 1 -> true;
%% Record functions.
is_guard_func('record', Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_guard_func('is-record', 2) -> true;
is_guard_func('record-index', 2) -> true;
is_guard_func('record-field', 3) -> true;
%% Struct functions.
is_guard_func('struct', Ar) when Ar >= 1, (Ar rem 2) =:= 1 -> true;
is_guard_func('is-struct', Ar) when Ar =:= 1; Ar =:= 2 -> true;
is_guard_func('struct-field', 3) -> true;
is_guard_func('struct-update', Ar) when Ar >= 2, (Ar rem 2) =:= 0 -> true;
%% List functions.
is_guard_func('++', Ar) when Ar >= 1 -> true;
is_guard_func('--', Ar) when Ar >= 1 -> true;
%% Everything else is not a guard function.
is_guard_func(Name, Ar) when is_atom(Name), is_integer(Ar) -> false.

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
is_lfe_bif(Name, Ar) when is_atom(Name), is_integer(Ar) -> false.

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
