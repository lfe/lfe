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

%% File    : lfe.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang standard library.

-module(lfe).

%% Standard lisp library.
-export([eval/1,eval/2,
         'macro-function'/1,'macro-function'/2,
         macroexpand/1,macroexpand/2,
         'macroexpand-1'/1,'macroexpand-1'/2,
         'macroexpand-all'/1,'macroexpand-all'/2]).

-export(['LFE-EXPAND-EXPORTED-MACRO'/3]).

%% 'LFE-EXPAND-EXPORTED-MACRO'(Name, Args, Env) -> {yes,Expansion} | no.
%%  A smart implementation of this where we call the lfe_macro module
%%  to expand and check for us. Using an empty environment ensures
%%  only predefined macros are used.

'LFE-EXPAND-EXPORTED-MACRO'(Name, Args, _) ->
    lfe_macro:expand_expr([Name|Args], lfe_env:new()).

%% Standard lisp library functions.
%%  eval(Sexpr) -> Value.
%%  macro-function(Name [,Environment]) -> Macro | [].
%%  macroexpand(Form [,Environment]) -> Expansion | Form.
%%  macroexpand-1(Form [,Environment]) -> Expansion | Form.
%%  macroexpand-all(Form [,Environment]) -> Expansion | Form.

eval(Sexpr) -> eval(Sexpr, lfe_env:new()).      %Empty environment.
eval(Sexpr, Env) -> lfe_eval:expr(Sexpr, Env).

'macro-function'(Symb) -> 'macro-function'(Symb, lfe_env:new()).
'macro-function'(Symb, Env) ->
    case lfe_env:get_mbinding(Symb, Env) of
        {yes,Macro} ->
            Macro;
        no -> []
    end.

macroexpand(Form) -> macroexpand(Form, lfe_env:new()).
macroexpand(Form, Env) ->
    case lfe_macro:expand_expr(Form, Env) of
        {yes,Exp} -> Exp;
        no -> Form
    end.

'macroexpand-1'(Form) -> 'macroexpand-1'(Form, lfe_env:new()).
'macroexpand-1'(Form, Env) ->
    case lfe_macro:expand_expr_1(Form, Env) of
        {yes,Exp} -> Exp;
        no -> Form
    end.

'macroexpand-all'(Form) -> 'macroexpand-all'(Form, lfe_env:new()).
'macroexpand-all'(Form, Env) -> lfe_macro:expand_expr_all(Form, Env).
