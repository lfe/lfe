%% Copyright (c) 2008-2020 Robert Virding
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

%%% File    : lfe_macro_struct.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang macro expander for structs.

-module(lfe_macro_struct).

-export([define/3,format_error/1]).

-import(lists, [map/2,foldr/3,concat/1]).

-include("lfe.hrl").
-include("lfe_macro.hrl").

%% Errors.
format_error(bad_struct_def) -> <<"bad definition of struct">>;
format_error(_) -> "struct error".

define(Fdefs0, Env, St0) ->
    Fdefs1 = evaluate_fdefs(Fdefs0, Env),
    {yes,[progn,['define-struct',Fdefs1]],St0}.

evaluate_fdefs(Fdefs0, Env) ->
    Fun = fun ([F,Def,T]) when is_atom(F) -> [F,lfe_eval:expr(Def, Env),T];
	      ([F,Def]) when is_atom(F) -> [F,lfe_eval:expr(Def, Env)];
	      ([F]) when is_atom(F) -> [F];
	      (F) when is_atom(F) -> F;
	      (_) -> bad_struct_def_error()
	  end,
    Fdefs1 = lists:map(Fun, Fdefs0),
    Fdefs1.

bad_struct_def_error() -> error(bad_struct_def).
