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

%% File    : lfe_qlc.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang interface to qlc transform.

%% Interface the standard Erlang QLC transformer. The only problem is
%% that this includes a lint check of the input QLC, which means it
%% will catch unbound variables. Seeing this call can be used from the
%% macro expander where we have no idea of variable binding we have to
%% be a bit cunning.

-module(lfe_qlc).

-export([expand/2]).

%% expand(ListComp, Options) -> {ok,Transform} | {error,Errors}.
%%  Instead of trying to find variables ourselves we first call the
%%  transformer with no variable bindings. We then create dummy
%%  variable bindings and call the transformer again. We will now get
%%  any "real" errors which we return.

expand(LC, _Opts) ->
    case qlc_pt:transform_expression(LC, []) of
        {not_ok,Errs} ->
            Uvs = unbound_vars(Errs, ordsets:new()),
            %% io:format("~p\n", [{Errs,Uvs}]),
            case qlc_pt:transform_expression(LC, Uvs) of
                {not_ok,Errors} -> {error,Errors};
                Ok              -> Ok
            end;
        Ok -> Ok
    end.

unbound_vars([{error,{_,_,{unbound_var,V}}}|Es], Uvs) ->
    unbound_vars(Es, ordsets:add_element({V,null}, Uvs));
unbound_vars([_|Es], Uvs) -> unbound_vars(Es, Uvs);
unbound_vars([], Uvs) -> Uvs.
