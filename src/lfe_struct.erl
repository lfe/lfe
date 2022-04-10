%% Copyright (c) 2022 Robert Virding
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

%% File    : lfe_struct.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang library for elixir structs.

-module(lfe_struct).

-export([to_assocs/1]).

-include("lfe.hrl").

to_assocs([Key,Val|Kvs]) ->
    [[tuple,Key,Val]|to_assocs(Kvs)];
to_assocs([Key]) ->                             %Should we catch this?
    error({missing_struct_field_value,Key});
to_assocs([]) -> [].
