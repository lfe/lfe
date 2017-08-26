%% Copyright (c) 2017 Robert Virding
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

%%% File    : lfe_abstract_code.erl
%%% Author  : Robert Virding
%%% Purpose : Extract LFE abstract code for BEAM data.

%% This is quite simple as the compiler produces Core erlang for us so
%% there is no need to convert it when the debug info is
%% required. However we cannot return Erlang AST.

-module(lfe_abstract_code).

-export([make_debug_info/2,make_chunk/2,debug_info/4]).

%% debug_info(Format, Module, Data, Options) -> {ok,Code} | {error,Error}.

debug_info(core_v1, _Mod, {Data,_Copts}, _Opts) ->
    {ok,Data};
debug_info(_Format, _, _, _) ->
    {error,unknown_format}.

%% make_debug_info(Core, Opts) -> {ok,DebugInfo}.
%% make_chunk(Core, Opts) -> {ok,Chunk}.

make_debug_info(Core, Opts) ->
    %%io:format("~p\n", [Core]),
    {ok,{debug_info_v1,?MODULE,{Core,Opts}}}.

make_chunk(Core, Opts) ->
    {ok,DebugInfo} = make_debug_info(Core, Opts),
    Chunk = {"Dbgi",erlang:term_to_binary(DebugInfo, [compressed])},
    {ok,Chunk}.
