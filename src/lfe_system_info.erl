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

%% File    : lfe_system_info.erl
%% Author  : Duncan McGreggor
%% Purpose : Lisp Flavoured Erlang functions returning system data.

-module(lfe_system_info).

-export([app_version/1,
         os_versions/0,
         core_versions/0,
         tool_versions/0,
         version/0, version/1]).

-define(APP_LOOKUP_ERROR, {error, 'app-not-found'}).
-define(TOOL_LOOKUP_ERROR, {error, 'tool-not-found'}).
-define(NEWLINE, 10).

trim(String) ->
    string:strip(String, both, ?NEWLINE).

cmd(String) ->
    trim(os:cmd(String)).

app_version(AppName) ->
    application:load(AppName),
    case application:get_key(AppName, vsn) of
        {ok,Vsn} -> Vsn;
        _ -> ?APP_LOOKUP_ERROR
    end.

os_version () ->
    case os:type() of
        {unix, _} -> [{os, cmd("uname -a")}];
        {_, nt} -> [{os, cmd("ver")}];
        _ -> []
    end.

kernel_version() ->
    case os:type() of
        {unix, _} -> [{kernel, os:version()}];
        _ -> []
    end.

os_versions() ->
    os_version() ++ kernel_version().

core_versions() ->
    [{otp, erlang:system_info(otp_release)},
     {emulator, erlang:system_info(version)},
     {system, trim(erlang:system_info(system_version))},
     {driver, erlang:system_info(driver_version)},
     {nif, erlang:system_info(nif_version)},
     {lfe, app_version(lfe)}].

rebar_version() ->
    case os:find_executable(rebar) of
        false -> [];
        _ -> [{rebar, cmd("rebar --version")}]
    end.

rebar3_version() ->
    case os:find_executable(rebar3) of
        false -> [];
        _ -> [{rebar3, cmd("rebar3 --version")}]
    end.

mix_version() ->
    case os:find_executable(mix) of
        false -> [];
        _ -> [{mix, cmd("mix --version")}]
    end.

relx_version() ->
    case os:find_executable(relx) of
        false -> [];
        _ -> [{relx, cmd("relx --version")}]
    end.

tool_versions() ->
    rebar_version() ++ rebar3_version() ++ mix_version() ++ relx_version().

uncached_version() ->
    os_versions() ++ core_versions() ++ tool_versions().

version() ->
    case erlang:get(lfe_version_info) of
        undefined -> Ver = uncached_version(),
                     erlang:put(lfe_version_info, Ver),
                     Ver;
        Vsn -> Vsn
    end.

version(AppName) ->
    case proplists:get_value(AppName, version()) of
        {error, _} -> ?APP_LOOKUP_ERROR;
        undefined -> ?APP_LOOKUP_ERROR;
        Vsn -> Vsn
    end.
