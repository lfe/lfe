%% Copyright (c) 2017-2020 Robert Virding
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

%% This has been copied from erl_abstract_code.erl.

-module(lfe_abstract_code).
-export([debug_info/4]).

%% debug_info(Format, Module, Data, Options) -> {ok,Code} | {error,Error}.

debug_info(_Format, _Module, {none,_Copts}, _Opts) ->
    {error, missing};
debug_info(erlang_v1, _Mod, {AbstrCode,_CompilerOpts}, _Opts) ->
    {ok,AbstrCode};
debug_info(core_v1, _Mod, {AbstrCode,CompilerOpts}, Opts) ->
    CoreOpts = add_core_returns(delete_reports(CompilerOpts ++ Opts)),
    try compile:noenv_forms(AbstrCode, CoreOpts) of
	{ok,_,Core,_} -> {ok,Core};
	_Error -> {error,failed_conversion}
    catch
	error:_E -> {error,failed_conversion}
    end;
debug_info(_Format, _, _Data, _) ->
    {error,unknown_format}.

delete_reports(Opts) ->
    [Opt || Opt <- Opts, not is_report_option(Opt)].

is_report_option(report) -> true;
is_report_option(report_errors) -> true;
is_report_option(report_warnings) -> true;
is_report_option(_) -> false.

add_core_returns(Opts) ->
    [to_core,return_errors,return_warnings] ++ Opts.
