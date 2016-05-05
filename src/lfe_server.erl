%% Copyright (c) 2008-2015 Robert Virding
%% Copyright (c) 2015 Duncan McGreggor
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

%% File    : lfe_server.erl
%% Author  : Duncan McGreggor
%% Purpose : A headless stateful LFE REPL server

%% Docs TBD
-module(lfe_server).

-behaviour(gen_server).

-export([start/0, start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([send/1]).

start() ->
    start_link().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Env = lfe_env:new(),
    State = lfe_shell:new_state("lfe", [], Env),
    {ok, State}.

handle_call({send, Lfe}, _From, State) ->
    Self = self(),
    try
        process_flag(trap_exit, true),
        lfe_shell:eval_form(Lfe, Self, State, {stdio, false}),
        receive
            {eval_value, _Caller, Value, State1} ->
            {reply, Value, State1}
        end
    catch
        Exception:Reason ->
            Reason
    end;
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% API

send(Lfe) ->
  gen_server:call(?MODULE, {send, Lfe}).

