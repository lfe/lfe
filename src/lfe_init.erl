%% Copyright (c) 2008-2014 Robert Virding
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

%%% File    : lfe_init.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang init module.

%%% This little beauty allows you to start Erlang with the LFE shell
%%% running and still has ^G and user_drv enabled. Use it as follows:
%%%
%%% erl -user lfe_init
%%%
%%% Add -pa to find modules if necessary.
%%%
%%% Thanks to Attila Babo for showing me how to do this.

-module(lfe_init).

-export([start/0]).

%% Start LFE running a script or the shell depending on arguments.

start() ->
    case init:get_plain_arguments() of
        ["-lfe_eval"|As] ->                     %Run a command string
            user:start(),                       %Start user for io
            run_string(As);
        [S|As] ->                               %Run a script
            user:start(),                       %Start user for io
            spawn_link(fun () ->
                               lfe_shell:run_script(S, As),
                               init:stop(0)
                       end);
        [] ->                                   %Run a shell
            user_drv:start(['tty_sl -c -e',{lfe_shell,start,[]}])
    end.

run_string([]) -> run_string([], []);           %No command
run_string(["--"]) -> run_string([], []);       %No command
run_string([S,"--"|As]) -> run_string(S, As);
run_string([S|As]) -> run_string(S, As).

run_string([], _) ->                            %No command
    io:put_chars(user, "eval: missing command\n"),
    halt(1);
run_string(S, []) ->
    run_string(S, ["lfe"]);
run_string(S, As) ->
    spawn_link(fun () ->
                       lfe_shell:run_string(S, As),
                       init:stop(0)
               end).
