%% Copyright (c) 2008-2024 Robert Virding
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

%%% The calls needed to start user/user_drv have changed from OTP
%%% 26. In the release after 26 the module user no longer exists and
%%% user_drv has a different interface. Note that this is sort of
%%% documented but these modules are not included in the standard
%%% Erlang documentation.

-module(lfe_init).

-export([start/0]).

-include("lfe.hrl").

-define(OK_STATUS, 0).
-define(ERROR_STATUS, 127).

%% Start LFE running a script or the shell depending on arguments.

start() ->
    OTPRelease = erlang:system_info(otp_release),
    %% Find out which repl/shell we want to use.
    Repl = case init:get_argument(repl) of
	       {ok, [[R|_]]} -> list_to_atom(R);
	       _Other -> lfe_repl
	   end,
    case collect_args(init:get_plain_arguments()) of
        {[],[]} ->                              %Run a repl/shell
	    if OTPRelease >= "26" ->
		    %% The new way 26 and later.
		    user_drv:start(#{initial_shell => {Repl,start,[]}});
	       true ->
		    %% The old way before 26.
		    user_drv:start(['tty_sl -c -e',{Repl,start,[]}])
	    end;
        {Evals,Script} ->
	    if OTPRelease >= "26" ->
		    %% The new way 26 and later)
		    user_drv:start(#{initial_shell => noshell});
	       true ->
		    %% The old way before 26.
		    user:start()
	    end,
            run_evals_script(Repl, Evals, Script)
    end.

collect_args([E,Eval|Args]) when E == "-lfe_eval" ; E == "-eval" ; E == "-e" ->
    {Evals,Script} = collect_args(Args),
    {[Eval] ++ Evals,Script};
collect_args([E]) when E == "-lfe_eval" ; E == "-eval" ; E == "-e" ->
    {[],[]};
collect_args(Args) -> {[],Args}.                %Remaining become script

%% run_evals_script(Repl, Evals, Script) -> Pid.
%%  Firat evaluate all the eval strings if any then the script if
%%  there is one. The state from the string is past into the
%%  script. We can handle no strings and no script.

run_evals_script(Repl, Evals, Script) ->
    S = fun () ->
		io:format(user, "~p\n", [{Evals,Script}]),
                St = Repl:run_strings(Evals),
                case Script of
                    [F|As] ->
                        Repl:run_script(F, As, St);
                    [] -> {[],St}
                end
        end,
    spawn_link(fun () -> run_script(Repl, S) end).

%% run_script(Repl, Script)
%%  Run a script and terminate the erlang process afterwards.

run_script(Repl, Script) ->
    try
        Script(),                               %Evaluate the script
        %% For some reason we need to wait a bit before stopping.
        timer:sleep(1),
        init:stop(?OK_STATUS)
    catch
        ?CATCH(Class, Error, Stack)
            Skip = fun (M, _F, _A) ->
			   (M =:= lfe_eval) or (M =:= Repl)
		   end,
            Format = fun (T, I) -> lfe_io:prettyprint1(T, 15, I, 80) end,
            Cs = lfe_error:format_exception(Class, Error, Stack, 
					    Skip, Format, 1),
            io:put_chars(Cs),
            halt(?ERROR_STATUS)
    end.
