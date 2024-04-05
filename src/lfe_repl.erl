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

%% File    : lfe_repl.erl
%% Author  : Robert Virding
%% Purpose : A simple Lisp Flavoured Erlang REPL.

%% We keep three environments: the current environment; the saved
%% environment which contains the environment from before a slurp; and
%% the base environment which contains the predefined shell variables,
%% functions and macros. The base environment is used when we need to
%% revert to an "empty" environment. The save environment is used to
%% store the current environment when we 'slurp' a file which we can
%% revert back to when we do an 'unslurp'.

-module(lfe_repl).

-export([start/0,start/1,server/0,server/1,
         run_script/2,run_script/3,
         run_strings/1,run_strings/2,run_string/1,run_string/2]).

%% Useful for the LFE rebar3 plugin
-export([banner/0,banner/1,banner/2]).

-import(orddict, [store/3,find/2]).
-import(lists, [reverse/1,foreach/2]).

-include("lfe.hrl").
-include("lfe_docs.hrl").

%% Coloured strings for the LFE banner, red, green, yellow and blue.
-define(RED(Str), "\e[31m" ++ Str ++ "\e[0m").
-define(GRN(Str), "\e[1;32m" ++ Str ++ "\e[0m").
-define(YLW(Str), "\e[1;33m" ++ Str ++ "\e[0m").
-define(BLU(Str), "\e[1;34m" ++ Str ++ "\e[0m").
-define(BOLD(Str), "\e[1m" ++ Str ++ "\e[0m").

%% -compile([export_all]).

%% run_script(File, Args) -> {Value,State}.
%% run_script(File, Args, State) -> {Value,State}.

-spec run_script(_, _) -> no_return().
-spec run_script(_, _, _) -> no_return().

run_script(File, Args) ->
    run_script(File, Args, lfe_repl_eval:new_state(File, Args)).

run_script(File, Args, St0) ->
    St1 = lfe_repl_eval:upd_state(File, Args, St0),
    case lfe_repl_eval:read_script_file(File) of
        {ok, Forms} ->
            lfe_repl_eval:run_loop(Forms, St1);
        {error, E} ->
            lfe_repl_eval:slurp_errors("lfe", [E]),
            {error, St1}
    end.

%% run_strings(Strings) -> {Value,State}.
%% run_strings(Strings, State) -> {Value,State}.

run_strings(Strings) ->
    run_strings(Strings, lfe_repl_eval:new_state("lfe", [])).

run_strings(Strings, St) ->
    lists:foldl(fun (S, St0) ->
                        {_,St1} = run_string(S, St0),
                        St1
                end, St, Strings).

%% run_string(String) -> {Value,State}.
%% run_string(String, State) -> {Value,State}.

run_string(String) ->
    run_string(String, lfe_repl_eval:new_state("lfe", [])).

run_string(String, St0) ->
    St1 = lfe_repl_eval:upd_state(String, [], St0),
    case lfe_repl_eval:read_script_string(String) of
        {ok, Forms} ->
            lfe_repl_eval:run_loop(Forms, St1);
        {error, E} ->
            lfe_repl_eval:slurp_errors("lfe", [E]),
            {error, St1}
    end.

%% start() -> Pid.
%% start(State) -> Pid.

start() ->
    spawn(fun () -> server() end).

start(St) ->
    spawn(fun () -> server(St) end).

%% server() -> no_return().
%% server(State) -> no_return().

server() ->
    %% Create a default base env of predefined shell variables with
    %% default nil bindings and basic shell macros.
    State = lfe_repl_eval:new_state("lfe", []),
    server(State).

server(State) ->
    process_flag(trap_exit, true),              %Must trap exists
    display_banner(),
    %% Set shell io to use LFE expand in edlin, ignore error.
    io:setopts([{expand_fun,fun (B) -> lfe_edlin_expand:expand(B) end}]),
    Eval = start_eval(State),                   %Start an evaluator
    server_loop(Eval, State).                      %Run the loop

server_loop(Eval0, St0) ->
    %% Read the form
    Prompt = prompt(),
    {Ret,Eval1} = read_expression(Prompt, Eval0, St0),
    case Ret of
        {ok,Form} ->
            {Eval2,St1} = repl_eval(Form, Eval1, St0),
            server_loop(Eval2, St1);
        {error,E} ->
            lfe_repl_eval:list_errors([E]),
            server_loop(Eval1, St0)
    end.

%% start_eval(State) -> Evaluator.
%%  Start an evaluator process.

start_eval(State) ->
    {ok,Eval} = lfe_repl_eval:start_link(State),
    Eval.

%% repl_eval(From, Evaluator, State) -> {Evaluator,State}.
%%  Evaluate one repl expression. The evaluator may crash in which
%%  case we restart it and return the pid of the new evaluator.

repl_eval(Form, Eval0, St0) ->
    Reply = lfe_repl_eval:eval_form(Eval0, Form),
    case Reply of
        {ok,Value,St1} ->
            %% Print the result, but only to depth 30.
            VS = lfe_io:prettyprint1(Value, 30),
            io:requests([{put_chars,unicode,VS},nl]),
            {Eval0,St1};
        {Class,Reason,StackTrace} ->
            report_exception(Class,Reason,StackTrace),
            Eval1 = start_eval(St0),
            {Eval1,St0}
    end.

    %% Eval0 ! {eval_expr,self(),Form},
    %% receive
    %%     {eval_value,Eval0,_Value,St1} ->
    %%         %% We actually don't use the returned value here.
    %%         {Eval0,St1};
    %%     {eval_error,Eval0,Class} ->
    %%         %% Eval errored out, get the exit signal.
    %%         receive {'EXIT',Eval0,{Reason,Stk}} -> ok end,
    %%         report_exception(Class, Reason, Stk),
    %%         Eval1 = start_eval(St0),
    %%         {Eval1,St0};
    %%     {'EXIT',Eval0,Error} ->
    %%         %% Eval exited or was killed
    %%         report_exception(error, Error, []),
    %%         Eval1 = start_eval(St0),
    %%         {Eval1,St0}
    %% end.

%% prompt() -> Prompt.

prompt() ->
    %% Don't bother flattening the list, no need.
    case is_alive() of
        true -> lfe_io:format1("~s~s~s", node_prompt());
        false ->
            %% If a user supplied the ~node formatting option but the
            %% node is not actually alive, let's get rid of it
            P1 = user_prompt(),
            P2 = re:replace(P1, "~node", "", [{return, list}]),
            lfe_io:format1("~s", [P2])
    end.

node_prompt () ->
    Prompt = user_prompt(),
    Node = atom_to_list(node()),
    case re:run(Prompt, "~node") of
        nomatch -> ["(", Node, [")", Prompt]];
        _ -> ["", re:replace(Prompt, "~node", Node, [{return, list}]), ""]
    end.

user_prompt () ->
    %% Allow users to set a prompt with the -prompt flag; note that
    %% without the flag the default is "lfe> " and to obtain the
    %% old-style LFE prompt, use -prompt classic.
    case init:get_argument(prompt) of
        {ok, [[]]} -> [""];
        {ok, [["classic"]]} -> ["> "];
        {ok, [P]} -> P;
        _ -> ["lfe> "]
    end.

report_exception(Class, Reason, Stk) ->
    %% Use LFE's simplified version of erlang shell's error
    %% reporting but which LFE prettyprints data.
    Skip = fun (M, _F, _A) -> (M =:= lfe_eval) or (M =:= lfe_repl_eval) end,
    Format = fun (T, I) -> lfe_io:prettyprint1(T, 15, I, 80) end,
    Cs = lfe_error:format_exception(Class, Reason, Stk, Skip, Format, 1),
    io:put_chars("** " ++ Cs),                  %Make it more note worthy
    io:nl().

%% read_expression(Prompt, Evaluator, State) -> {Return,Evaluator}.
%%  Start a reader process and wait for an expression. We are cunning
%%  here and just use the exit reason to pass the expression. We must
%%  also handle the evaluator dying and restart it.

read_expression(Prompt, Eval, St) ->
    Read = fun () ->
                   %% io:put_chars(Prompt),
                   %% Ret = lfe_io:read_line(),
                   Ret = lfe_io:read_line(Prompt),
                   exit(Ret)
           end,
    Rdr = spawn_link(Read),
    read_expression_1(Rdr, Eval, St).

read_expression_1(Rdr, Eval, St) ->
    %% Eval is not doing anything here, so exits from it can only
    %% occur if it was terminated by a signal from another process.
    receive
        {'EXIT',Rdr,Ret} ->
            {Ret,Eval};
        {'EXIT',Eval,{Reason,Stk}} ->
            report_exception(error, Reason, Stk),
            read_expression_1(Rdr, start_eval(St), St);
        {'EXIT',Eval,Reason} ->
            report_exception(exit, Reason, []),
            read_expression_1(Rdr, start_eval(St), St)
    end.

banner() ->
    banner(lfe:version()).

banner(Vsn) ->
    banner(Vsn, quit_message()).

banner(Vsn, QuitMsg) ->
    ?GRN("   ..-~") ++ ?YLW(".~_") ++ ?GRN("~---..") ++ "\n" ++
       ?GRN("  (      ") ++ ?YLW("\\\\") ++ ?GRN("     )") ++ "    |   A Lisp-2+ on the Erlang VM\n" ++
       ?GRN("  |`-.._") ++ ?YLW("/") ++ ?GRN("_") ++ ?YLW("\\\\") ++ ?GRN("_.-':") ++ "    |   Type " ++ ?GRN("(help)") ++ " for usage info.\n" ++
       ?GRN("  |         ") ++ ?RED("g") ++ ?GRN(" |_ \\") ++  "   |\n" ++
       ?GRN("  |        ") ++ ?RED("n") ++ ?GRN("    | |") ++   "  |   Docs: " ++ ?BLU("http://docs.lfe.io/") ++ "\n" ++
       ?GRN("  |       ") ++ ?RED("a") ++ ?GRN("    / /") ++   "   |   Source: " ++ ?BLU("http://github.com/lfe/lfe") ++ "\n" ++
       ?GRN("   \\     ") ++ ?RED("l") ++ ?GRN("    |_/") ++  "    |\n" ++
       ?GRN("    \\   ") ++ ?RED("r") ++ ?GRN("     /") ++  "      |   LFE v" ++
        Vsn ++ " " ++  QuitMsg ++ "\n" ++
       ?GRN("     `-") ++ ?RED("E") ++ ?GRN("___.-'") ++ "\n\n".

display_banner() ->
    %% When LFE is called with -noshell, we want to skip the banner. Also, there may be
    %% circumstances where the shell is desired, but the banner needs to be disabled,
    %% thus we want to support both use cases.
    case init:get_argument(noshell) of
        error -> case init:get_argument(nobanner) of
                    error -> io:put_chars(banner());
                    _ -> false
                 end;
        _ -> false
    end.

quit_message() ->
    %% We can update this later to check for env variable settings for
    %% shells that require a different control character to abort, such
    %% as jlfe.
    "(abort with ^G)".
