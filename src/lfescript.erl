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

%%% File    : lfescript.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang scripting interface.

-module(lfescript).

%% External API.
-export([script_name/0]).

%% Internal API.
-export([start/0,start/1]).
-export([run/1,run/2]).

-include("lfe.hrl").

%% External API.

script_name() ->
    [Sname|_] = init:get_plain_arguments(),
    Sname.

%% Internal API.

-define(OK_STATUS, 0).
-define(ERROR_STATUS, 127).

-spec start() -> no_return().
-spec start(_Options) -> no_return().
-spec run(_CmdLine) -> no_return().
-spec run(_CmdLine, _Options) -> no_return().
%%  Evaluate the LFE script. All errors which are caught here are
%%  internal errors. Start gets its arguments from the command line
%%  while run gets them as an argument.

start() -> start([]).

start(Lopts) ->
    run(init:get_plain_arguments(), Lopts).

run(CmdLine) -> run(CmdLine, []).

run([File|Args], Lopts) ->
    try
        process_flag(trap_exit, false),
        parse_check_run(File, Args, Lopts)
    catch
        %% Catch program errors.
        throw:Str ->
            lfe_io:format("lfescript: ~s\n", [Str]),
            halt(?ERROR_STATUS);
        ?CATCH(_, Reason, Stack)
            lfe_io:format("lfescript: Internal error: ~p\n", [Reason]),
            lfe_io:format("~p\n", [Stack]),
            halt(?ERROR_STATUS)
    end;
run([], _) ->
    lfe_io:format("lfescript: Missing filename\n", []),
    halt(?ERROR_STATUS).

%% parse_check_run(FileName, Args, Options) -> no_return().
%%  Parse the script file, check the code, build a function
%%  environment and call the main function. This function never checks
%%  return values, it just assumes that when a script error is
%%  detected/occurs the functions will call halt(?ERROR_STATUS).
%%  Non-handled errors are program errors.

parse_check_run(File, Args, Lopts) ->
    Fs0 = parse_file(File, Args, Lopts),
    {Fs1,Fenv0} = expand_macros(Fs0, File, Args, Lopts),
    Norms = normalise_code(Fs1, File, Args, Lopts),
    check_code(Norms, File, Args, Lopts),
    lists:member("s", Lopts) andalso halt(?OK_STATUS),
    Fenv1 = make_env(Norms, Fenv0, File, Args, Lopts),
    eval_code(Fenv1, File, Args, Lopts),
    halt(?OK_STATUS).                %Everything worked, just exit

error_exit(File, Es, Ws) ->
    list_errors(File, Es),
    list_warnings(File, Ws),
    halt(?ERROR_STATUS).

list_warnings(File, [{Line,Mod,Error}|Ws]) ->
    Cs = Mod:format_error(Error),
    lfe_io:format("~s:~w: Warning: ~s\n", [File,Line,Cs]),
    list_warnings(File, Ws);
list_warnings(_, []) -> ok.

list_errors(File, [{Line,Mod,Error}|Es]) ->
    Cs = Mod:format_error(Error),
    lfe_io:format("~s:~w: ~s\n", [File,Line,Cs]),
    list_errors(File, Es);
list_errors(_, []) -> ok.

%% parse_file(FileName, Args, Lopts) -> [{Expr,Line}].
%% parse_file(FileName) -> {ok,[{Expr,Line}]} | {error,Error}.
%%  This code is copied from lfe_io. We need to do that as we special
%%  case ignoring the first line.

parse_file(File, _, _) ->
    case parse_file(File) of
        {ok,Fs} -> Fs;
        {error,Error} ->
            error_exit(File, [Error], [])
    end.

parse_file(File) ->
    case file:open(File, [read]) of
        {ok,Fd} ->
            io:get_line(Fd, ''),                %Skip first line
            lfe_io:parse_file(Fd, 2);
        {error,Error} ->
            {error,{none,file,Error}}
    end.

%% expand_macros(Forms, File, Args, Lopts) -> {Forms,Fenv}.

expand_macros(Fs0, File, _, _) ->
    case lfe_macro:expand_fileforms(Fs0, lfe_env:new(), true, false) of
        {ok,Fs1,Fenv,Ws} ->
            list_warnings(File, Ws),
            {Fs1,Fenv};
        {error,Es,Ws} -> error_exit(File, Es, Ws)
    end.

%% normalise_code(Forms, File, Args, Lopts) -> Norms.

normalise_code(Forms, File, _Args, _Lopts) ->
    Module = [{['define-module',dummy,[],[[export,[main,1]]]],1} | Forms],
    case lfe_normalise:module(Module) of
        {ok,dummy,Norms,Ws} ->
            list_warnings(File, Ws),
            Norms;
        {error,Es,Ws} ->
            error_exit(File, Es, Ws)
    end.

%% check_code(Norms, File, Args, Lopts) -> ok.
%%  Call lfe_lint to check the code. Must create a dummy module to
%%  make lfe_lint happy.

check_code(Norms, File, _, _) ->
    case lfe_lint:module(Norms) of
        {ok,dummy,Ws} ->
            list_warnings(File, Ws);
        {error,Es,Ws} -> error_exit(File, Es, Ws)
    end.

%% make_env(Forms, File, Args, Lopts) -> FunctionEnv.

make_env(Forms, Fenv, _, _, _) ->
    F = fun ([function,_Line,Name,Def], Fs) ->
                Ar = function_arity(Def),
                Fs ++ [{Name,Ar,Def}];
            (_Other, Fs) ->
                Fs
        end,
    Fbs = lists:foldl(F, [], Forms),
    lfe_eval:make_letrec_env(Fbs, Fenv).

function_arity([lambda,As|_]) -> length(As);
function_arity(['match-lambda',[Pats|_]|_]) -> length(Pats).

%% eval_code(Fenv, File, Args, Lopts) -> Res.
%%  Evaluate the code. We must explicitly catch and handle errors in
%%  the script.

eval_code(Fenv, _, Args, _) ->
    try
        lfe_eval:expr([main,[quote,Args]], Fenv)
    catch
        %% Catch all exceptions in the code.
        ?CATCH(Class, Error, Stack)
            %% Report all errors.
            Skip = fun (_M, _F, _A) -> false end,
            Format = fun (T, I) -> lfe_io:prettyprint1(T, 15, I, 80) end,
            Cs = lfe_error:format_exception(Class, Error, Stack, 
                                            Skip, Format, 1),
            io:put_chars(Cs),
            halt(?ERROR_STATUS)
    end.
