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

%%% File    : lfescript.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang scripting interface.

-module(lfescript).

%% External API.
-export([script_name/0]).

%% Internal API.
-export([start/0,start/1]).
-export([run/1,run/2]).

script_name() ->
    [Sname|_] = init:get_plain_arguments(),
    Sname.

%% Internal API.

-define(OK_STATUS, 0).
-define(ERROR_STATUS, 127).

%% start() -> no_return().
%% start(Options) -> no_return().
%% run(CmdLine) -> no_return().
%% run(CmdLine, Options) -> no_return().
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
        _:Reason ->
            Stack = erlang:get_stacktrace(),    %Need to get this first
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
    check_code(Fs1, File, Args, Lopts),
    lists:member("s", Lopts) andalso halt(?OK_STATUS),
    Fenv1 = make_env(Fs1, Fenv0, File, Args, Lopts),
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
        {ok,F} ->
            io:get_line(F, ''),                 %Skip first line
            case io:request(F, {get_until,unicode,'',lfe_scan,tokens,[2]}) of
                {ok,Ts,_} ->
                    Ret = parse_file1(Ts, [], []),
                    file:close(F),
                    Ret;
                {error,Error,_} -> {error,Error}
            end;
        {error,Error} -> {error,{none,file,Error}}
    end.

parse_file1([_|_]=Ts0, Pc0, Ss) ->
    case lfe_parse:sexpr(Pc0, Ts0) of
        {ok,L,S,Ts1} -> parse_file1(Ts1, [], [{S,L}|Ss]);
        {more,Pc1} ->
            %% Need more tokens but there are none, so call again to
            %% generate an error message.
            {error,E,_} = lfe_parse:sexpr(Pc1, {eof,99999}),
            {error,E};
        {error,E,_} -> {error,E}
    end;
parse_file1([], _, Ss) -> {ok,lists:reverse(Ss)}.

%% expand_macros(Forms, File, Args, Lopts) -> {Forms,Fenv}.

expand_macros(Fs0, File, _, _) ->
    case lfe_macro:expand_forms(Fs0, lfe_env:new(), true, false) of
        {ok,Fs1,Fenv,Ws} ->
            list_warnings(File, Ws),
            {Fs1,Fenv};
        {error,Es,Ws} -> error_exit(File, Es, Ws)
    end.

%% check_code(Forms, File, Args, Lopts) -> ok.
%%  Call lfe_lint to check the code. Must create a dummy module to
%%  make lfe_lint happy.

check_code(Fs, File, _, _) ->
    Module = [{['define-module',dummy,[],[[export,[main,1]]]],1}|Fs],
    case lfe_lint:module(Module) of
        {ok,dummy,Ws} ->
            list_warnings(File, Ws);
        {error,Es,Ws} -> error_exit(File, Es, Ws)
    end.

%% make_env(Forms, File, Args, Lopts) -> FunctionEnv.

make_env(Fs, Fenv, _, _, _) ->
    {Fbs,null} = lfe_lib:proc_forms(fun collect_form/3, Fs, null),
    lfe_eval:make_letrec_env(Fbs, Fenv).

collect_form(['define-function',F,_Meta,Def], _, St) ->
    Ar = function_arity(Def),
    {[{F,Ar,Def}],St}.

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
        Class:Error ->
            St = erlang:get_stacktrace(),       %Need to get this first
            Skip = fun (_) -> false end,
            Format = fun (T, I) -> lfe_io:prettyprint1(T, 15, I, 80) end,
            Cs = lfe_lib:format_exception(Class, Error, St, Skip, Format, 1),
            io:put_chars(Cs),
            halt(?ERROR_STATUS)
    end.
