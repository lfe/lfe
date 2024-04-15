%% Copyright (c) 2024 Robert Virding
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

%% File    : lfe_error.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang error report functions.

-module(lfe_error).

%% Supported and documented exported functions in this module.
-export([format_exception/6,format_stacktrace/3]).

%% format_exception(Class, Error, Stacktrace, SkipFun, FormatFun, Indentation)
%%      -> DeepCharList.
%%  Format an exception. Class, Error and Stacktrace describe the
%%  exception; SkipFun is used to trim the end of stack; FormatFun is
%%  used to format terms; and Indentation is the current column.

format_exception(Cl, Error0, St0, Skip, Format, I) ->
    Cs = case Cl of                             %Class type as string
             throw -> "throw";
             exit -> "exit";
             error -> "error"
         end,
    {Error1,St1} = case is_stacktrace(St0) of
                       true -> {Error0,St0};
                       false -> {{Error0,St0},[]}
                   end,
    P = "exception " ++ Cs ++ ": ",             %Class description string
    [P,format_reason(Error1, length(P)+I-1),"\n",
     format_stacktrace(St1, Skip, Format)].

%% format_reason(Error, Indentation) -> DeepCharList.
%%  Format an error giving a little better information. Explicitly
%%  handle errors known from ERTS here, anything is assumed to come
%%  from lfe_eval.

%% The ERTS exit codes.
format_reason(badarg, _I) ->
    <<"bad argument">>;
format_reason({badarg,V}, I) ->
    format_value(V, <<"bad argument ">>, I);
format_reason(badarith, _I) ->
    <<"error in arithmetic expression">>;
format_reason({badarity,{Fun,As}}, _I)
  when is_function(Fun) ->
    %% Only the arity is displayed, not the arguments As.
    lfe_io:format1(<<"~s called with ~s">>,
                   [format_fun(Fun),argss(length(As))]);
format_reason({badfun,Term}, I) ->
    format_value(Term, <<"bad function ">>, I);
format_reason({badmatch,V}, I) ->
    format_value(V, <<"no match of value ">>, I);
format_reason({case_clause,V}, I) ->
    %% "there is no case clause with a true guard sequence and a
    %% pattern matching..."
    format_value(V, <<"no case clause matching ">>, I);
format_reason(function_clause, _I) ->
    <<"no function clause matching">>;
format_reason(if_clause, _I) ->
    <<"no if clause matching">>;
format_reason(noproc, _I) -> <<"no such process or port">>;
format_reason(notalive, _I) ->
    <<"the node cannot be part of a distributed system">>;
format_reason(system_limit, _I) ->
    <<"a system limit has been reached">>;
format_reason(timeout_value, _I) ->
    <<"bad receive timeout value">>;
format_reason({try_clause,V}, I) ->
    %% "there is no try clause with a true guard sequence and a
    %% pattern matching..."
    format_value(V, <<"no try clause matching ">>, I);
format_reason(undef, _I) ->
    <<"undefined function">>;
%% We now pass the buck to lfe_eval.
format_reason(Error, _) ->
    lfe_eval:format_error(Error).

argss(0) -> <<"no arguments">>;
argss(1) -> <<"one argument">>;
argss(2) -> <<"two arguments">>;
argss(N) -> lfe_io:format1(<<"~w arguments">>, [N]).

format_fun(Fun) when is_function(Fun) ->
    {module,M} = erlang:fun_info(Fun, module),
    %% {name,F} = erlang:fun_info(Fun, name),
    {arity,A} = erlang:fun_info(Fun, arity),
    case erlang:fun_info(Fun, type) of
        {type,local} when M =:= lfe_eval ->
            lfe_io:format1(<<"interpreted function with arity ~w">>, [A]);
        _ -> lfe_io:print1(Fun)
    end.

format_value(Val, ErrStr, I) ->
    Sz = I + iolist_size(ErrStr),
    lfe_io:format1(<<"~s~.*P">>, [ErrStr,Sz,Val,10]).

%% format_stacktrace(Stacktrace, SkipFun, FormatFun) -> DeepCharList.
%%  Format a stacktrace. SkipFun is used to trim the end of stack;
%%  FormatFun is used to format terms.

format_stacktrace(St0, Skip, Format) ->
    Drop = fun ({M,F,A,_}) -> Skip(M, F, A);
               ({M,F,A}) -> Skip(M, F, A);      %Pre R15
               (_Other) -> false                %To be safe
           end,
    St1 = lists:reverse(lists:dropwhile(Drop, lists:reverse(St0))),
    Print = fun (F) -> format_stackcall(F, Format) end,
    lists:map(Print, St1).

format_stackcall({M,F,A}, _) when is_integer(A) ->    %Pre R15
    lfe_io:format1("  in ~w:~w/~w\n", [M,F,A]);
format_stackcall({M,F,A}, Format) ->
    ["  in ",Format([M,':',F|A], 5),"\n"];
format_stackcall({M,F,A,Loc},_) when is_integer(A) -> %R15 and later.
    lfe_io:format1("  in ~w:~w/~w ~s\n", [M,F,A,location(Loc)]);
format_stackcall({M,F,A,_}, Format) ->
    ["  in ",Format([M,':',F|A], 5),"\n"].

location(Loc) ->
    File = proplists:get_value(file, Loc),
    Line = proplists:get_value(line, Loc),
    if File =/= undefined, Line =/= undefined ->
            lfe_io:format1("(~s, line ~w)", [File,Line]);
       true -> ""
    end.

is_stacktrace([{M,F,A}|Fs])                     %Pre R15
  when is_atom(M), is_atom(F), is_integer(A) -> is_stacktrace(Fs);
is_stacktrace([{M,F,As}|Fs])
  when is_atom(M), is_atom(F), length(As) >= 0 -> is_stacktrace(Fs);
is_stacktrace([{M,F,A,I}|Fs])                   %R15 and later
  when is_atom(M), is_atom(F), is_integer(A), is_list(I) -> is_stacktrace(Fs);
is_stacktrace([{M,F,As,I}|Fs])
  when is_atom(M), is_atom(F), length(As) >= 0, is_list(I) -> is_stacktrace(Fs);
is_stacktrace([]) -> true;
is_stacktrace(_) -> false.
