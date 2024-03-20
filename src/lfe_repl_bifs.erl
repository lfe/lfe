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

%% File    : lfe_repl_bifs.erl
%% Author  : Robert Virding
%% Purpose : A simple Lisp Flavoured Erlang REPL built in functions.

%% These are the standard known built-in functions for the REPL. They
%% are modelled on the ones in the Erlang shell as these give a lot of
%% useful inteofmation about the system.

-module(lfe_repl_bifs).

-include("lfe.hrl").
-include("lfe_docs.hrl").

%% The shell functions which generally callable.
-export([c/1,c/2,cd/1,ec/1,ec/2,ep/1,ep/2,epp/1,epp/2,flush/0,help/0,
         h/1,h/2,h/3,i/0,i/1,i/3,l/1,ls/1,clear/0,m/0,m/1,memory/0,memory/1,
         nregs/0,pid/3,p/1,p/2,pp/1,pp/2,pwd/0,q/0,regs/0,uptime/0,exit/0]).

%% c(File [,Args]) -> {ok,Module} | error.
%%  Compile and load an LFE file.

c(File) -> c(File, []).

c(File, Opts0) ->
    Opts1 = [report,verbose|Opts0],             %Always report verbosely
    case lfe_comp:file(File, Opts1) of
        Ok when element(1, Ok) =:= ok ->        %Compilation successful
            Return = lists:member(return, Opts1),
            Binary = lists:member(binary, Opts1),
            OutDir = outdir(Opts1),
            load_files(Ok, Return, Binary, OutDir);
        Error -> Error
    end.

load_files(Ok, _, true, _) -> Ok;               %Binary output
load_files(Ok, Ret, _, Out) ->                  %Beam files created.
    Mods = element(2, Ok),
    lists:map(fun (M) -> load_file(M, Ret, Out) end, Mods).

load_file(Ok, _, Out) ->
    case element(2, Ok) of
        [] -> Ok;                               %No module file to load
        Mod ->                                  %We have a module name
            Bfile = filename:join(Out, atom_to_list(Mod)),
            code:purge(Mod),
            code:load_abs(Bfile, Mod)           %Undocumented
    end.

outdir([{outdir,Dir}|_]) -> Dir;                %Erlang way
outdir([[outdir,Dir]|_]) -> Dir;                %LFE way
outdir([_|Opts]) -> outdir(Opts);
outdir([]) -> ".".

%% cd(Dir) -> ok.

cd(Dir) -> c:cd(Dir).

%% ec(File [,Args]) -> Res.
%%  Compile and load an Erlang file.

ec(F) -> c:c(F).

ec(F, Os) -> c:c(F, Os).

%% ep(Expr [, Depth]) -> ok.
%% epp(Expr [, Depth]) -> ok.
%%  Print/prettyprint a value in Erlang format.

ep(E) ->
    Cs = io_lib:write(E),
    io:put_chars([Cs,$\n]).

ep(E, D) ->
    Cs = io_lib:write(E, D),
    io:put_chars([Cs,$\n]).

epp(E) ->
    Cs = io_lib:format("~p", [E]),
    io:put_chars([Cs,$\n]).

epp(E, D) ->
    Cs = io_lib:format("~P", [E,D]),
    io:put_chars([Cs,$\n]).

%% help() -> ok.

help() ->
    io:put_chars(<<"\nLFE shell built-in functions\n\n"
                   "(c file)       -- compile and load code in <file>\n"
                   "(cd dir)       -- change working directory to <dir>\n"
                   "(clear)        -- clear the REPL output\n"
                   "(doc mod)      -- documentation of a module\n"
                   "(doc mod:mac)  -- documentation of a macro\n"
                   "(doc m:f/a)    -- documentation of a function\n"
                   "(ec file)      -- compile and load code in erlang <file>\n"
                   "(ep expr)      -- print a term in erlang form\n"
                   "(epp expr)     -- pretty print a term in erlang form\n"
                   "(exit)         -- quit - an alias for (q)\n"
                   "(flush)        -- flush any messages sent to the shell\n"
                   "(h)            -- an alias for (help)\n"
                   "(h m)          -- help about module\n"
                   "(h m m)        -- help about function and macro in module\n"
                   "(h m f a)      -- help about function/arity in module\n"
                   "(help)         -- help info\n"
                   "(i)            -- information about the system\n"
                   "(i pids)       -- information about a list of pids\n"
                   "(i x y z)      -- information about pid #Pid<x.y.z>\n"
                   "(l module)     -- load or reload <module>\n"
                   "(ls)           -- list files in the current directory\n"
                   "(ls dir)       -- list files in directory <dir>\n"
                   "(m)            -- which modules are loaded\n"
                   "(m mod)        -- information about module <mod>\n"
                   "(memory)       -- memory allocation information\n"
                   "(memory t)     -- memory allocation information of type <t>\n"
                   "(p expr)       -- print a term\n"
                   "(pp expr)      -- pretty print a term\n"
                   "(pid x y z)    -- convert x, y, z to a pid\n"
                   "(pwd)          -- print working directory\n"
                   "(q)            -- quit - shorthand for init:stop/0\n"
                   "(regs)         -- information about registered processes\n"
                   "(nregs)        -- information about all registered processes\n"
                   "(uptime)       -- print node uptime\n"
                   "\n"
                   "LFE shell built-in forms\n\n"
                   "(reset-environment)             -- reset the environment to its initial state\n"
                   "(run file)                      -- execute all the shell commands in a <file>\n"
                   "(set pattern expr)\n"
                   "(set pattern (when guard) expr) -- evaluate <expr> and match the result with\n"
                   "                                   pattern binding\n"
                   "(slurp file)                    -- slurp in a LFE source <file> and makes\n"
                   "                                   everything available in the shell\n"
                   "(unslurp)                       -- revert back to the state before the last\n"
                   "                                   slurp\n\n"
                   "LFE shell built-in variables\n\n"
                   "+/++/+++      -- the three previous expressions\n"
                   "*/**/***      -- the values of the previous expressions\n"
                   "-             -- the current expression output\n"
                   "$ENV          -- the current LFE environment\n\n"
                 >>).

%% i([Pids]) -> ok.

i() -> c:i().

i(Pids) -> c:i(Pids).

i(X, Y, Z) -> c:i(X, Y, Z).

%% l(Modules) -> ok.
%%  Load the modules.

l(Ms) ->
    lists:foreach(fun (M) -> c:l(M) end, Ms).

%% ls(Dir) -> ok.

ls(Dir) -> apply(c, ls, Dir).

%% clear() -> ok.

clear() -> io:format("\e[H\e[J").

%% m([Modules]) -> ok.
%%  Print module information. Instead of using the c module we do
%%  module info ourselves to get all the data and the right formats.

m() ->
    mformat("Module", "File"),
    lists:foreach(fun ({Mod,File}) ->
                          Mstr = lists:flatten(lfe_io:print1(Mod)),
                          mformat(Mstr, File)
                  end,
                  lists:sort(code:all_loaded())).

mformat(S1, S2) ->
    Fstr = if length(S1) > 20 -> "~s~n                      ~s~n";
              true -> "~-20s  ~s~n"
           end,
    lfe_io:format(Fstr, [S1,S2]).

m(Ms) ->
    lists:foreach(fun (M) -> print_module(M) end, Ms).

print_module(M) ->
    Info = M:module_info(),
    lfe_io:format("Module: ~w~n", [M]),
    print_object_file(M),
    print_md5(Info),
    print_compile_time(Info),
    print_compile_options(Info),
    print_exports(Info),
    print_macros(Info).

print_md5(Info) ->
    case lists:keyfind(md5, 1, Info) of
        {md5,<<MD5:128>>} -> lfe_io:format("MD5: ~.16b~n", [MD5]);
        false -> ok
    end.

print_compile_time(Info) ->
    Cstr = case get_compile_info(Info, time) of
               {Year,Month,Day,Hour,Min,Sec} ->
                   lfe_io:format1("~w-~.2.0w-.2.0~w ~.2.0w:~.2.0w:~.2.0w",
                                  [Year,Month,Day,Hour,Min,Sec]);
               error -> "No compile time info avaliable"
           end,
    lfe_io:format("Compiled: ~s~n", [Cstr]).

print_object_file(M) ->
    case code:is_loaded(M) of
        {file,File} -> lfe_io:format("Object file: ~s~n", [File]);
        _ -> ok
    end.

print_compile_options(Info) ->
    case get_compile_info(Info, options) of     %Export Opts
               Opts when is_list(Opts) -> ok;
               error -> Opts = []
    end,
    lfe_io:format("Compiler options: ~p~n", [Opts]).

print_exports(Info) ->
    lfe_io:format("Exported functions:~n", []),
    Exps = case lists:keyfind(exports, 1, Info) of
               {exports,Es} -> Es;
               false -> []
           end,
    print_names(fun ({N,Ar}) -> lfe_io:format1("~w/~w", [N,Ar]) end, Exps).

print_macros(Info) ->
    lfe_io:format("Exported macros:~n", []),
    Macs = case lists:keyfind(attributes, 1, Info) of
               {attributes,Attrs} ->
                   Fun = fun ({'export-macro',Ms}) -> Ms;
                             (_) -> []
                         end,
                   lists:flatmap(Fun, Attrs);
               false -> []
           end,
    print_names(fun (N) -> lfe_io:print1(N) end, Macs).

print_names(Format, Names) ->
    %% Generate flattened list of strings.
    Strs = lists:map(fun (N) -> lists:flatten(Format(N)) end,
                     lists:sort(Names)),
    %% Split into equal length lists and print out.
    {S1,S2} = lists:split(round(length(Strs)/2), Strs),
    print_name_strings(S1, S2).

print_name_strings([N1|N1s], [N2|N2s]) ->
    lfe_io:format("  ~-30s  ~-30s~n", [N1,N2]),
    print_name_strings(N1s, N2s);
print_name_strings([N1], []) ->
    lfe_io:format("  ~s~n", [N1]);
print_name_strings([], []) -> ok.

get_compile_info(Info, Tag) ->
    case lists:keyfind(compile, 1, Info) of
        {compile,C} ->
            case lists:keyfind(Tag, 1, C) of
                {Tag,Val} -> Val;
                false -> error
            end;
        false -> error
    end.

%% p(Expr [, Depth]) -> ok.
%% pp(Expr [, Depth]) -> ok.
%%  Print/prettyprint a value in LFE format.

p(E) ->
    Cs = lfe_io:print1(E),
    io:put_chars([Cs,$\n]).

p(E, D) ->
    Cs = lfe_io:print1(E, D),
    io:put_chars([Cs,$\n]).

pp(E) ->
    Cs = lfe_io:prettyprint1(E),
    io:put_chars([Cs,$\n]).

pp(E, D) ->
    Cs = lfe_io:prettyprint1(E, D),
    io:put_chars([Cs,$\n]).

%% pid(A, B, C) -> Pid.
%%  Build a pid from its 3 "parts".

pid(A, B, C) -> c:pid(A, B, C).

%% pwd() -> ok.

pwd() -> c:pwd().

%% q() -> ok.

q() -> c:q().

%% flush() -> ok.

flush() -> c:flush().

%% regs() -> ok.

regs() -> c:regs().

%% nregs() -> ok.

nregs() -> c:nregs().

%% exit() -> ok.

exit() -> c:q().

%% memory() -> ok.

memory() -> c:memory().

%% memory(Type) -> ok.

memory(Type) -> c:memory(Type).

%% uptime() -> ok.

uptime() -> c:uptime().

%% doc(Mod) -> ok | {error,Error}.
%% doc(Mod, Func) -> ok | {error,Error}.
%% doc(Mod, Func, Arity) -> ok | {error,Error}.
%%  Print out documentation of a module/macro/function. Always try to
%%  find the file and use it as this is the only way to get hold of
%%  the chunks. This may get a later version than is loaded.

%% doc(?Q(What)) -> doc(What);                     %Be kind if they quote it
%% doc(What) ->
%%     [Mod|F] = lfe_lib:split_name(What),

h(Mod) ->
    case lfe_docs:get_module_docs(Mod) of
        {ok,#docs_v1{}=Docs} ->
            Ret = get_module_doc(Mod, Docs),
            format_doc(Ret);
        Error -> Error
    end.

h(Mod, Func) ->
    case lfe_docs:get_module_docs(Mod) of
        {ok,#docs_v1{}=Docs} ->
            Ret = get_macro_doc(Mod, Func, Docs),
            format_doc(Ret);
        Error -> Error
    end.

h(Mod, Func, Arity) ->
    case lfe_docs:get_module_docs(Mod) of
        {ok,#docs_v1{}=Docs} ->
            Ret = get_function_doc(Mod, Func, Arity, Docs),
            format_doc(Ret);
        Error -> Error
    end.

format_doc({error,_}=Error) -> Error;
format_doc(Docs) ->
    {match,Lines} = re:run(Docs, "(.+\n|\n)",
                           [unicode,global,{capture,all_but_first,binary}]),
    Pline = fun (Line) ->
                    io:put_chars(Line),
                    1                           %Output one line
           end,
    paged_output(Pline, Lines),
    ok.


-ifdef(EEP48).

get_module_doc(Mod, #docs_v1{format = ?NATIVE_FORMAT}=Docs) ->
    shell_docs:render(Mod, Docs);
get_module_doc(Mod, #docs_v1{format = ?LFE_FORMAT}=Docs) ->
    lfe_shell_docs:render(Mod, Docs);
get_module_doc(_Mod, #docs_v1{format = Enc}) ->
    {error, {unknown_format, Enc}}.

get_macro_doc(Mod, Name, #docs_v1{format = ?NATIVE_FORMAT}=Docs) ->
    shell_docs:render(Mod, Name, Docs);
get_macro_doc(Mod, Name, #docs_v1{format = ?LFE_FORMAT}=Docs) ->
    lfe_shell_docs:render(Mod, Name, Docs);
get_macro_doc(_Mod, _Name, #docs_v1{format = Enc}) ->
    {error, {unknown_format, Enc}}.

get_function_doc(Mod, Name, Arity, #docs_v1{format = ?NATIVE_FORMAT}=Docs) ->
    shell_docs:render(Mod, Name, Arity, Docs);
get_function_doc(Mod, Name, Arity, #docs_v1{format = ?LFE_FORMAT}=Docs) ->
    lfe_shell_docs:render(Mod, Name, Arity, Docs);
get_function_doc(_Mod, _Name, _Arity, #docs_v1{format = Enc}) ->
    {error, {unknown_format, Enc}}.

-else.

get_module_doc(Mod, #docs_v1{format = ?LFE_FORMAT}=Docs) ->
    lfe_shell_docs:render(Mod, Docs);
get_module_doc(_Mod, #docs_v1{format = Enc}) ->
    {error, {unknown_format, Enc}}.

get_macro_doc(Mod, Name, #docs_v1{format = ?LFE_FORMAT}=Docs) ->
    lfe_shell_docs:render(Mod, Name, Docs);
get_macro_doc(_Mod, _Name, #docs_v1{format = Enc}) ->
    {error, {unknown_format, Enc}}.

get_function_doc(Mod, Name, Arity, #docs_v1{format = ?LFE_FORMAT}=Docs) ->
    lfe_shell_docs:render(Mod, Name, Arity, Docs);
get_function_doc(_Mod, _Name, _Arity, #docs_v1{format = Enc}) ->
    {error, {unknown_format, Enc}}.

-endif.

%% paged_output(PrintItem, Items) -> ok.
%%  Output item lines a page at a time. This can handle an item
%%  returning multiple lines.

paged_output(Pitem, Items) ->
    %% How many rows per "page", just set it to 30 for now.
    Limit = 30,
    paged_output(Pitem, 0, Limit, Items).

paged_output(Pitem, Curr, Limit, Items) when Curr >= Limit ->
    case more() of
        more -> paged_output(Pitem, 0, Limit, Items);
        less -> ok
    end;
paged_output(Pitem, Curr, Limit, [Item|Items]) ->
    Olines = Pitem(Item),
    paged_output(Pitem, Curr + Olines, Limit, Items);
paged_output(_, _, _, []) -> ok.

more() ->
    case io:get_line('More (y/n)? ') of
        "y\n" -> more;
        "c\n" -> more;
        "n\n" -> less;
        "q\n" -> less;
        _ -> more()
    end.
