%% Copyright (c) 2008-2013 Robert Virding
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

%%% File    : lfe_comp.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang compiler (to core Erlang).

-module(lfe_comp).

-export([file/1,file/2,forms/1,forms/2]).

%% -compile(export_all).

-import(lists, [member/2,keysearch/3,filter/2,foreach/2,
        all/2,map/2,flatmap/2,foldl/3,foldr/3,mapfoldl/3,mapfoldr/3]).
-import(ordsets, [add_element/2,is_element/2,from_list/1,union/2]).
-import(orddict, [store/3,find/2]).

-include_lib("compiler/src/core_parse.hrl").

-record(comp, {base="",             %Base name
               odir=".",            %Output directory
               lfile="",            %Lisp file
               bfile="",            %Beam file
               cfile="",            %Core file
               opts=[],             %User options
               mod=[],              %Module name
               ret=file,            %What is returned [Val] | []
               code=none,           %Code after last pass.
               errors=[],
               warnings=[]
          }).

%% file(Name) ->
%%      {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns} | error.
%% file(Name, Options) ->
%%      {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns} | error.
%%  Compile the LFE file Name.

-define(DEFAULT_OPTS, [verbose,report]).

file(Name) -> do_file(Name, ?DEFAULT_OPTS).

file(Name, Opts) -> do_file(Name, Opts).

do_file(Name, Opts0) ->
    Opts1 = lfe_comp_opts(Opts0),
    St0 = #comp{opts=Opts1},
    St1 = filenames(Name, St0),
    case lfe_io:parse_file(St1#comp.lfile) of
    {ok,Fs} ->
        %% Do the actual compilation work.
        do_forms(St1#comp{code=Fs});
    {error,Error} -> do_error_return(St1#comp{errors=[Error]})
    end.

%% forms(Forms) -> {ok,Mod,Bin,Warnings} | {error,Errors,Warnings}.
%% forms(Forms, Options) -> {ok,Mod,Bin,Warnings} | {error,Errors,Warnings}.
%%  Compile the LFE forms Forms, always return a binary.

forms(Forms) -> do_forms(Forms, ?DEFAULT_OPTS).

forms(Forms, Opts) -> do_forms(Forms, Opts).

do_forms(Fs0, Opts0) ->
    Opts1 = lfe_comp_opts(Opts0),
    St0 = #comp{opts=[binary|Opts1]},        %Implicit binary option
    St1 = filenames("-no-file-", St0#comp{opts=Opts1}),
    %% Tag forms with a "line number", just use their index.
    {Fs1,_} = mapfoldl(fun (F, N) -> {{F,N},N+1} end, 1, Fs0),
    do_forms(St1#comp{code=Fs1}).

%% filenames(File, State) -> State.
%%  The default output dir is the current directory unless an
%%  explicit one has been given in the options.

filenames(File, St) ->
    %% Test for explicit outdir.
    Odir = case keysearch(outdir, 1, St#comp.opts) of
           {value,{outdir,D}} -> D;
           false -> "."
       end,
    Dir = filename:dirname(File),
    Base = filename:basename(File, ".lfe"),
    Lfile = filename:join(Dir, Base ++ ".lfe"),
    Bfile = Base ++ ".beam",
    Cfile = Base ++ ".core",
    St#comp{base=Base,
        lfile=Lfile,
        odir=Odir,
        bfile=filename:join(Odir, Bfile),
        cfile=filename:join(Odir, Cfile)}.

%% lfe_comp_opts(Opts) -> Opts.
%%  Check options for lfe compiler.

lfe_comp_opts(Opts) ->
    filter(fun (_) -> true end, Opts).

%% do_forms(State) ->
%%      {ok,Mod,[Core],[Warnings]} | {error,Errors,Warnings} | error.
%%  Run the actual LFE compiler passes.

do_forms(St0) ->
    Ps = passes(),
    case do_passes(Ps, St0) of
    {ok,St1} -> do_ok_return(St1);
    {error,St1} -> do_error_return(St1)
    end.

%% do_macro_expand(State) -> {ok,State} | {error,State}.
%% do_lint(State) -> {ok,State} | {error,State}.
%% do_lfe_codegen(State) -> {ok,State} | {error,State}.
%% do_erl_comp(State) -> {ok,State} | {error,State}.
%%  The actual compiler passes.

do_macro_expand(St) ->
    case lfe_macro:expand_forms(St#comp.code, lfe_env:new()) of
    {ok,Fs,Env,Ws} ->
        debug_print("mac: ~p\n", [{Fs,Env}], St),
        {ok,St#comp{code=Fs,warnings=St#comp.warnings ++ Ws}};
    {error,Es,Ws} ->
        {error,St#comp{errors=St#comp.errors ++ Es,
               warnings=St#comp.warnings ++ Ws}}
    end.

do_lint(St) ->
    case lfe_lint:module(St#comp.code, St#comp.opts) of
    {ok,Ws} ->
        {ok,St#comp{warnings=St#comp.warnings ++ Ws}};
    {error,Es,Ws} ->
        {error,St#comp{errors=St#comp.errors ++ Es,
               warnings=St#comp.warnings ++ Ws}}
    end.

do_lfe_codegen(#comp{code=Fs0}=St) ->
    Opts = lfe_comp_opts(St#comp.opts),
    Fs1 = lfe_pmod:module(Fs0, Opts),
    {Mod,Core} = lfe_codegen:forms(Fs1, Opts),
    {ok,St#comp{code=Core,mod=Mod}}.

do_erl_comp(St) ->
    ErlOpts = erl_comp_opts(St),        %Options to erlang compiler
    Es = St#comp.errors,
    Ws = St#comp.warnings,
    case compile:forms(St#comp.code, ErlOpts) of
    {ok,_,Result,Ews} ->
        {ok,St#comp{code=Result,warnings=Ws ++ fix_erl_errors(Ews)}};
    {error,Ees,Ews} ->
        {error,St#comp{errors=Es ++ fix_erl_errors(Ees),
               warnings=Ws ++ fix_erl_errors(Ews)}}
    end.

%% erl_comp_opts(State) -> Options.
%%  Strip out report options and make sure erlang compiler returns
%%  errors and warnings. Also remove other options which might cause
%%  strange behaviour.

erl_comp_opts(St) ->
    Os0 = St#comp.opts,
    Filter = fun (report) -> false;         %No reporting!
         (report_warnings) -> false;
         (report_errors) -> false;
         ('S') -> false;                    %No stopping early
         ('E') -> false;
         ('P') -> false;
         (dcore) -> false;
         (to_core0) -> false;
         (warnings_as_errors) -> false;     %We handle this ourselves
         (_) -> true                        %Everything else
         end,
    Os1 = filter(Filter, Os0),
    %% Now build options for the erlang compiler. 'no_bopt' turns off
    %% an optimisation in the guard which crashes our code.
    [from_core,                         %We are compiling from core
     {source,St#comp.lfile},            %Set the source file
     return,                            %Ensure we return something
     binary,                            %We want a binary
     no_bopt|Os1].

%% passes() -> [Pass].
%% do_passes(Passes, State) -> {ok,State} | {error,State}.
%%  {when_flag,Flag,Cmd}
%%  {unless_flag,Flag,Cmd}
%%  {do,Fun}
%%  {pass,Fun}
%%  {done,PrintFun,Ext}

passes() ->
    [{do,fun do_macro_expand/1},
     {when_flag,to_exp,{done,fun sexpr_pp/2,"expand"}},
     {do,fun do_lint/1},
     {when_flag,to_lint,{done,fun sexpr_pp/2,"lint"}},
     {do,fun do_lfe_codegen/1},
     {when_flag,to_core0,{done,fun core_pp/2,"core"}},
     {do,fun do_erl_comp/1},
     %% These options will have made erl compiler return internal form
     %% after pass.
     {when_flag,to_core,{done,fun core_pp/2,"core"}},
     {when_flag,to_kernel,{done,fun kernel_pp/2,"kernel"}},
     {when_flag,to_asm,{done,fun asm_pp/2,"S"}},
     {unless_test,fun werror/1,{done,fun beam_write/2,"beam"}}]. %Should be last

do_passes([{do,Fun}|Ps], St0) ->
    case Fun(St0) of
    {ok,St1} -> do_passes(Ps, St1);
    {error,St1} -> {error,St1}
    end;
do_passes([{when_flag,Flag,Cmd}|Ps], St) ->
    case member(Flag, St#comp.opts) of
    true -> do_passes([Cmd|Ps], St);
    false -> do_passes(Ps, St)
    end;
do_passes([{unless_flag,Flag,Cmd}|Ps], St) ->
    case member(Flag, St#comp.opts) of
    true -> do_passes(Ps, St);
    false -> do_passes([Cmd|Ps], St)
    end;
do_passes([{when_test,Test,Cmd}|Ps], St) ->
    case Test(St) of
    true -> do_passes([Cmd|Ps], St);
    false -> do_passes(Ps, St)
    end;
do_passes([{unless_test,Test,Cmd}|Ps], St) ->
    case Test(St) of
    true -> do_passes(Ps, St);
    false -> do_passes([Cmd|Ps], St)
    end;
do_passes([{done,Fun,Ext}|_], St) ->
    %% Either return code as value or print out file.
    case member(binary, St#comp.opts) of
    true -> {ok,St#comp{ret=[St#comp.code]}};
    false -> do_save_file(Fun, Ext, St#comp{ret=[]})
    end;
do_passes([], St) -> {ok,St}.            %Got to the end, everything ok!

do_save_file(Fun, Ext, St) ->
    Name = filename:join(St#comp.odir, St#comp.base ++ ["."|Ext]),
    %% delayed_write useful here but plays havoc with erjang.
    case file:open(Name, [write]) of
    {ok,File} ->
        Fun(File, St#comp.code),
        ok = file:close(File),
        {ok,St};
    {error,E} -> {error,St#comp{errors=[{file,E}]}}
    end.

%% sexpr_pp(File, Sexprs) -> ok.
%% core_pp(File, Sexprs) -> ok.
%% kernel_pp(File, Sexprs) -> ok.
%% asm_pp(File, Sexprs) -> ok.
%% beam_write(File, Beamcode) -> ok.

sexpr_pp(File, Code) -> lfe_io:prettyprint(File, Code),io:nl(File).

core_pp(File, Core) -> io:put_chars(File, [core_pp:format(Core),$\n]).

kernel_pp(File, Kern) -> io:put_chars(File, [v3_kernel_pp:format(Kern),$\n]).

asm_pp(File, Asm) -> beam_listing:module(File, Asm).

beam_write(File, Beam) -> file:write(File, Beam).

%% fix_erl_errors([{File,Errors}]) -> Errors.

fix_erl_errors(Fes) -> flatmap(fun ({_,Es}) -> Es end, Fes).

werror(#comp{opts=Opts,warnings=Ws}) ->
    Ws =/= [] andalso member(warnings_as_errors, Opts).

%% do_ok_return(State) -> {ok,Mod,...}.
%% do_error_return(State) -> {error,...} | error.
%% Note that this handling of 'warnings_as_errors' is the same in the
%% vanilla erlang compiler 'compile'.

do_ok_return(#comp{lfile=Lfile,opts=Opts,ret=Ret0,warnings=Ws}=St) ->
    case werror(St) of
    true -> do_error_return(St);        %Warnings are errors!
    false ->
        when_opt(report, Opts, fun () -> list_warnings(Lfile, Ws) end),
        %% Fix right return.
        Ret1 = case member(return, Opts) of
               true -> Ret0 ++ [return_errors(Lfile, Ws)];
               false -> Ret0
           end,
        list_to_tuple([ok,St#comp.mod|Ret1])
    end.

do_error_return(#comp{lfile=Lfile,opts=Opts,errors=Es,warnings=Ws}) ->
    when_opt(report, Opts, fun () -> list_errors(Lfile, Es) end),
    when_opt(report, Opts, fun () -> list_warnings(Lfile, Ws) end),
    %% Fix right return.
    case member(return, Opts) of
    true -> {error,return_errors(Lfile, Es),return_errors(Lfile, Ws)};
    false -> error
    end.

return_errors(_, []) -> [];
return_errors(Lfile, Es) -> [{Lfile,Es}].

list_warnings(F, Ws) ->
    foreach(fun ({Line,Mod,Warn}) ->
            Cs = Mod:format_error(Warn),
            lfe_io:format("~s:~w: Warning: ~s\n", [F,Line,Cs]);
        ({Mod,Warn}) ->
            Cs = Mod:format_error(Warn),
            lfe_io:format("~s: Warning: ~s\n", [F,Cs])
        end, Ws).

list_errors(F, Es) ->
    foreach(fun ({Line,Mod,Error}) ->
            Cs = Mod:format_error(Error),
            lfe_io:format("~s:~w: ~s\n", [F,Line,Cs]);
        ({Mod,Error}) ->
            Cs = Mod:format_error(Error),
            lfe_io:format("~s: ~s\n", [F,Cs])
        end, Es).

debug_print(Format, Args, St) ->
    when_opt(debug_print, St#comp.opts,
        fun () -> lfe_io:format(Format, Args) end).

%% when_opt(Option, Options, Fun) -> ok.
%% unless_opt(Option, Options, Fun) -> ok.
%%  Vall Fun when Option is/is not a member of Options.

when_opt(Opt, Opts, Fun) ->
    case member(Opt, Opts) of
    true -> Fun();
    false -> ok
    end.

%% unless_opt(Opt, Opts, Fun) ->
%%     case member(Opt, Opts) of
%%     true -> ok;
%%     false ->  Fun()
%%     end.

%% Direct translations.
%% (defmacro when-opt (fun o os)
%%   `(if (member ,o ,os) (funcall ,fun) 'ok))
%% (defmacro unless-opt (fun o os)
%%   `(if (member ,o ,os) 'ok (funcall ,fun)))

%% Lispier versions.
%% (defmacro when-opt
%%   ((o os . body)
%%    `(if (member ,o ,os) (progn ,@body) 'ok)))
%% (defmacro unless-opt
%%   ((o os . body)
%%    `(if (member ,o ,os) 'ok (progn ,@body))))
%% (defmacro debug-print (f as st)
%%   (when-opt 'debug_print (comp-opts st) (: lfe_io format f as)))
