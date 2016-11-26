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

%%% File    : lfe_comp.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang compiler (to core Erlang).

%% All the code in the file is treated as one sequence of forms until
%% after the macroexpansion pass when it is split into separate
%% modules. However up until after the lint pass all errors and
%% warnings are collected together in the errors and warnings
%% fields. After this the errors become more module specific and are
%% kept together with the compiled code, both for core and the
%% following erlang formats.

-module(lfe_comp).

-export([format_error/1]).
-export([file/1,file/2,forms/1,forms/2,default_options/0]).

%% -compile(export_all).

-import(lists, [member/2,keyfind/3,filter/2,foreach/2,all/2,any/2,
                map/2,flatmap/2,foldl/3,foldr/3,mapfoldl/3,mapfoldr/3]).

-include("lfe_comp.hrl").

%% The main compiler state.

-record(comp, {base="",                         %Base name
               ldir=".",                        %Lisp file dir
               lfile="",                        %Lisp file
               odir=".",                        %Output directory
               opts=[],                         %User options
               ipath=[],                        %Include path
               cinfo=none,                      %Common compiler info
               module=[],                       %Module name
               code=[],                         %Code after last pass.
               return=[],                       %What is returned [Val] | []
               errors=[],
               warnings=[]
              }).

%% default_options() -> Options.
%%  Return the default compiler options.

-define(DEFAULT_OPTS, [verbose,report]).

%% Errors.
format_error(write_file) -> "error writing file".

default_options() -> ?DEFAULT_OPTS.

%% file(Name) ->
%%      {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns} | error.
%% file(Name, Options) ->
%%      {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns} | error.
%%  Compile the LFE file Name.

file(Name) -> file(Name, default_options()).

file(Name, Opts) -> do_compile({file,Name}, Opts).

%% forms(Forms) -> {ok,Mod,Bin,Warnings} | {error,Errors,Warnings}.
%% forms(Forms, Options) -> {ok,Mod,Bin,Warnings} | {error,Errors,Warnings}.
%%  Compile the LFE forms Forms, always return a binary.

forms(Forms) -> forms(Forms, default_options()).

forms(Forms, Opts) -> do_compile({forms,Forms}, Opts).

do_compile(Input, Opts) ->
    Ifun = fun () ->
                   Ret = try
                             internal(Input, Opts)
                         catch
                             error:Reason ->
                                 St = erlang:get_stacktrace(),
                                 {error,{Reason,St}}
                         end,
                   exit(Ret)
           end,
    {Pid,Ref} = spawn_monitor(Ifun),
    receive
        {'DOWN',Ref,_,Pid,Res} -> Res
    end.

%% internal(Input, Options) -> Result.

internal({file,Name}, Opts) -> do_file(Name, Opts);
internal({forms,Forms}, Opts) -> do_forms(Forms, Opts).

do_file(Name, Opts0) ->
    Opts1 = lfe_comp_opts(Opts0),
    St0 = #comp{opts=Opts1,code=[]},            %Code must be list!
    St1 = filenames(Name, ".lfe", St0),
    St2 = include_path(St1),
    case lfe_io:parse_file(St2#comp.lfile) of
        {ok,Fs} ->
            %% Do the actual compilation work.
            do_forms(St2#comp{code=Fs});
        {error,Error} -> do_error_return(St2#comp{errors=[Error]})
    end.

do_forms(Fs0, Opts0) ->
    Source = proplists:get_value(source, Opts0, "-no-file-"),
    Opts1 = lfe_comp_opts(Opts0),
    St0 = #comp{opts=[binary|Opts1]},           %Implicit binary option
    St1 = filenames(Source, ".lfe", St0),
    St2 = include_path(St1),
    %% Tag forms with a "line number", just use their index.
    {Fs1,_} = mapfoldl(fun (F, N) -> {{F,N},N+1} end, 1, Fs0),
    do_forms(St2#comp{code=Fs1}).

%% filenames(File, Suffix, State) -> State.
%%  The default output dir is the current directory unless an
%%  explicit one has been given in the options.

filenames(File, Suffix, St) ->
    %% Test for explicit outdir.
    Odir = outdir(St#comp.opts),
    Ldir = filename:dirname(File),
    Base = filename:basename(File, Suffix),
    Lfile = filename:join(Ldir, Base ++ Suffix),
    St#comp{base=Base,
            ldir=Ldir,
            lfile=Lfile,
            odir=Odir
           }.

outdir([{outdir,Dir}|_]) -> Dir;                %Erlang way
outdir([[outdir,Dir]|_]) -> Dir;                %LFE way
outdir([_|Opts]) -> outdir(Opts);
outdir([]) -> ".".

%% include_path(State) -> State.
%%  Set the include path, we permit {i,Dir} and [i,Dir].

include_path(#comp{ldir=Ldir,opts=Opts}=St) ->
    Ifun = fun ({i,I}, Is) -> [I|Is];           %Erlang way
               ([i,I], Is) -> [I|Is];           %LFE way
               (_, Is) -> Is
           end,
    %% Same ordering as in the erlang compiler.
    Is = [".",Ldir|foldr(Ifun, [], Opts)],       %Default entries
    St#comp{ipath=Is}.

%% compiler_info(State) -> CompInfo.

compiler_info(#comp{lfile=F,opts=Os,ipath=Is}) ->
    #cinfo{file=F,opts=Os,ipath=Is}.

%% lfe_comp_opts(Opts) -> Opts.
%%  Translate from LFE to erlang standard options for lfe compiler.

lfe_comp_opts(Opts) ->
    Fun = fun ('to-split') -> to_split;
              ('to-expmac') -> to_expmac;
              ('to-expand') -> to_expand;
              ('to-exp') -> to_exp;             %Backwards compatibility
              ('to-pmod') -> to_pmod;
              ('to-lint') -> to_lint;
              ('no-docs') -> no_docs;
              ('to-core0') -> to_core0;
              ('to-core') -> to_core;
              ('to-kernel') -> to_kernel;
              ('to-asm') -> to_asm;
              ('no-export-macros') -> no_export_macros;
              ('warnings-as-errors') -> warnings_as_errors;
              ('report-warnings') -> report_warnings;
              ('report-errors') -> report_errors;
              ('debug-print') -> debug_print;
              (O) -> O
          end,
    map(Fun, Opts).

%% do_forms(State) ->
%%      {ok,Mod,[Core],[Warnings]} | {error,Errors,Warnings} | error.
%%  Run the actual LFE compiler passes.

do_forms(St0) ->
    %% Fill in the common compiler info.
    St1 = St0#comp{cinfo=compiler_info(St0)},
    Ps = passes(),
    case do_passes(Ps, St1) of
        {ok,St2} -> do_ok_return(St2);
        {error,St2} -> do_error_return(St2)
    end.

%% passes() -> [Pass].
%% do_passes(Passes, State) -> {ok,State} | {error,State}.
%%
%%  {when_flag,Flag,Cmd}    Do Cmd if Flag is or is not in the
%%  {unless_flag,Flag,Cmd}  option list.
%%
%%  {when_test,Test,Cmd}    Do Cmd if the Test function returns 'true'
%%  {unless_test,Test,Cmd}  or 'false'.
%%
%%  {do,Fun}                Call Fun and then continue.
%%
%%  {listing,PrintFun}      End compilation calling PrintFun to output
%%                          file.
%%
%%  done                    End compilation.
%%
%%  {done,PrintFun}         End compilation calling PrintFun to output
%%                          file, unless 'binary' is specified in which
%%                          current code will be returned.

passes() ->
    [
     %% Split input file into separate modules.
     {do,fun do_split_file/1},
     {when_flag,to_split,{done,fun split_pp/1}},
     %% Do per-module macro processing.
     {unless_flag,no_export_macros,{do,fun do_export_macros/1}},
     {when_flag,to_expmac,{done,fun expmac_pp/1}},
     %% Now we expand and trim remaining macros.
     {do,fun do_expand_macros/1},
     {when_flag,to_expand,{done,fun expand_pp/1}},
     {when_flag,to_exp,{done,fun expand_pp/1}}, %Backwards compatibility
     {do,fun do_lfe_pmod/1},
     {when_flag,to_pmod,{done,fun pmod_pp/1}},
     {do,fun do_lfe_lint/1},
     {when_flag,to_lint,{done,fun lint_pp/1}},
     {unless_flag,no_docs,{do,fun do_get_docs/1}},
     {do,fun do_lfe_codegen/1},
     {when_flag,to_core0,{done,fun core_pp/1}},
     {do,fun do_erl_comp/1},
     %% These options will have made erlang compiler return internal
     %% form after pass.
     {when_flag,to_core,{done,fun erl_core_pp/1}},
     {when_flag,to_kernel,{done,fun erl_kernel_pp/1}},
     {when_flag,to_asm,{done,fun erl_asm_pp/1}},
     %% Write docs beam chunks.
     {unless_flag,no_docs,{do,fun do_add_docs/1}},
     %% Now we just write the beam file unless warnings-as-errors is
     %% set and we have warnings.
     {when_test,fun is_werror/1,error},
     {done,fun beam_write/1}                    %Should be last
    ].

do_passes([{when_flag,Flag,Cmd}|Ps], #comp{opts=Opts}=St) ->
    do_passes(?IF(member(Flag, Opts), [Cmd|Ps], Ps), St);
do_passes([{unless_flag,Flag,Cmd}|Ps], #comp{opts=Opts}=St) ->
    do_passes(?IF(member(Flag, Opts), Ps, [Cmd|Ps]), St);
do_passes([{when_test,Test,Cmd}|Ps], St) ->
    do_passes(?IF(Test(St), [Cmd|Ps], Ps), St);
do_passes([{unless_test,Test,Cmd}|Ps], St) ->
    do_passes(?IF(Test(St), Ps, [Cmd|Ps]), St);
do_passes([{do,Fun}|Ps], St0) ->
    case Fun(St0) of
        {ok,St1} -> do_passes(Ps, St1);
        {error,St1} -> {error,St1}
    end;
do_passes([{listing,PrintFun}|_], St) ->
    PrintFun(St);
do_passes([done|_], St) -> {ok,St};             %Just end now
do_passes([error|_], St) -> {error,St};
do_passes([{done,Fun}|_], St) ->
    %% Print unless binary, in which case end.
    do_passes([{unless_flag,binary,{listing,Fun}}], St);
do_passes([], St) -> {ok,St}.                   %Got to the end, everything ok!

%% do_split_file(State) -> {ok,State} | {error,State}.
%%  Split a file into separate modules.  Everything defined before the
%%  first module is available in every module, after that things are
%%  local to the module in which they are defined. We need to expand
%%  top-level macros in forms so we can safelt detect the start of
%%  each module (with define-module form).

do_split_file(#comp{cinfo=Ci,code=Code}=St) ->
    case collect_pre_forms(Code, Ci) of         %Expand pre module forms
        {Pfs,Fs,Env0,Mst0} ->
            %% Expand the modules using the pre forms and environment.
            case collect_modules(Fs, Pfs, Env0, Mst0) of
                {ok,Ms,_Mst1} ->
                    {ok,St#comp{code=Ms,
                                warnings=St#comp.warnings}};
                {error,Es,Ws} ->
                    {error,St#comp{code=[],     %Pseudo module list.
                                   errors=St#comp.errors ++ Es,
                                   warnings=St#comp.warnings ++ Ws}}
            end;
        {error,Es,Ws} ->
            {error,St#comp{code=[],             %Pseudo module list.
                           errors=St#comp.errors ++ Es,
                           warnings=St#comp.warnings ++ Ws}}
    end.

%% collect_pre_forms(Forms, CompInfo) ->
%%     {PreForms,RestForms,Env,State}.

collect_pre_forms(Fs, Ci) ->
    Env = lfe_env:new(),
    %% Don't deep expand, keep everything.
    St = lfe_macro:expand_form_init(Ci, false, true),
    collect_mod_forms(Fs, Env, St).

%% collect_modules(Forms, PreForms, PreEnv, State) ->
%%     {Modules,State}.
%%  Collect and expand modules upto the end. Each module initially has
%%  the pre environment and all pre forms are appended to it.

collect_modules(Fs, PreFs, PreEnv, St) ->
    collect_modules(Fs, [], PreFs, PreEnv, St).

collect_modules([{['define-module',Name|_],_}=Mdef|Fs0], Ms, PreFs, PreEnv, St0) ->
    %% Expand and collect all forms upto next define-module or end.
    case collect_mod_forms(Fs0, PreEnv, St0) of
        {Mfs0,Fs1,_,St1} ->
            M = #module{name=Name,code=[Mdef] ++ PreFs ++ Mfs0},
            collect_modules(Fs1, [M|Ms], PreFs, PreEnv, St1);
        Error -> Error
    end;
collect_modules([], Ms, _PreFs, _PreEnv, St) ->
    {ok,lists:reverse(Ms),St}.

%% collect_mod_forms(Forms, Env, State) ->
%% collect_mod_forms(Forms, Acc, Env, State) ->
%%     {Modforms,RestForms,Env,State}.
%%  Expand and collect forms upto the next define-module or end. We
%%  also flatten top-level nested progn code.

collect_mod_forms(Fs, Env0, St0) ->
    case collect_mod_forms(Fs, [], Env0, St0) of
        {Acc,Rest,Env1,St1} ->
            {lists:reverse(Acc),Rest,Env1,St1};
        {error,_,_}=Error -> Error
    end.

collect_mod_forms([F0|Fs0], Acc, Env0, St0) ->
    case lfe_macro:expand_fileform(F0, Env0, St0) of
        {ok,{['define-module'|_],_}=F1,Env1,St1} ->
            {Acc,[F1|Fs0],Env1,St1};
        {ok,{['progn'|Pfs],L},Env1,St1} ->      %Flatten progn's
            Fs1 = [ {F,L} || F <- Pfs ] ++ Fs0,
            collect_mod_forms(Fs1, Acc, Env1, St1);
        {ok,F1,Env1,St1} ->
            collect_mod_forms(Fs0, [F1|Acc], Env1, St1);
        {error,Es,Ws,_} -> {error,Es,Ws}
    end;
collect_mod_forms([], Acc, Env, St) -> {Acc,[],Env,St}.

%% do_export_macros(State) -> {ok,State} | {error,State}.
%% do_expand_macros(State) -> {ok,State} | {error,State}.
%%  Process the macros in each module. Do_expand_macros is the last
%%  pass which fully expands all remaining macros and flattens the
%%  output.

do_export_macros(#comp{cinfo=Ci,code=Ms0}=St) ->
    Umac = fun (#module{code=Mfs0}=Mod) ->
                   {Mfs1,_} = lfe_macro_export:module(Mfs0, Ci),
                   Mod#module{code=Mfs1}
           end,
    Ms1 = lists:map(Umac, Ms0),
    {ok,St#comp{code=Ms1}}.

do_expand_macros(#comp{cinfo=Ci,code=Ms0}=St0) ->
    Emac = fun (#module{code=Fs0}=Mod) ->
                   Env = lfe_env:new(),
                   %% Deep expand, keep everything.
                   Mst = lfe_macro:expand_form_init(Ci, true, true),
                   case process_forms(fun expand_form/3, Fs0, {Env,Mst}) of
                       {Fs1,_} -> Mod#module{code=Fs1};
                       {error,_,_}=Error -> Error
                   end
           end,
    Ms1 = lists:map(Emac, Ms0),
    St1 = St0#comp{code=Ms1},
    ?IF(all_module(Ms1), {ok,St1}, {error,St1}).

expand_form(F0, L, {Env0,St0}) ->
    case lfe_macro:expand_form(F0, L, Env0, St0) of
        {ok,[progn|Pfs],Env1,St1} ->
            process_forms(fun expand_form/3, Pfs, L, {Env1,St1});
        %%{ok,['eval-when-compile'|_],Env1,St1} ->
        %%    {[],{Env1,St1}};
        {ok,F1,Env1,St1} ->
            {[{F1,L}],{Env1,St1}};
        {error,Es,Ws,_} -> throw({expand_form,{error,Es,Ws}})
    end.

%% process_forms(Fun, Forms, State) -> {Forms,State} | Error.
%% process_forms(Fun, Forms, Line, State) -> {Forms,State} | Error.
%%  Wrappers around lfe_lib:proc_forms which catch thrown errors.

process_forms(Fun, Fs, St) ->
    try lfe_lib:proc_forms(Fun, Fs, St)
    catch
        throw:{expand_form,Error} -> Error
    end.

process_forms(Fun, Fs, L, St) ->
    try lfe_lib:proc_forms(Fun, Fs, L, St)
    catch
        throw:{expand_form,Error} -> Error
    end.

%% do_lfe_pmod(State) -> {ok,State} | {error,State}.
%% do_lint(State) -> {ok,State} | {error,State}.
%% do_lfe_codegen(State) -> {ok,State} | {error,State}.
%% do_get_docs(State) -> {ok,State} | {error,State}.
%% do_erl_comp(State) -> {ok,State} | {error,State}.
%%  The actual compiler passes.

do_lfe_pmod(#comp{cinfo=Ci,code=Ms0}=St) ->
    Pmod = fun (#module{code=Mfs0}=Mod) ->
                   {Name,Mfs1} = lfe_pmod:module(Mfs0, Ci),
                   Mod#module{name=Name,code=Mfs1}
           end,
    Ms1 = lists:map(Pmod, Ms0),
    {ok,St#comp{code=Ms1}}.

do_lfe_lint(#comp{cinfo=Ci,code=Ms0}=St0) ->
    Lint = fun (#module{code=Mfs,warnings=Ws}=Mod) ->
                   case lfe_lint:module(Mfs, Ci) of
                       {ok,Name,Lws} -> Mod#module{name=Name,warnings=Ws++Lws};
                       {error,Les,Lws} -> {error,Les,Ws++Lws}
                   end
           end,
    %% Lint the modules, then check if all are ok.
    Ms1 = lists:map(Lint, Ms0),
    St1 = St0#comp{code=Ms1},
    ?IF(all_module(Ms1), {ok,St1}, {error,St1}).

do_get_docs(#comp{cinfo=Ci,code=Ms0}=St0) ->
    Doc = fun (#module{code=Mfs,warnings=Ws}=Mod) ->
                  case lfe_doc:extract_module_docs(Mfs, Ci) of
                      {ok,Docs} -> Mod#module{docs=Docs};
                      {error,Des,Dws} -> {error,Des,Ws ++ Dws}
                  end
          end,
    Ms1 = lists:map(Doc, Ms0),
    St1 = St0#comp{code=Ms1},
    ?IF(all_module(Ms1), {ok,St1}, {error,St1}).

do_lfe_codegen(#comp{cinfo=Ci,code=Ms0}=St) ->
    Code = fun (#module{name=Name,code=Mfs}=Mod) ->
                   %% Name consistency check!
                   {Name,Core} = lfe_codegen:module(Mfs, Ci),
                   Mod#module{code=Core}
           end,
    Ms1 = lists:map(Code, Ms0),
    {ok,St#comp{code=Ms1}}.

do_erl_comp(#comp{code=Ms0}=St0) ->
    ErlOpts = erl_comp_opts(St0),               %Options to erlang compiler
    %% Compile all the modules, then if all are ok.
    Ms1 = lists:map(fun (M) -> do_erl_comp_mod(M, ErlOpts) end, Ms0),
    St1 = St0#comp{code=Ms1},
    ?IF(all_module(Ms1), {ok,St1}, {error,St1}).

do_erl_comp_mod(#module{code=Core,warnings=Ws}=Mod, ErlOpts) ->
    %% lfe_io:format("~p\n", [Core]),
    case compile:forms(Core, ErlOpts) of
        {ok,_,Result,Ews} ->
            Mod#module{code=Result,warnings=Ws ++ fix_erl_errors(Ews)};
        {error,Ees,Ews} ->
            {error,fix_erl_errors(Ees),fix_erl_errors(Ews)}
    end.

all_module(Res) -> lists:all(fun (X) -> is_record(X, module) end, Res).

%% erl_comp_opts(State) -> Options.
%%  Strip out report options and make sure erlang compiler returns
%%  errors and warnings. Also remove other options which might cause
%%  strange behaviour.

erl_comp_opts(St) ->
    Os0 = St#comp.opts,
    Filter = fun (report) -> false;             %No reporting!
                 (report_warnings) -> false;
                 (report_errors) -> false;
                 ('S') -> false;                %No stopping early
                 ('E') -> false;
                 ('P') -> false;
                 (dcore) -> false;
                 (to_core0) -> false;
                 (warnings_as_errors) -> false; %We handle these ourselves
                 ({source,_}) -> false;
                 (_) -> true                    %Everything else
             end,
    Os1 = filter(Filter, Os0),
    %% Now build options for the erlang compiler. 'no_bopt' turns off
    %% an optimisation in the guard which crashes our code.
    [from_core,                                 %We are compiling from core
     {source,St#comp.lfile},                    %Set the source file
     return,                                    %Ensure we return something
     binary,                                    %We want a binary
     no_bopt|Os1].

%% split_pp(State) -> {ok,State} | {error,State}.
%% expmac_pp(State) -> {ok,State} | {error,State}.
%% expand_pp(State) -> {ok,State} | {error,State}.
%% pmod_pp(State) -> {ok,State} | {error,State}.
%% lint_pp(State) -> {ok,State} | {error,State}.
%% sexpr_pp(State) -> {ok,State} | {error,State}.
%% core_pp(State) -> {ok,State} | {error,State}.
%% erl_core_pp(State) -> {ok,State} | {error,State}.
%% erl_kernel_pp(State) -> {ok,State} | {error,State}.
%% erl_asm_pp(State) -> {ok,State} | {error,State}.
%% beam_write(State) -> {ok,State} | {error,State}.
%%  Output the various file types. The XXX_pp functions output with
%%  the same name as the input file while beam_write outputs to the
%%  module name.

%% This just print the whole file structure.
split_pp(St) -> sexpr_pp(St, "split").
expmac_pp(St) -> sexpr_pp(St, "expmac").
expand_pp(St) -> sexpr_pp(St, "expand").
pmod_pp(St) -> sexpr_pp(St, "pmod").
lint_pp(St) -> sexpr_pp(St, "lint").

sexpr_pp(St, Ext) ->
    Save = fun (File, #module{code=Code}) ->
                   lfe_io:prettyprint(File, Code), io:nl(File)
           end,
    do_list_save_file(Save, Ext, St).

%% These print a list of module structures.
core_pp(St) ->
    Save = fun (File, #module{code=Core}) ->
                   io:put_chars(File, [core_pp:format(Core),$\n])
           end,
    do_list_save_file(Save, "core", St).

erl_core_pp(St) ->
    Save = fun (File, #module{code=Core}) ->
                   io:put_chars(File, [core_pp:format(Core),$\n])
           end,
    do_list_save_file(Save, "core", St).

erl_kernel_pp(St) ->
    Save = fun (File, #module{code=Kern}) ->
                   io:put_chars(File, [v3_kernel_pp:format(Kern),$\n]) end,
    do_list_save_file(Save, "kernel", St).

erl_asm_pp(St) ->
    Save = fun (File, #module{code=Asm}) ->
                   beam_listing:module(File, Asm), io:nl(File) end,
    do_list_save_file(Save, "S", St).

do_list_save_file(SaveOne, Ext, St) ->
    SaveAll = fun (File, Code) ->
                      lists:foreach(fun (C) -> SaveOne(File, C) end, Code)
              end,
    do_save_file(SaveAll, Ext, St).

do_save_file(SaveAll, Ext, St) ->
    Name = filename:join(St#comp.odir, St#comp.base ++ ["."|Ext]),
    %% delayed_write useful here but plays havoc with erjang.
    case file:open(Name, [write]) of
        {ok,File} ->
            Ret = SaveAll(File, St#comp.code),
            ok = file:close(File),
            case Ret of
                ok -> {ok,St};
                {error,_} ->
		    %% Just signal we couldn't write the file.
		    {error,St#comp{errors=[{lfe_comp,write_file}]}}
            end;
        {error,_} ->
	    %% Just signal we couldn't write the file.
	    {error,St#comp{errors=[{lfe_comp,write_file}]}}
    end.

do_add_docs(#comp{cinfo=Ci,code=Ms0}=St0) ->
    Add = fun (#module{code=Beam0,docs=Docs}=Mod) ->
                  case lfe_doc:save_module_docs(Beam0, Docs, Ci) of
                      {ok,Beam1} -> Mod#module{code=Beam1};
                      {error,Es} -> {error,Es,[]}
                  end
          end,
    %%Add = fun (Mod) -> lfe_doc:save_module_docs(Mod, Ci) end,
    Ms1 = lists:map(Add, Ms0),
    St1 = St0#comp{code=Ms1},
    ?IF(all_module(Ms1), {ok,St1}, {error,St1}).

beam_write(St0) ->
    Ms1 = lists:map(fun (M) -> beam_write_module(M, St0) end, St0#comp.code),
    St1 = St0#comp{code=Ms1},
    %% Check return status.
    ?IF(all_module(Ms1), {ok,St1}, {error,St1}).

beam_write_module(#module{name=M,code=Beam}=Mod, St) ->
    Name = filename:join(St#comp.odir, lists:concat([M,".beam"])),
    case file:write_file(Name, Beam) of
        ok -> Mod;
        {error,_} ->
	    %% Just signal we couldn't write the file.
            {error,[{lfe_comp,write_file}],[]}
    end.

%% fix_erl_errors([{File,Errors}]) -> Errors.

fix_erl_errors(Fes) -> flatmap(fun ({_,Es}) -> Es end, Fes).

%% is_werror(State) -> true | false.
%%  Check if warnings_as_errors is set and we have warnings.

is_werror(#comp{code=Code,opts=Opts,warnings=Ws}) ->
    case member(warnings_as_errors, Opts) of
        true ->
            (Ws =/= []) orelse
                any(fun (#module{warnings=Mws}) -> Mws =/= [] end, Code);
        false -> false
    end.

%% do_ok_return(State) -> {ok,Mod,...}.
%% do_error_return(State) -> {error,...} | error.
%%  Note that this handling of 'warnings_as_errors' differs from the
%%  vanilla erlang compiler 'compile'. We explicitly check for it.

do_ok_return(#comp{code=Code,lfile=Lfile,opts=Opts,warnings=Ws}) ->
    ?WHEN_OPT(report, Opts, fun () -> list_warnings(Lfile, Ws) end),
    %% Fix right return.
    Report = member(report, Opts),
    Return = member(return, Opts),
    Binary = member(binary, Opts),
    RetMod = fun (M) -> ok_return_mod(M, Report, Return, Binary, Lfile) end,
    Ret0 = lists:map(RetMod, Code),
    Ret1 = if Return -> [Ret0,return_ews(Lfile, Ws)];
              true -> [Ret0]
           end,
    list_to_tuple([ok|Ret1]).                   %And build the ok tuple

ok_return_mod(#module{name=Name,code=Mods,warnings=Ws}, Report, Return, Binary, Lfile) ->
    Report andalso list_warnings(Lfile, Ws),
    Ret0 = if Return -> [return_ews(Lfile, Ws)];
              true -> []
           end,
    Ret1 = if Binary -> [Mods|Ret0];
              true -> Ret0
           end,
    list_to_tuple([ok,Name|Ret1]).              %And build the ok tuple

do_error_return(#comp{code=Code,lfile=Lfile,opts=Opts,errors=Es,warnings=Ws}) ->
    ?WHEN_OPT(report, Opts, fun () -> list_errors(Lfile, Es) end),
    ?WHEN_OPT(report, Opts, fun () -> list_warnings(Lfile, Ws) end),
    Report = lists:member(report, Opts),
    Return = lists:member(return, Opts),
    RetMod = fun (M) -> error_return_mod(M, Report, Return, Lfile) end,
    Err = lists:map(RetMod, Code),
    %% Fix right return.
    ?IF(Return, {error,Err,return_ews(Lfile, Es),return_ews(Lfile, Ws)}, error).

error_return_mod(#module{warnings=Ws}, Rep, _, Lfile) ->
    Rep andalso list_warnings(Lfile, Ws),
    {error,[],return_ews(Lfile, Ws)};           %No errors, only warnings
error_return_mod({error,Es,Ws}, Rep, _, Lfile) ->
    Rep andalso list_errors(Lfile, Es),
    Rep andalso list_warnings(Lfile, Ws),
    {error,return_ews(Lfile, Es),return_ews(Lfile, Ws)}.

return_ews(_, []) -> [];
return_ews(Lfile, Es) -> [{Lfile,Es}].

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
