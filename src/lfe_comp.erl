%% Copyright (c) 2008,2009 Robert Virding. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%%% File    : lfe_comp.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang compiler (to core Erlang).

-module(lfe_comp).

-export([file/1,file/2,forms/1,forms/2]).

%% -compile(export_all).

-import(lists, [member/2,keysearch/3,
		all/2,map/2,foldl/3,foldr/3,mapfoldl/3,mapfoldr/3,
		concat/1]).
-import(ordsets, [add_element/2,is_element/2,from_list/1,union/2]).
-import(orddict, [store/3,find/2]).

-import(lfe_lib, [new_env/0]).

-include_lib("compiler/src/core_parse.hrl").

-record(comp, {base="",				%Base name
	       lfile="",			%Lisp file
	       bfile="",			%Beam file
	       cfile="",			%Core file
	       opts=[],				%User options
	       mod=[],				%Module name
	       ret=file				%Return file|dcore|tcore|bin
	      }).

%% file(Name) ->
%%      {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns} | error.
%% file(Name, Options) ->
%%      {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns} | error.
%%  Compile the LFE file Name.

file(Name) -> file(Name, [verbose,report]).	%Default options

file(Name, Opts0) ->
    {Opts1,St0} = lfe_comp_opts(Opts0, #comp{}),
    St1 = filenames(Name, St0#comp{opts=Opts1}),
    case lfe_io:parse_file(St1#comp.lfile) of
	{ok,Fs} ->
	    %% Do the actual compilation work.
	    case do_forms(Fs, St1) of
		{ok,Core,Ws,St2} -> erl_comp(Core, Ws, St2);
		{error,Es,Ws,St2} -> do_error_return(Es, Ws, St2)
	    end;
	{error,Error} -> do_error_return([Error], [], St1)
    end.

%% forms(Forms) -> {ok,Mod,Bin,Warnings} | {error,Errors,Warnings}.
%% forms(Forms, Options) -> {ok,Mod,Bin,Warnings} | {error,Errors,Warnings}.
%%  Compile the LFE forms Forms, always return a binary.

forms(Forms) -> forms(Forms, [verbose,report]).	%Default options

forms(Fs0, Opts0) ->
    {Opts1,St0} = lfe_comp_opts(Opts0, #comp{}),
    St1 = filenames("-no-file-", St0#comp{opts=Opts1}),
    %% Tag forms with a "line number", just use their index.
    {Fs1,_} = mapfoldl(fun (F, N) -> {{F,N},N+1} end, 1, Fs0),
    case do_forms(Fs1, St1) of
	{ok,Core,Ws,St2} -> erl_comp(Core, Ws, St2);
	{error,Es,Ws,St2} -> do_error_return(Es, Ws, St2)
    end.

%% filenames(File, State) -> State.
%%  The default output dir is the current directory unless an
%%  explicit one has been given in the options.

filenames(File, St0) ->
    Dir = filename:dirname(File),
    Base = filename:basename(File, ".lfe"),
    Lfile = filename:join(Dir, Base ++ ".lfe"),
    Bfile = Base ++ ".beam",
    Cfile = Base ++ ".core",
    St1 = St0#comp{lfile=Lfile},
    %% Test for explicit out dir.
    case keysearch(outdir, 1, St1#comp.opts) of
	{value,{outdir,D}} ->
	    St1#comp{bfile=filename:join(D, Bfile),
		     cfile=filename:join(D, Cfile)};
	_ ->
	    St1#comp{bfile=Bfile,cfile=Cfile}
    end.

%% lfe_comp_opts(Opts, State) -> {Opts,State}.
%%  Check options for return type (dcore/tcore/binary), trim options
%%  and set options in state.

lfe_comp_opts(Opts, St) ->
    foldr(fun(dcore, {Os,S}) -> {Os,S#comp{ret=dcore}};
	     (to_core, {Os,S}) -> {Os,S#comp{ret=to_core}};
	     (binary, {Os,S}) -> {Os,S#comp{ret=binary}};
	     (file, {Os,S}) -> {Os,S#comp{ret=file}};
	     (Opt, {Os,S}) -> {[Opt|Os],S}
	  end, {[],St#comp{ret=file}}, Opts).

%% do_forms(Forms, State) ->
%%      {ok,Core,Warnings,State} | {error,Errors,Warnings,State}.
%%  Run the actual LFE compiler passes.

do_forms(Fs0, St) ->
    %% First macro expand forms.
    {Fs1,Env} = lfe_macro:expand_forms(Fs0, new_env()),
    debug_print("mac: ~p\n", [{Fs1,Env}], St),
    %% Lint and then compile if ok.
    case lfe_lint:module(Fs1, St#comp.opts) of
	{ok,Ws} ->
	    Fs2 = lfe_pmod:module(Fs1, St#comp.opts),
	    {Mod,Core1} = lfe_codegen:forms(Fs2, St#comp.opts),
	    {ok,Core1,Ws,St#comp{mod=Mod}};
	{error,Es,Ws} ->
	    {error,Es,Ws,St}
    end.

%% erl_comp(Core, Warnings, State) ->
%%      {ok,Mod[,Binary][,Warnings]}.
%%  Run the erlang compiler on the core module.

erl_comp(Core, Warns, St) ->
    Eopts = erl_comp_opts(St#comp.opts),	%Fix options for compiler
    %% Do work and fix returns accordingly.
    Ret = case St#comp.ret of
	      dcore ->
		  ok = file:write_file(St#comp.cfile, [core_pp:format(Core),$\n]),
		  {ok,[],[]};
	      to_core ->
		  case compile:forms(Core, [from_core,to_core|Eopts]) of
		      {ok,_,Cfr,Ews} ->
			  Cpp = [core_pp:format(Cfr),$\n],
			  ok = file:write_file(St#comp.cfile, Cpp),
			  {ok,[],Ews};
		      Error -> Error
		  end;
	      binary ->
		  case compile:forms(Core, [from_core,binary|Eopts]) of
		      {ok,_,Bin,Ews} -> {ok,[Bin],Ews};
		      Error -> Error
		  end;
	      file ->
		  case compile:forms(Core, [from_core,binary|Eopts]) of
		      {ok,_,Bin,Ews} ->
			  ok = file:write_file(St#comp.bfile, Bin),
			  {ok,[],Ews};
		      Error -> Error
		  end
	  end,
    case Ret of
	{ok,Stuff,Ews1} ->
	    do_ok_return(Stuff, Warns ++ fix_erl_errors(Ews1), St);
	{error,Ees,Ews1} ->
	    do_error_return(fix_erl_errors(Ees),
			    Warns ++ fix_erl_errors(Ews1), St)
    end.

%% fix_erl_errors([{File,Errors}]) -> Errors.

fix_erl_errors([{_,Es}|Fes]) -> Es ++ fix_erl_errors(Fes);
fix_erl_errors([]) -> [].

%% erl_comp_opts(Options) -> Options.
%%  Strip out report options and make sure erlang compiler returns
%%  errors and warnings.

erl_comp_opts([report|Os]) -> erl_comp_opts(Os);
erl_comp_opts([report_warnings|Os]) -> erl_comp_opts(Os);
erl_comp_opts([report_errors|Os]) -> erl_comp_opts(Os);
erl_comp_opts([O|Os]) -> [O|erl_comp_opts(Os)];
erl_comp_opts([]) -> [return].			%Ensure return!

%% do_ok_return(Ret, Warnings, State) -> {ok,Mod,...}.
%% do_error_return(Errors, Warnings, State) -> {error,...} | error.

do_ok_return(Ret0, Warns, St) ->
    Lfile = St#comp.lfile,
    Opts = St#comp.opts,
    when_opt(fun () -> list_warnings(Lfile, Warns) end, report, Opts),
    %% Fix right return.
    Ret1 = case member(return, Opts) of
	       true -> Ret0 ++ [return_errors(Lfile, Warns)];
	       false -> Ret0
	   end,
    list_to_tuple([ok,St#comp.mod|Ret1]).

do_error_return(Es, Ws, St) ->
    Lfile = St#comp.lfile,
    Opts = St#comp.opts,
    when_opt(fun () -> list_errors(Lfile, Es) end, report, Opts),
    when_opt(fun () -> list_warnings(Lfile, Ws) end, report, Opts),
    %% Fix right return.
    case member(return, Opts) of
	true -> {error,return_errors(Lfile, Es),return_errors(Lfile, Ws)};
	false -> error
    end.

return_errors(_, []) -> [];
return_errors(Lfile, Es) -> [{Lfile,Es}].

list_warnings(F, [{Line,Mod,Warn}|Ws]) ->
    io:fwrite("~s:~w: Warning: ~s\n", [F,Line,Mod:format_error(Warn)]),
    list_warnings(F, Ws);
list_warnings(F, [{Mod,Warn}|Ws]) ->
    io:fwrite("~s: Warning: ~s\n", [F,Mod:format_error(Warn)]),
    list_warnings(F, Ws);
list_warnings(_, []) -> [].

list_errors(F, [{Line,Mod,Error}|Ws]) ->
    io:fwrite("~s:~w: ~s\n", [F,Line,Mod:format_error(Error)]),
    list_errors(F, Ws);
list_errors(F, [{Mod,Error}|Ws]) ->
    io:fwrite("~s: ~s\n", [F,Mod:format_error(Error)]),
    list_errors(F, Ws);
list_errors(_, []) -> [].

debug_print(Format, Args, St) ->
    when_opt(fun () -> io:fwrite(Format, Args) end, debug_print, St#comp.opts).

%% when_opt(Fun, Option, Options) -> ok.
%% unless_opt(Fun, Option, Options) -> ok.
%%  Vall Fun when Option is/is not a member of Options.

when_opt(Fun, Opt, Opts) ->
    case member(Opt, Opts) of
	true -> Fun();
	false -> ok
    end.

%% unless_opt(Fun, Opt, Opts) ->
%%     case member(Opt, Opts) of
%% 	true -> ok;
%% 	false ->  Fun()
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
%%   (when-opt 'debug_print (comp-opts st) (: io fwrite f as)))
