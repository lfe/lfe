%% Copyright (c) 2008 Robert Virding. All rights reserved.
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

-import(lfe_lib, [new_env/0,add_env/2,
		  add_vbinding/3,vbinding/2,fbinding/3,add_fbinding/4,
		  add_ibinding/5,gbinding/3]).

-include_lib("compiler/src/core_parse.hrl").

-record(comp, {base="",				%Base name
	       lfile="",			%Lisp file
	       bfile="",			%Beam file
	       cfile="",			%Core file
	       opts=[],				%User options
	       mod=[]				%Module name
	      }).

%% file(Name) -> {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns}.
%% file(Name, Options) ->
%%      {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns}.

file(Name) -> file(Name, [report]).		%Default options

file(Name, Opts) ->
    St0 = #comp{opts=Opts},
    St1 = filenames(Name, Opts, St0),
    case lfe_io:parse_file(St1#comp.lfile) of
	{ok,Fs} ->
	    %% Do the actual compilation work.
	    case forms(Fs, St1, Opts) of
		{ok,Core,Ws,St2} -> erl_comp(Core, Ws, Opts, St2);
		{error,Es,Ws,St2} -> do_error_return(Es, Ws, Opts, St2)
	    end;
	{error,Error} -> do_error_return([Error], [], Opts, St1)
    end.

%% filenames(File, Options, State) -> State.
%%  The default output dir is the current directory unless an
%%  explicit one has been given in the options.

filenames(File, Opts, St0) ->
    Dir = filename:dirname(File),
    Base = filename:basename(File, ".lfe"),
    Lfile = filename:join(Dir, Base ++ ".lfe"),
    Bfile = Base ++ ".beam",
    Cfile = Base ++ ".core",
    St1 = St0#comp{lfile=Lfile,
		   opts=Opts},
    %% Test for explicit out dir.
    case keysearch(outdir, 1, Opts) of
	{value,{outdir,D}} ->
	    St1#comp{bfile=filename:join(D, Bfile),
		     cfile=filename:join(D, Cfile)};
	_ ->
	    St1#comp{bfile=Bfile,cfile=Cfile}
    end.

%% forms(Forms) -> {ok,Bin,Warnings} | {error,Errors,Warnings}.
%% forms(Forms, Options) -> {ok,Bin,Warnings} | {error,Errors,Warnings}.

forms(Forms) -> forms(Forms, [report]).		%Default options.
forms(Forms, Opts) ->
    %% Do the actual compilation work.
    case forms(Forms, #comp{lfile="no-file"}, Opts) of
	{ok,Core,Ws,St} -> erl_comp(Core, Ws, Opts, St);
	{error,Es,Ws,St} -> do_error_return(Es, Ws, Opts, St)
    end.

%% forms(Forms, State, Options) ->
%%      {ok,Mod,Core,Warnings,State} | {error,Errors,Warnings,State}.
%%  Run the actual LFE compiler passes.

forms(Fs0, St0, Opts) ->
    St1 = St0#comp{opts=Opts},
    %% First macro expand forms.
    {Fs1,Env} = lfe_macro:expand_forms(Fs0, new_env()),
    debug_print("mac: ~p\n", [{Fs1,Env}], St1),
    %% Lint and then compile if ok.
    case lfe_lint:module(Fs1, Opts) of
	{ok,Ws} ->
	    {Mod,Core1} = lfe_codegen:forms(Fs1, Opts),
	    {ok,Core1,Ws,St1#comp{mod=Mod}};
	{error,Es,Ws} ->
	    {error,Es,Ws,St1}
    end.

%% erl_comp(Core, Warnings, Options, State) ->
%%      {ok,Mod[,Binary][,Warnings]}.
%%  Run the erlang compiler on the forms. Assume no errors here.

erl_comp(Core, Warns, Opts, St) ->
    %% Strip out options we don't want to send to erlang compiler.
    Eopts = strip_options(Opts),
    %% Search for selected options.
    Dcore = member(dcore, Opts),
    Tcore = member(to_core, Opts),
    Binary = member(binary, Opts),
    %% Fix returns accordingly.
    if Dcore ->					%Save raw core code
	    ok = file:write_file(St#comp.cfile, [core_pp:format(Core),$\n]),
	    do_ok_return([], Warns, Opts, St);
       Tcore ->					%Save optimised core code
	    {ok,_,Copt,Ews} =
		compile:forms(Core, [from_core,return|Eopts]),
	    ok = file:write_file(St#comp.cfile, [core_pp:format(Copt),$\n]),
	    do_ok_return([], Warns ++ fix_erl_errors(Ews), Opts, St);
       true ->					%Make BEAM code
	    {ok,_,Bin,Ews} =
		compile:forms(Core, [from_core,return|Eopts]),
	    Ret = if Binary -> [Bin];		%Return as binary
		     true ->			%Save BEAM file
			  ok = file:write_file(St#comp.bfile, Bin),
			  []
		  end,
	    do_ok_return(Ret, Warns ++ fix_erl_errors(Ews), Opts, St)
    end.

%% fix_erl_errors([{File,Errors}]) -> Errors.

fix_erl_errors([{_,Es}|Fes]) -> Es ++ fix_erl_errors(Fes);
fix_erl_errors([]) -> [].

%% strip_options(Options) -> Options.
%%  Strip out options to make sure erlang compiler returns everything.

strip_options([report|Os]) -> strip_options(Os);
strip_options([report_warnings|Os]) -> strip_options(Os);
strip_options([report_errors|Os]) -> strip_options(Os);
strip_options([O|Os]) -> [O|strip_options(Os)];
strip_options([]) -> [].

%% do_ok_return(Ret, Warnings, Options, State) -> {ok,Mod,...}.
%% do_error_return(Errors, Warnings, Options, State) -> {error,...} | error.

do_ok_return(Ret0, Warns, Opts, St) ->
    Lfile = St#comp.lfile,
    unless_opt(fun () -> list_warnings(Lfile, Warns) end, return, Opts),
    Ret1 = case member(return, Opts) of
	       true -> Ret0 ++ [return_errors(Lfile, Warns)];
	       false -> Ret0
	   end,
    list_to_tuple([ok,St#comp.mod|Ret1]).

do_error_return(Es, Ws, Opts, St) ->
    Lfile = St#comp.lfile,
    unless_opt(fun () -> list_errors(Lfile, Es) end, return, Opts),
    unless_opt(fun () -> list_warnings(Lfile, Ws) end, return, Opts),
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

unless_opt(Fun, Opt, Opts) ->
    case member(Opt, Opts) of
	true -> ok;
	false ->  Fun()
    end.

%% (defmacro when-opt (fun o os)
%%   `(if (member o os) '(funcall fun) ok))
%% (defmacro unless-opt (fun o os)
%%   `(if (member o os) 'ok (funcall fun)))
