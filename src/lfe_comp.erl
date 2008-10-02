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
	       opts=[],				%Options
	       mod=[]				%Module name
}).

%% file(Name) -> {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns}.
%% file(Name, Options) ->
%%      {ok,Mod,Warns} | {ok,Mod,Binary,Ws} | {error,Errors,Warns}.

file(Name) -> file(Name, [report]).		%Default options

file(Name, Opts) ->
    St0 = #comp{},
    St1 = filenames(Name, Opts, St0),
    case lfe_io:parse_file(St1#comp.lfile) of
	{ok,Fs} ->
	    %% Do the actual compilation work.
	    case forms(Fs, St1, Opts) of
		{ok,Core,Ws,St2} ->
		    erl_comp(Core, Ws, Opts, St2);
		{error,Es,Ws,_} -> do_error_return(Es, Ws, Opts)
	    end;
	{error,Error} -> do_error_return([Error], [], Opts)
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

%% erl_comp(Core, Warnings, Options, State) ->
%%      {ok,Mod[,Binary][,Warnings]}.
%%  Run the erlang compiler on the forms. Assume no errors here.

erl_comp(Core, Warns, Opts, St) ->
    %% Search for selected options.
    Dcore = member(dcore, Opts),
    Tcore = member(to_core, Opts),
    Binary = member(binary, Opts),
    %% Fix returns accordingly.
    if Dcore ->					%Save raw core code
	    ok = file:write_file(St#comp.cfile, [core_pp:format(Core),$\n]),
	    do_ok_return([], Warns, Opts, St);
       Tcore ->					%Save optimised core code
	    {ok,_,Copt,Ws} =
		compile:forms(Core, [from_core,return|Opts]),
	    ok = file:write_file(St#comp.cfile, [core_pp:format(Copt),$\n]),
	    do_ok_return([], Warns ++ Ws, Opts, St);
       true ->					%Make BEAM code
	    {ok,_,Bin,Ws} =
		compile:forms(Core, [from_core,return|Opts]),
	    Ret = if Binary -> [Bin];		%Return as binary
		     true ->			%Save BEAM file
			  ok = file:write_file(St#comp.bfile, Bin),
			  []
		  end,
	    do_ok_return(Ret, Warns ++ Ws, Opts, St)
    end.

do_ok_return(Ret0, Warns, Opts, St) ->
    unless_opt(fun () -> list_warnings(Warns) end, return, Opts),
    Ret1 = case member(return, Opts) of
	       true -> Ret0 ++ [Warns];
	       false -> Ret0
	   end,
    list_to_tuple([ok,St#comp.mod|Ret1]).

do_error_return(Es, Ws, Opts) ->
    unless_opt(fun () -> list_errors(Es) end, return, Opts),
    unless_opt(fun () -> list_warnings(Ws) end, return, Opts),
    %% Fix right return.
    case member(return, Opts) of
	true -> {error,Es,Ws};
	false -> error
    end.

list_warnings([{Line,Mod,Warn}|Ws]) ->
    io:fwrite("~w: Warning: ~s\n", [Line,Mod:format_error(Warn)]),
    list_warnings(Ws);
list_warnings([{Mod,Warn}|Ws]) ->
    io:fwrite("Warning: ~s\n", [Mod:format_error(Warn)]),
    list_warnings(Ws);
list_warnings([]) -> [].

list_errors([{Line,Mod,Error}|Ws]) ->
    io:fwrite("~w: ~s\n", [Line,Mod:format_error(Error)]),
    list_errors(Ws);
list_errors([{Mod,Error}|Ws]) ->
    io:fwrite("~s\n", [Mod:format_error(Error)]),
    list_errors(Ws);
list_errors([]) -> [].

%% forms(Forms) -> {ok,Bin,Warnings} | {error,Errors,Warnings}.
%% forms(Forms, Options) -> {ok,Bin,Warnings} | {error,Errors,Warnings}.

forms(Forms) -> forms(Forms, [report]).		%Default options.
forms(Forms, Opts) ->
    case forms(Forms, #comp{}, Opts) of
	{ok,Core,Ws,St} ->
	    {ok,_,Bin} =
		compile:forms(Core, [from_core,return_errors|St#comp.opts]),
	    {ok,St#comp.mod,Bin,Ws};
	{error,Es,Ws,_} -> do_error_return(Es, Ws, Opts)
    end.

%% forms(Forms, State, Options) ->
%%      {ok,Mod,Core,Warnings,State} | {error,Errors,Warnings,State}.
%%  Run the actual compiler passes.

forms(Fs0, St0, Os) ->
    St1 = St0#comp{opts=Os},
    %% First macro expand forms.
    {Fs1,Env} = lfe_macro:expand_forms(Fs0, new_env()),
    debug_print("mac: ~p\n", [{Fs1,Env}], St1),
    %% Lint and then compile if ok.
    case lfe_lint:module(Fs1, St1#comp.opts) of
	{ok,Ws} ->
	    {Mod,Core1} = lfe_codegen:forms(Fs1, Os),
	    {ok,Core1,Ws,St1#comp{mod=Mod}};
	{error,Es,Ws} ->
	    {error,Es,Ws,St1}
    end.

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

%% (defmacro unless-opt (fun o os)
%%   `(if (member o os) 'ok (funcall fun)))
