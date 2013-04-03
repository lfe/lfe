%% Copyright (c) 2013 Robert Virding
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

%% File    : lfe_macro_include.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang macro expander for include macros.

%% Expand the (include-file ...) and (include-lib ...) macros handling
%% if they are LFE syntax files or erlang syntax files. Erlang syntax
%% files are ones which end in .hrl. We only handle basic record and
%% macro definitions.

-module(lfe_macro_include).

-export([file/3,lib/3,format_error/1]).

-export([trans_forms/1]).

-compile([export_all]).

-include("lfe_macro.hrl").

read_hrl_file_1(Name) ->
    case epp:open(Name, []) of
	{ok,Epp} ->
	    %% These are two undocumented functions of epp.
	    Fs = epp:parse_file(Epp),
	    Ms = epp:macro_defs(Epp),
	    epp:close(Epp),			%Now we close epp
	    {ok,Fs,Ms};
	{error,E} -> {error,E}
    end.

%% Errors.
format_error({notrans_record,R}) ->
    io_lib:format("unable to translate record ~w", [R]);
format_error({notrans_macro,M}) ->
    io_lib:format("unable to translate macro ~w", [M]).

%% file([FileName], Env, State) -> {yes,(progn ...),State} | no.
%%  Expand the (include-file ...) macro.  This is a VERY simple
%%  include file macro! We just signal errors.

file(Body, _, St0) ->
    case include_name(Body) of
	{ok,Name} ->
	    case read_file(Name, St0) of	%Try to read file
		{ok,Fs,St1} -> {yes,['progn'|Fs],St1};
		{error,E} -> error(E)
	    end;
	{error,E} -> error(E)
    end.

%% lib([FileName], Env, State) -> {yes,(progn ...),State} | no.
%%  Expand the (include-lib ...) macro.  This is a VERY simple include
%%  lib macro! First try to include the file directly else assume
%%  first directory name is a library name. We just signal errors.

lib(Body, _, St0) ->
    case include_name(Body) of
	{ok,Name} ->
	    case read_file(Name, St0) of
		{ok,Fs,St1} -> {yes,['progn'|Fs],St1};
		{error,_} ->
		    case lib_file_name(Name) of
			{ok,Lfile} ->
			    case read_file(Lfile, St0) of
				{ok,Fs,St1} -> {yes,['progn'|Fs],St1};
				{error,E} -> error(E)
			    end;
			{error,_} -> error(badarg)
		    end
	    end;
	{error,E} -> error(E)
    end.

%% include_name(Body) -> bool().
%%  Gets the file name from the include-XXX body.

include_name([Name]) ->
    case io_lib:char_list(Name) of
	true -> {ok,Name};
	false -> {error,badarg}
    end;
include_name(_) -> {error,badarg}.

%% lib_file_name(LibPath) -> {ok,LibFileName} | {error,Error}.
%%  Construct path to true library file.

lib_file_name(Lpath) ->
    [Lname|Rest] = filename:split(Lpath),
    case code:lib_dir(list_to_atom(Lname)) of
	Ldir when is_list(Ldir) ->
	    {ok,filename:join([Ldir|Rest])};
	{error,E} -> {error,E}
    end.

%% read_file(FileName, State) -> {ok,Forms,State} | {error,Error}.

read_file(Name, St) ->
    case lists:suffix(".hrl", Name) of
	true -> read_hrl_file(Name, St);       %Read file as .hrl file
	false -> read_lfe_file(Name, St)
    end.

read_lfe_file(Name, St) ->
    %% Read the file as an LFE file.
    case lfe_io:read_file(Name) of
	{ok,Fs} -> {ok,Fs,St};
	{error,E} -> {error,E}
    end.

%% read_hrl_file(FileName, State) -> {ok,Forms,State} | {error,Error}.
%%  We use two undocumented functions of epp which allow us to get
%%  inside and get out the macros.

read_hrl_file(Name, St) ->
    case epp:open(Name, []) of
	{ok,Epp} ->
	    %% These are two undocumented functions of epp.
	    Fs = epp:parse_file(Epp),
	    Ms = epp:macro_defs(Epp),
	    epp:close(Epp),			%Now we close epp
	    parse_hrl_file(Fs, Ms, St);
	{error,E} -> {error,E}
    end.

%% parse_hrl_file(Forms, Macros, State) -> {ok,Forms,State} | {error,Error}.

parse_hrl_file(Fs0, Ms0, St) ->
    Fs1 = trans_forms(Fs0),
    Ms1 = trans_macros(Ms0),
    {ok,Fs1 ++ Ms1,St}.

trans_forms([{attribute,_,record,{Name,Fields}}|Fs]) ->
    Rs = record_fields(Fields),
    [[defrecord,Name|Rs]|trans_forms(Fs)];
trans_forms([{error,_}|Fs]) -> trans_forms(Fs);	%What should we do with these?
trans_forms([_|Fs]) -> trans_forms(Fs);		%Ignore everything else
trans_forms([]) -> [].

record_fields(Fs) ->
    [ record_field(F) || F <- Fs ].

record_field({record_field,_,F}) ->
    lfe_trans:from_lit(F);
record_field({record_field,_,F,Def}) ->
    Fd = lfe_trans:from_lit(F),
    Ld = lfe_trans:from_expr(Def),
    [Fd,Ld].

trans_macros([{_,undefined}|Ms]) ->		%Skip undefined macros
    trans_macros(Ms);
trans_macros([{{atom,Mac},{none,_}}|Ms]) ->	%Skip predefined macros
    trans_macros(Ms);
trans_macros([{{atom,Mac},Defs}|Ms]) ->
    case trans_macro_defs(Defs) of
	[] -> trans_macros(Ms);
	Lcls -> [[defmacro,Mac|Lcls]|trans_macros(Ms)]
    end;
%% trans_macros([{{atom,Mac},Defs}|Ms]) ->
%%     Mdef = trans_macro(Mac, Def),
%%     [Mdef|trans_macros(Ms)];
trans_macros([]) -> [].

%% trans_macro(Mac, {none,Ts}) ->			%Predefined macros
%%     [defmacro,Mac,[]|trans_macro_body([], Ts)];
trans_macro(Mac, Defs) ->
    case trans_macro_defs(Defs) of
	[] -> [progn];
	Lcls -> [defmacro,Mac|Lcls]
    end.

trans_macro_defs([{none,Ds}|Defs]) ->		%Put the no arg version last
    trans_macro_defs_1(Defs ++ [{none,Ds}]);
trans_macro_defs(Defs) ->
    trans_macro_defs_1(Defs).

trans_macro_defs_1([{none,{none,Ts}}|Defs]) ->
    case catch {ok,trans_macro_body([], Ts)} of
	{ok,Ld} ->
	    AnyArgs = ['_'|Ld],
	    [AnyArgs|trans_macro_defs_1(Defs)];
	_ -> trans_macro_defs_1(Defs)		%Skip errors
    end;
trans_macro_defs_1([{N,{As,Ts}}|Defs]) when is_integer(N) ->
    case catch {ok,trans_macro_body(As, Ts)} of
	{ok,Ld} -> [[As|Ld]|trans_macro_defs_1(Defs)];
	_ -> trans_macro_defs_1(Defs)		%Skip errors
    end;
trans_macro_defs_1([]) -> [].

trans_macro_body(As, Ts0) ->
    %% Wrap variables in arg list with an (unquote ...) call.
    Ts1 = lists:foldr(fun ({var,L,V}=T, Ts) ->
			      case lists:member(V, As) of
				  true ->
				      [{atom,L,unquote},{'(',L},T,{')',L}|Ts];
				  false -> [T|Ts]
			      end;
			  (T, Ts) -> [T|Ts]
		      end, [], Ts0),
    %% Only allow single expressions, otherwise screws up backquoting.
    {ok,[E]} = erl_parse:parse_exprs(Ts1 ++ [{dot,0}]),
    [?BQ(lfe_trans:from_expr(E))].

    %% {ok,[_]=F} = erl_parse:parse_exprs(Ts1 ++ [{dot,0}]),
    %% backquote_last(lfe_trans:from_body(F)).

%% Backquote the last expression in the body.
%% backquote_last([E]) -> [?BQ(E)];
%% backquote_last([E|Es]) -> [E|backquote_last(Es)].
