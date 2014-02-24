%% Copyright (c) 2008-2014 Robert Virding
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

%% File    : lfe_env.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang environment functions.

-module(lfe_env).

-export([new/0,add_env/2,
	 add_vbinding/3,add_vbindings/2,is_vbound/2,get_vbinding/2,
	 fetch_vbinding/2,update_vbinding/3,del_vbinding/2,
	 add_fbinding/4,add_fbindings/2,update_fbinding/4,
	 is_fbound/3,get_fbinding/3,add_ibinding/5,
	 is_gbound/3,get_gbinding/3,
	 add_mbinding/3,add_mbindings/2,update_mbinding/3,
	 is_mbound/2,get_mbinding/2]).

-import(lfe_lib, [is_bif/2,is_lfe_bif/2,is_erl_bif/2,is_guard_bif/2]).
-import(lists, [reverse/1,reverse/2,map/2,foldl/3,dropwhile/2]).

%% -compile([export_all]).

%% new() -> Env.
%% add_env(Env1, Env2) -> Env.
%% add_vbinding(Name, Val, Env) -> Env.
%% add_vbindings([{Name,Val}], Env) -> Env.
%% update_vbinding(Name, Val, Env) -> Env.
%% is_vbound(Symb, Env) -> bool().
%% get_vbinding(Name, Env) -> {yes,Val} | no.
%% fetch_vbinding(Name, Env) -> Val.
%% del_vbinding(Name, Env) -> Env.
%% add_fbinding(Name, Arity, Val, Env) -> Env.
%% add_fbindings([{Name,Arity,Val}], Env) -> Env.
%% update_fbinding(Name, Arity, Val, Env) -> Env.
%% add_ibinding(Mod, Name, Arity, LocalName, Env) -> Env.
%% is_fbound(Symb, Arity, Env) -> bool().
%% get_fbinding(Name, Arity, Env) -> {yes,Val} | {yes,Mod,Name} | no.
%% is_gbound(Symb, Arity, Env) -> bool().
%% get_gbinding(Name, Arity, Env) -> {yes,Mod,Name} | no.
%% add_mbinding(Name, Macro, Env) -> Env.
%% add_mbindings([{Name,Macro}], Env) -> Env.
%% update_mbinding(Name, Macro, Env) -> Env.
%% is_mbound(Symb, Env) -> bool().
%% get_mbinding(Name, Env) -> {yes,Macro} | no.
%%
%%  Unfortunately the searching property means we can not use a simple
%%  dictionary but need an ordered sequence.
%%
%%  The dictionary has structures:
%%  Variables - {variable,Name,Value}
%%  Functions - {function,Name,Arity,Value}
%%  Imports -   {function,LocalName,Arity,Module,RemoteName}
%%  Macros -    {macro,Name,Value}.
%%
%%  Macros and functions occupy the same environment so they shadow
%%  each other.
%%
%%  add_Xbinding adds a completely *new* binding to the head of Env.
%%  update_Xbinding finds the closest existing binding and updates it.

new() -> [].

add_env(E1, E2) -> E1 ++ E2.

add_vbinding(N, V, Env) -> [{variable,N,V}|Env].

add_vbindings(Vbs, Env) ->
    foldl(fun ({N,V}, E) -> add_vbinding(N, V, E) end, Env, Vbs).

update_vbinding(N, V, [{variable,N,_}|Env]) -> [{variable,N,V}|Env];
update_vbinding(N, V, [Vb|Env]) ->
    [Vb|update_vbinding(N, V, Env)].

is_vbound(N, [{variable,N,_}|_]) -> true;
is_vbound(N, [_|Env]) -> is_vbound(N, Env);
is_vbound(_, []) -> false.

get_vbinding(N, [{variable,N,V}|_]) -> {yes,V};
get_vbinding(N, [_|Env]) -> get_vbinding(N, Env);
get_vbinding(_, []) -> no.

fetch_vbinding(N, [{variable,N,V}|_]) -> V;
fetch_vbinding(N, [_|Env]) -> fetch_vbinding(N, Env).

del_vbinding(N, [{variable,N,_}|Env]) -> Env;
del_vbinding(N, [Vb|Env]) -> [Vb|del_vbinding(N, Env)];
del_vbinding(_, []) -> [].			%Be nice but should we

add_fbinding(N, A, V, Env) -> [{function,N,A,V}|Env].

add_fbindings(Fbs, Env) ->
    foldl(fun ({N,Ar,V}, E) -> add_fbinding(N, Ar, V, E) end, Env, Fbs).

update_fbinding(N, A, V, [{function,N,A,_}|Env]) ->
    [{function,N,A,V}|Env];
update_fbinding(N, A, V, [Fb|Env]) ->
    [Fb|update_fbinding(N, A, V, Env)].

add_ibinding(M, R, A, L, Env) -> [{function,L,A,M,R}|Env].

is_fbound(N, A, [{function,N,A,_}|_]) -> true;
is_fbound(N, A, [{function,N,A,_,_}|_]) -> true;
is_fbound(N, _, [{macro,N,_}|_]) -> false;	%Macros shadow
is_fbound(N, A, [_|Env]) -> is_fbound(N, A, Env);
is_fbound(N, A, []) -> is_bif(N, A).    	%Known BIF, LFE or erlang

get_fbinding(N, A, [{function,N,A,V}|_]) -> {yes,V};
get_fbinding(N, A, [{function,N,A,M,F}|_]) -> {yes,M,F};	%Import
get_fbinding(N, _, [{macro,N,_}|_]) -> no;			%Macros shadow
get_fbinding(N, A, [_|Env]) -> get_fbinding(N, A, Env);
get_fbinding(N, A, []) ->
    %% First check if is an LFE BIF.
    case is_lfe_bif(N, A) of
	true -> {yes,lfe_lib,N};
	false ->
	    %% Now check if it is a known BIF.
	    case is_erl_bif(N, A) of
		true -> {yes,erlang,N};
		false -> no
	    end
    end.

is_gbound(N, A, [{function,N,A,_}|_]) -> false;
is_gbound(N, A, [{function,N,A,_,_}|_]) -> false;
is_gbound(N, _, [{macro,N,_}|_]) -> false;	%Macros shadow
is_gbound(N, A, [_|Env]) -> is_gbound(N, A, Env);
is_gbound(N, A, []) -> is_guard_bif(N, A).    	%Known guard BIF

get_gbinding(N, A, [{function,N,A,_}|_]) -> no;
get_gbinding(N, A, [{function,N,A,_,_}|_]) -> no;	%Import
get_gbinding(N, _, [{macro,N,_}|_]) -> no;		%Macros shadow
get_gbinding(N, A, [_|Env]) -> get_gbinding(N, A, Env);
get_gbinding(N, A, []) ->
    case is_guard_bif(N, A) of
	true -> {yes,erlang,N};
	false -> no
    end.

add_mbinding(N, V, Env) -> [{macro,N,V}|Env].

add_mbindings(Mbs, Env) ->
    foldl(fun ({N,V}, E) -> add_mbinding(N, V, E) end, Env, Mbs).

update_mbinding(N, V, [{macro,N,_}|Env]) ->
    [{macro,N,V}|Env];
update_mbinding(N, V, [Mb|Env]) ->
    [Mb|update_mbinding(N, V, Env)].

is_mbound(N, [{function,N,_,_}|_]) -> false;	%Functions shadow
is_mbound(N, [{function,N,_,_,_}|_]) -> false;	%Functions shadow
is_mbound(N, [{macro,N,_}|_]) -> true;
is_mbound(N, [_|Env]) -> is_mbound(N, Env);
is_mbound(_, []) -> false.

get_mbinding(N, [{function,N,_,_}|_]) -> no;	%Functions shadow
get_mbinding(N, [{function,N,_,_,_}|_]) -> no;	%Functions shadow
get_mbinding(N, [{macro,N,V}|_]) -> {yes,V};
get_mbinding(N, [_|Env]) -> get_mbinding(N, Env);
get_mbinding(_, []) -> no.
