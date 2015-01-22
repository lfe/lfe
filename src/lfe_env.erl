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
         get_vars/1,clr_vars/1,set_vars/2,get_funs/1,clr_funs/1,set_funs/2,
         add_vbinding/3,add_vbindings/2,is_vbound/2,get_vbinding/2,
         fetch_vbinding/2,del_vbinding/2,
         add_fbinding/4,add_fbindings/2,
         is_fbound/3,get_fbinding/3,add_ibinding/5,
         is_gbound/3,get_gbinding/3,
         add_mbinding/3,add_mbindings/2,
         is_mbound/2,get_mbinding/2]).

-import(lfe_lib, [is_bif/2,is_lfe_bif/2,is_erl_bif/2,is_guard_bif/2]).
-import(lists, [reverse/1,reverse/2,map/2,foldl/3,dropwhile/2]).

%% The environment structure.
-record(env, {vars=[],funs=[]}).

%% -compile([export_all]).

%% new() -> Env.
%% add_env(Env1, Env2) -> Env.
%% get_vars(Env) -> Vars.
%% clr_vars(Env) -> Env.
%% set_vars(Vars, Env) -> Env.
%% get_funs(Env) -> Funs.
%% clr_funs(Env) -> Env.
%% set_funs(Funs, Env) -> Env.
%% add_vbinding(Name, Val, Env) -> Env.
%% add_vbindings([{Name,Val}], Env) -> Env.
%% is_vbound(Symb, Env) -> bool().
%% get_vbinding(Name, Env) -> {yes,Val} | no.
%% fetch_vbinding(Name, Env) -> Val.
%% del_vbinding(Name, Env) -> Env.
%% add_fbinding(Name, Arity, Val, Env) -> Env.
%% add_fbindings([{Name,Arity,Val}], Env) -> Env.
%% add_ibinding(Mod, Name, Arity, LocalName, Env) -> Env.
%% is_fbound(Symb, Arity, Env) -> bool().
%% get_fbinding(Name, Arity, Env) -> {yes,Val} | {yes,Mod,Name} | no.
%% is_gbound(Symb, Arity, Env) -> bool().
%% get_gbinding(Name, Arity, Env) -> {yes,Mod,Name} | no.
%% add_mbinding(Name, Macro, Env) -> Env.
%% add_mbindings([{Name,Macro}], Env) -> Env.
%% is_mbound(Symb, Env) -> bool().
%% get_mbinding(Name, Env) -> {yes,Macro} | no.
%%
%%  The dictionary is in two parts: the variable bindings and the
%%  function/macro bindings, the vars and the funs fields respectively
%%  in the env record. The variables are kept in an orddict with the
%%  name as key.
%%
%%  For the functions/macros it is a little more complex due to
%%  shadowing. Defining a macro effectively shadows all the function
%%  bindings with the same name while defining a function effectively
%%  shadows a macro with the same name or replaces a function
%%  definition with the same name and arity. Functions are kept an
%%  orddict with the name as key and the value is either the macro
%%  definition or a dict of arity definition.
%%
%%  Rebinding a legal guard bif with a function in the module means it
%%  is no longer a legal guard bif but must be explicitly called with
%%  module erlang.

new() -> #env{vars=orddict:new(),funs=orddict:new()}. 

add_env(#env{vars=Vs1,funs=Fs1}, #env{vars=Vs2,funs=Fs2}) ->
    Merge = fun (_, V1, _) -> V1 end,           %Always take left env
    #env{vars=orddict:merge(Merge, Vs1, Vs2),
         funs=orddict:merge(Merge, Fs1, Fs2)}.

get_vars(Env) -> Env#env.vars.
clr_vars(Env) -> Env#env{vars=orddict:new()}.
set_vars(Vars, Env) -> Env#env{vars=Vars}.

get_funs(Env) -> Env#env.funs.
clr_funs(Env) -> Env#env{funs=orddict:new()}.
set_funs(Funs, Env) -> Env#env{funs=Funs}.

%% Variables.

add_vbinding(N, V, #env{vars=Vs}=Env) ->
    Env#env{vars=orddict:store(N, V, Vs)}.

add_vbindings(Vbs, #env{vars=Vs0}=Env) ->
    Vs1 = foldl(fun ({N,V}, Vs) -> orddict:store(N, V, Vs) end, Vs0, Vbs),
    Env#env{vars=Vs1}.

is_vbound(N, #env{vars=Vs}) ->
    orddict:is_key(N, Vs).

get_vbinding(N, #env{vars=Vs}) ->
    case orddict:find(N, Vs) of
        {ok,V} -> {yes,V};
        error -> no
    end.

fetch_vbinding(N, #env{vars=Vs}) ->
    orddict:fetch(N, Vs).

del_vbinding(N, #env{vars=Vs}=Env) ->
    Env#env{vars=orddict:erase(N, Vs)}.

%% Functions.

add_fbinding(N, A, V, #env{funs=Fs0}=Env) ->
    Fs1 = add_fbinding_1(N, A, {A,V}, Fs0),
    Env#env{funs=Fs1}.

add_fbinding_1(N, A, T, Fs) ->
    Def = {function,[T]},                       %We KNOW!
    Upd = fun ({function,Fas}) ->
                  {function,lists:keystore(A, 1, Fas, T)};
              (_) -> Def                        %Overwrite macros
          end,
    orddict:update(N, Upd, Def, Fs).

add_fbindings(Fbs, #env{funs=Fs0}=Env) ->
    Fs1 = foldl(fun ({N,A,V}, Fs) -> add_fbinding_1(N, A, {A,V}, Fs) end,
                Fs0, Fbs),
    Env#env{funs=Fs1}.

add_ibinding(M, R, A, L, #env{funs=Fs0}=Env) ->
    Fs1 = add_fbinding_1(L, A, {A,M,R}, Fs0),
    Env#env{funs=Fs1}.

is_fbound(N, A, #env{funs=Fs}) ->
    case orddict:find(N, Fs) of
        {ok,{function,Fas}} ->
	    case lists:keyfind(A, 1, Fas) of
		false -> is_bif(N, A);
		_ -> true
	    end;
        {ok,_} -> false;                        %A macro
        error -> is_bif(N, A)
    end.

get_fbinding(N, A, #env{funs=Fs}) ->
    case orddict:find(N, Fs) of
        {ok,{function,Fas}} ->
            case lists:keyfind(A, 1, Fas) of
                {A,M,F} -> {yes,M,F};
                {A,V} -> {yes,V};
                false -> get_bif(N, A)
            end;
        {ok,_} -> no;                           %A macro
        error -> get_bif(N, A)
    end.

get_bif(N, A) ->
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

is_gbound(N, A, #env{funs=Fs}) ->
    case orddict:find(N, Fs) of
        {ok,{function,Fas}} ->
	    case lists:keyfind(A, 1, Fas) of
		false -> is_guard_bif(N, A);
		_ -> false
	    end;
        {ok,_} -> false;                        %A macro
        error -> is_guard_bif(N, A)
    end.

get_gbinding(N, A, #env{funs=Fs}) ->
    case orddict:find(N, Fs) of
        {ok,{function,Fas}} ->
	    case lists:keyfind(A, 1, Fas) of
		false -> get_guard_bif(N, A);
		_ -> no
	    end;
        {ok,_} -> no;				%A macro
        error -> get_guard_bif(N, A)
    end.

get_guard_bif(N, A) ->
    case is_guard_bif(N, A) of
	true -> {yes,erlang,N};
	false -> no
    end.

%% Macros.

add_mbinding(N, V, #env{funs=Fs}=Env) ->
    Env#env{funs=orddict:store(N, {macro,V}, Fs)}.

add_mbindings(Fbs, #env{funs=Fs0}=Env) ->
    Fs1 = foldl(fun ({N,V}, Fs) -> orddict:store(N, {macro,V}, Fs) end,
                Fs0, Fbs),
    Env#env{funs=Fs1}.

is_mbound(N, #env{funs=Fs}) ->
    case orddict:find(N, Fs) of
        {ok,{macro,_}} -> true;
        _ -> false
    end.

get_mbinding(N, #env{funs=Fs}) ->
    case orddict:find(N, Fs) of
        {ok,{macro,V}} -> {yes,V};
        _ -> no
    end.
