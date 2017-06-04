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

%% File    : lfe_env.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang environment functions.

-module(lfe_env).

-export([new/0,add_env/2,
         get_vars/1,clr_vars/1,set_vars/2,fold_vars/3,
         get_funs/1,clr_funs/1,set_funs/2,fold_funs/3,fold_macros/3,
         add_vbinding/3,add_vbindings/2,is_vbound/2,get_vbinding/2,
         fetch_vbinding/2,del_vbinding/2,
         add_fbinding/4,add_fbindings/2,
         is_fbound/3,get_fbinding/3,add_ibinding/5,
         add_mbinding/3,add_mbindings/2,
         is_mbound/2,get_mbinding/2]).

%% Define access macros depending on whether we have maps.
-ifdef(HAS_MAPS).
-define(NEW(), #{}).
-define(IS_KEY(K, D), maps:is_key(K, D)).
-define(GET(K, D), maps:get(K, D)).
-define(FIND(K, D), maps:find(K, D)).
-define(PUT(K, V, D), maps:put(K, V, D)).
-define(ERASE(K, D), maps:remove(K, D)).
-define(FOLD(F, A, D), maps:fold(F, A, D)).
-define(UPDATE(K, UPD, DEF, D),                 %This is slightly complex
        begin (fun (___K, {ok,___V}) ->
                       maps:put(___K, UPD(___V), D);
                   (___K, error) ->
                       maps:put(___K, DEF, D)
               end)(K, maps:find(K, D)) end).
-else.
-define(NEW(), orddict:new()).
-define(IS_KEY(K, D), orddict:is_key(K, D)).
-define(GET(K, D), orddict:fetch(K, D)).
-define(FIND(K, D), orddict:find(K, D)).
-define(PUT(K, V, D), orddict:store(K, V, D)).
-define(ERASE(K, D), orddict:erase(K, D)).
-define(FOLD(F, A, D), orddict:fold(F, A, D)).
-define(UPDATE(K, UPD, DEF, D), orddict:update(K, UPD, DEF, D)).
-endif.

%% The environment structure.
-record(env, {vars=null,funs=null}).

%% -compile([export_all]).

%% new() -> Env.
%% add_env(Env1, Env2) -> Env.
%% get_vars(Env) -> Vars.
%% clr_vars(Env) -> Env.
%% set_vars(Vars, Env) -> Env.
%% fold_vars(Fun, Acc, Env) -> Acc.
%% get_funs(Env) -> Funs.
%% clr_funs(Env) -> Env.
%% set_funs(Funs, Env) -> Env.
%% fold_funs(Fun, Acc, Env) -> Acc.
%% fold_macros(Fun, Acc, Env) -> Acc.
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

new() -> #env{vars=?NEW(),funs=?NEW()}.

-ifdef(HAS_MAPS).
add_env(#env{vars=Vs1,funs=Fs1}, #env{vars=Vs2,funs=Fs2}) ->
    #env{vars=maps:merge(Vs2, Vs1),             %Always take left env
         funs=maps:merge(Fs2, Fs1)}.
-else.
add_env(#env{vars=Vs1,funs=Fs1}, #env{vars=Vs2,funs=Fs2}) ->
    Merge = fun (_, V1, _) -> V1 end,           %Always take left env
    #env{vars=orddict:merge(Merge, Vs1, Vs2),
         funs=orddict:merge(Merge, Fs1, Fs2)}.
-endif.

%% Accessing the variable table.

get_vars(Env) -> Env#env.vars.
clr_vars(Env) -> Env#env{vars=?NEW()}.
set_vars(Vars, Env) -> Env#env{vars=Vars}.
fold_vars(Fun, Acc, Env) ->
    ?FOLD(Fun, Acc, Env#env.vars).

%% Accessing the function/macro table.

get_funs(Env) -> Env#env.funs.
clr_funs(Env) -> Env#env{funs=?NEW()}.
set_funs(Funs, Env) -> Env#env{funs=Funs}.

%% Fold over functions and macros.

fold_funs(Fun, Acc, Env) ->
    Ofun = fun (F, {function,Fs}, Ac) ->        %Function
                   Ffun = fun ({Ar,Def}, A) ->
                                  Fun(F, Ar, Def, A)
                          end,
                   lists:foldl(Ffun, Ac, Fs);
               (_, _, A) -> A                   %Macro
           end,
    ?FOLD(Ofun, Acc, Env#env.funs).

fold_macros(Fun, Acc, Env) ->
    Ofun = fun (F, {macro,Def}, A) ->           %Macro
                   Fun(F, Def, A);
               (_, _, A) -> A                   %Function
           end,
    ?FOLD(Ofun, Acc, Env#env.funs).

%% Variables.

add_vbinding(N, V, #env{vars=Vs}=Env) ->
    Env#env{vars=?PUT(N, V, Vs)}.

add_vbindings(Vbs, #env{vars=Vs0}=Env) ->
    Vs1 = lists:foldl(fun ({N,V}, Vs) -> ?PUT(N, V, Vs) end, Vs0, Vbs),
    Env#env{vars=Vs1}.

is_vbound(N, #env{vars=Vs}) ->
    ?IS_KEY(N, Vs).

get_vbinding(N, #env{vars=Vs}) ->
    case ?FIND(N, Vs) of
        {ok,V} -> {yes,V};
        error -> no
    end.

fetch_vbinding(N, #env{vars=Vs}) ->
    ?GET(N, Vs).

del_vbinding(N, #env{vars=Vs}=Env) ->
    Env#env{vars=?ERASE(N, Vs)}.

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
    ?UPDATE(N, Upd, Def, Fs).

add_fbindings(Fbs, #env{funs=Fs0}=Env) ->
    Fs1 = lists:foldl(fun ({N,A,V}, Fs) -> add_fbinding_1(N, A, {A,V}, Fs) end,
		      Fs0, Fbs),
    Env#env{funs=Fs1}.

add_ibinding(M, R, A, L, #env{funs=Fs0}=Env) ->
    Fs1 = add_fbinding_1(L, A, {A,M,R}, Fs0),
    Env#env{funs=Fs1}.

is_fbound(N, A, #env{funs=Fs}) ->
    case ?FIND(N, Fs) of
        {ok,{function,Fas}} ->
            case lists:keyfind(A, 1, Fas) of
                false -> false;
                _ -> true
            end;
        _ -> false                              %A macro or not found
    end.

get_fbinding(N, A, #env{funs=Fs}) ->
    case ?FIND(N, Fs) of
        {ok,{function,Fas}} ->
            case lists:keyfind(A, 1, Fas) of
                {A,M,F} -> {yes,M,F};
                {A,V} -> {yes,V};
                false -> no
            end;
        _ -> no                                 %A macro or not found
    end.

%% Macros.

add_mbinding(N, V, #env{funs=Fs}=Env) ->
    Env#env{funs=?PUT(N, {macro,V}, Fs)}.

add_mbindings(Fbs, #env{funs=Fs0}=Env) ->
    Fs1 = lists:foldl(fun ({N,V}, Fs) -> ?PUT(N, {macro,V}, Fs) end,
		      Fs0, Fbs),
    Env#env{funs=Fs1}.

is_mbound(N, #env{funs=Fs}) ->
    case ?FIND(N, Fs) of
        {ok,{macro,_}} -> true;
        _ -> false
    end.

get_mbinding(N, #env{funs=Fs}) ->
    case ?FIND(N, Fs) of
        {ok,{macro,V}} -> {yes,V};
        _ -> no
    end.
