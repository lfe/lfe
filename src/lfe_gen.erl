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

%%% File    : lfe_gen.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang dynamic code generator.

%% We have kept the old tuple based formats for exports and imports
%% for backwards compatibility but they are not documented.

-module(lfe_gen).

-export([new_module/1,add_exports/2,add_imports/2,add_attribute/2,add_form/2,
         build_mod/1,compile_mod/1]).

-import(lists, [map/2,foldl/3,mapfoldl/3]).

-record(gen, {name,exps=[],imps=[],attrs=[],forms=[]}).

%% new_module(Name) -> Module.
%% add_exports([{Name,Arity}], Module) -> Module.
%% add_imports({from,Mod,[{Name,Arity}]}, Module) -> Module.
%% add_attribute(Attr, Module) -> Module.
%% add_form(Form, Module) -> Module.
%% build_mod(Module) -> iolist().
%% compile_mod(Mod) -> {ok,Name,Bin,Warns} | {error,Errors,Warns}.
%%  The incremental interface to compiling a module.

new_module(Name) ->
    #gen{name=Name,forms=[]}.

add_exports(Exps, Mod) ->
    Es0 = Mod#gen.exps,
    Es1 = foldl(fun ([N,Ar], Es) when is_atom(N), is_integer(Ar) ->
                        ordsets:add_element([N,Ar], Es);
                    ({N,Ar}, Es) when is_atom(N), is_integer(Ar) ->
                        ordsets:add_element([N,Ar], Es)
                end, Es0, Exps),
    Mod#gen{exps=Es1}.

add_imports([from,M|Is], Mod) ->
    collect_from_imports(M, Is, Mod);
add_imports([rename,M|Is], Mod) ->
    collect_rename_imports(M, Is, Mod);
%% The older now deprecated forms.
add_imports({from,M,Is}, Mod) ->
    collect_from_imports(M, Is, Mod);
add_imports({rename,M,Is}, Mod) ->
    collect_rename_imports(M, Is, Mod).

collect_from_imports(M, Is, #gen{imps=Imps0}=Mod) ->
    From = fun ([F,A], Imps) -> store_import(F, A, F, Imps);
               ({F,A}, Imps) -> store_import(F, A, F, Imps)
           end,
    Imps1 = collect_imp(From, M, Imps0, Is),
    Mod#gen{imps=Imps1}.

collect_rename_imports(M, Is, #gen{imps=Imps0}=Mod) ->
    Rename = fun ([[F,A],R], Imps) -> store_import(F, A, R, Imps);
                 ({{F,A},R}, Imps) -> store_import(F, A, R, Imps)
             end,
    Imps1 = collect_imp(Rename, M, Imps0, Is),
    Mod#gen{imps=Imps1}.

store_import(F, A, R, Imps) ->
    orddict:store([F,A], R, Imps).

collect_imp(Fun, Mod, Imps, Is) ->
    Mimps0 = safe_fetch(Mod, Imps, []),
    Mimps1 = foldl(Fun, Mimps0, Is),
    orddict:store(Mod, Mimps1, Imps).

add_attribute(Attr, #gen{attrs=As}=Mod) ->
    Mod#gen{attrs=As ++ [Attr]}.

add_form(Form, #gen{forms=Fs}=Mod) ->
    Mod#gen{forms=Fs ++ [Form]}.

compile_mod(Mod) ->
    Fs = build_mod(Mod),
    case lfe_comp:forms(Fs, [return]) of
        {ok,[{ok,Name,Bin,Mws}],Ws} -> {ok,Name,Bin,Ws ++ Mws};
        {error,[{error,Mes,Mws}],Es,Ws} -> {error,Es ++ Mes,Ws ++ Mws}
    end.

build_mod(Mod) ->
    [build_def(Mod)|Mod#gen.forms].

%% build_def(ModDef) -> form().

build_def(Mod) ->
    Exps = Mod#gen.exps,                        %This is a set of [F,A]
    %% We know these are orddicts.
    ImpFun = fun ({M,Is}) ->
                     [rename,M|map(fun ({F,R}) -> [F,R] end, Is)]
             end,
    Imps = map(ImpFun, Mod#gen.imps),
    [defmodule,Mod#gen.name,
     [export|Exps],
     [import|Imps]|
     Mod#gen.attrs].

%% safe_fetch(Key, Dict, Default) -> Value.

safe_fetch(Key, D, Def) ->
    case orddict:find(Key, D) of
        {ok,Val} -> Val;
        error -> Def
    end.
