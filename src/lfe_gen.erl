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

%%% File    : lfe_gen.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang dynamic code generator.

-module(lfe_gen).

-export([compile_forms/1]).
-export([new_module/1,add_exports/2,add_imports/2,add_form/2,
	 print_mod/1,compile_mod/1]).

-import(lists, [map/2,foldl/3,mapfoldl/3]).
-import(ordsets, [add_element/2]).
-import(orddict, [store/3,find/2]).

-record(gen, {name,exps=[],imps=[],atts=[],forms=[]}).

%% compile_forms(Forms) -> {ok,Name,Bin,Warns} | {error,Errors,Warns}.
%%  Compile all LFE module forms in one go. Always return binary and errors.

compile_forms(Fs) ->
    case lfe_comp:forms(Fs, [return]) of
	{ok,Mod,Bin,Ws} -> {ok,Mod,Bin,Ws};
	{error,Es,Ws} -> {error,Es,Ws}
    end.

%% new_module(Name) -> Module.
%% add_exports([{Name,Arity}], Module) -> Module.
%% add_imports({from,Mod,[{Name,Arity}]}, Module) -> Module.
%% add_form(Form, Module) -> Module.
%% print_mod(Module) -> iolist().
%% compile_mod(Mod) -> {ok,Name,Bin,Warns} | {error,Errors,Warns}.
%%  The incremental interface to compiling a module.

new_module(Name) ->
    #gen{name=Name,forms=[]}.

add_exports(Exps, Mod) ->
    Es0 = Mod#gen.exps,
    Es1 = foldl(fun ({N,Ar}, Es) when is_atom(N), is_integer(Ar) ->
			add_element({N,Ar}, Es)
		end, Es0, Exps),
    Mod#gen{exps=Es1}.

add_imports({from,M,Is}, Mod) ->
    Imps0 = Mod#gen.imps,
    Imps1 = collect_imp(fun ({F,A}, Imps) -> store({F,A}, F, Imps) end,
			M, Imps0, Is),
    Mod#gen{imps=Imps1};
add_imports({rename,M,Is}, Mod) ->
    Imps0 = Mod#gen.imps,
    Imps1 = collect_imp(fun ({{F,A},R}, Imps) -> store({F,A}, R, Imps) end,
			M, Imps0, Is),
    Mod#gen{imps=Imps1}.

add_form(Form, Mod) ->
    Mod#gen{forms=Mod#gen.forms ++ [Form]}.

compile_mod(Mod) ->
    Fs = [build_def(Mod)|Mod#gen.forms],
    compile_forms(Fs).

print_mod(Mod) ->				%Needs fixing
    map(fun (F) -> [lfe_io:prettyprint1(F),io_lib:nl()] end,
	[build_def(Mod)|Mod#gen.forms]).

collect_imp(Fun, Mod, Imps, Is) ->
    Mimps0 = safe_fetch(Mod, Imps, []),
    Mimps1 = foldl(Fun, Mimps0, Is),
    store(Mod, Mimps1, Imps).

%% build_def(ModDef) -> form().

build_def(Mod) ->
    Exps = map(fun ({N,I}) -> [N,I] end, Mod#gen.exps),
    Imps = map(fun ({M,Is}) ->
		       [rename,M|map(fun ({{L,Ar},R}) -> [[L,Ar],R] end,
				     Is)]
	       end, Mod#gen.imps),
    [defmodule,Mod#gen.name,
     [export|Exps],
     [import|Imps]].

%% safe_fetch(Key, Dict, Default) -> Value.

safe_fetch(Key, D, Def) ->
    case find(Key, D) of
	{ok,Val} -> Val;
	error -> Def
    end.
