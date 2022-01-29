%% Copyright (c) 2022 Robert Virding
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

%% File    : lfe_shell_docs.erl
%% Author  : Robert Virding
%% Purpose : Render LFE docs for output in shell.

%% The interface is loosely modelled on the shell_docs module.

-module(lfe_shell_docs).

-export([render/2,render/3,render/4]).

-include("lfe.hrl").
-include("lfe_docs.hrl").

%% Coloured strings for the LFE banner, red, green, yellow and blue.
-define(RED(Str), "\e[31m" ++ Str ++ "\e[0m").
-define(GRN(Str), "\e[1;32m" ++ Str ++ "\e[0m").
-define(YLW(Str), "\e[1;33m" ++ Str ++ "\e[0m").
-define(BLU(Str), "\e[1;34m" ++ Str ++ "\e[0m").
-define(BOLD(Str), "\e[1m" ++ Str ++ "\e[0m").

%% render(Module, Docs) -> unicode:chardata().

render(Bin, Docs) when is_binary(Bin) ->
    {ok,{Mod,_}} = beam_lib:chunks(Bin, [], []), %Sneaky!
    render(Mod, Docs);
render(Mod, #docs_v1{format = ?LFE_FORMAT, module_doc=Mdoc}) ->
    [red_line(60),
     lfe_io:format1(?BLU("~p")++"\n\n", [Mod]),
     return_doc(Mod, Mdoc)].

%% render(Module, Function, Docs) -> unicode:chardata().

render(_Mod, Name, #docs_v1{format = ?LFE_FORMAT, docs = Docs}) ->
    Render = fun ({{function,_Func,_Ar},_,Sig,Doc,Meta}) ->
                     [red_line(60),
                      return_sig(function, Sig, Meta),
                      return_doc(Sig, Doc)];
                 ({{macro,_Macro,_},_,Sig,Doc,Meta}) ->
                     [red_line(60),
                      return_sig(macro, Sig, Meta),
                      return_doc(Sig, Doc)]
             end,
    Ret = [ Render(F) || {{_,N,_},_,_,_,_}=F <- Docs, N =:= Name ],
    return_render(Ret, function_missing).

%% render(Module, Function, Arity, Docs) -> unicode:chardata().

render(_Mod, Name, Arity, #docs_v1{format = ?LFE_FORMAT, docs = Docs}) ->
    Render = fun ({{function,_Func,_Ar},_,Sig,Doc,Meta}) ->
                     [red_line(60),
                      return_sig(function, Sig, Meta),
                      return_doc(Sig, Doc)]
             end,
    Ret = [ Render(F) || {{function,N,A},_,_,_,_}=F <- Docs,
                         N =:= Name, A =:= Arity ],
    return_render(Ret, function_missing).

return_doc(_Missing, #{<<"en">> := Dv}) ->
    lfe_io:format1("~s\n", [Dv]);
return_doc(Missing, None) when None =:= none; None =:= #{} ->
    lfe_io:format1(<<"No documentation for ~s\n">>, [Missing]);
return_doc(Missing, _Docs) ->
    lfe_io:format1(<<"Unknown format for ~s\n">>, [Missing]).

%% return_sig(_Type, _Sig, #{signature:=[Spec]}) ->
%%     lfe_io:format1(?BLU("~s") ++ "\n", [erl_pp:form(Spec)]);
return_sig(Type, Sig, _Meta) ->
    lfe_io:format1(?BLU("~s ~s") ++ "\n\n", [Type,Sig]).

return_render([], Error) -> {error,Error};
return_render(FDocs, _Error) -> FDocs.

%% red_line(Length) -> ok.
%%  Output a red line of Length characters.

red_line(Len) ->
    io_lib:format(?RED("~*c")++"\n", [Len,$-]).
