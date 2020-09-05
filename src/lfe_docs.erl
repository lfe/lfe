%% Copyright (c) 2020 Robert Virding
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

%% File    : lfe_docs.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang documentation handling.

%% This module takes a lot of input from an older module written by
%% Eric Bailey.
%%
%% The "Docs" format from EEP 48: Documentation storage and format
%%
%% {docs_v1,
%%  Anno :: erl_anno:anno(),
%%  BeamLanguage :: atom(),
%%  Format :: mime_type(),
%%  ModuleDoc :: #{DocLanguage := DocValue} | none | hidden,
%%  Metadata :: map(),
%%  Docs ::
%%    [{{Kind, Name, Arity},
%%      Anno :: erl_anno:anno(),
%%      Signature :: [binary()],
%%      Doc :: #{DocLanguage := DocValue} | none | hidden,
%%      Metadata :: map()
%%     }]} when DocLanguage :: binary(),
%%              DocValue :: binary() | term()

-module(lfe_docs).
-export([make_chunk/2,make_docs_info/2]).

-export([get_module_docs/1]).

%% The Docs v1 record.
-record(docs_v1, {anno,
                  beam_language,
                  format,
                  module_doc,
                  metadata,
                  docs = []
                 }).

%% Internal lfe_doc records.
-record(docs, {copts=[],
               module=[],
               macs=[],
               funcs=[]
              }).

-record(module, {name=[],                       %Name
                 anno=[],                       %Annotations
                 docs=[],                       %Documentation
                 meta=[],                       %Metadata
                 atts=[],                       %Attributes
                 fexps=[],                      %Exported functions
                 mexps=[]                       %Exported macros
                }).

-record(function, {name=[],
                   arity=[],
                   anno=[],
                   docs=[],
                   meta=[],
                   def=[]
                  }).

-record(macro, {name=[],
                arity=[],
                anno=[],
                docs=[],
                meta=[],
                def=[]
               }).

make_chunk(Code, CompilerOpts) ->
    {ok,DocInfo} = make_docs_info(Code, CompilerOpts),
    Chunk = {"Docs",erlang:term_to_binary(DocInfo)},
    {ok,Chunk}.

make_docs_info(Code, CompilerOpts) ->
    DS0 = #docs{copts=CompilerOpts,module=#module{}},
    DS1 = collect_forms(Code, DS0),
    %% io:format("~p\n", [DS1]),
    #module{anno=Anno,docs=Mdocs} = DS1#docs.module,
    Funcs = generate_funcs(DS1),
    Macros = generate_macros(DS1),
    DocInfo = #docs_v1{anno=Anno,
                       beam_language=lfe,
                       format= <<"text/markdown">>,
                       module_doc=#{<<"en">> => iolist_to_binary(Mdocs)},
                       metadata=#{},
                       docs=Funcs ++ Macros
                      },
    {ok,DocInfo}.

%% collect_forms(Code, DocsState) -> DocsState.

collect_forms(Code, DS) -> lists:foldl(fun collect_form/2, DS, Code).

collect_form({['define-module',Name,Meta,Atts],Line}, #docs{module=M0}=DS) ->
    M1 = M0#module{name=Name,anno=Line},
    M2 = collect_mod_attrs(Atts, M1),
    M3 = collect_mod_metas(Meta, M2),
    DS#docs{module=M3};
collect_form({['extend-module',Meta,Atts],_Line}, #docs{module=M0}=DS) ->
    M1 = collect_mod_attrs(Atts, M0),
    M2 = collect_mod_metas(Meta, M1),
    DS#docs{module=M2};
collect_form({['define-function',Name,Meta,Def],Line}, #docs{funcs=Fs}=DS) ->
    F = collect_function(Name, Meta, Def, Line),
    DS#docs{funcs=Fs ++ [F]};
collect_form({['define-macro',Name,Meta,Def],Line}, #docs{macs=Ms}=DS) ->
    M = collect_macro(Name, Meta, Def, Line),
    DS#docs{macs=Ms ++ [M]};
collect_form(_, DS) -> DS.

collect_mod_metas(Metas, Mod) ->
    Collect = fun ([doc|Ds], M) ->
                  M#module{docs=M#module.docs ++ Ds};
              (_, M) -> M
          end,
    lists:foldl(Collect, Mod, Metas).

collect_mod_attrs(Attrs, Mod) ->
    Collect = fun ([doc|Ds], M) ->
                      M#module{docs=M#module.docs ++ Ds};
                  ([export|Es], M) ->
                      M#module{fexps=M#module.fexps ++ Es};
                  (['export-macro'|Es], M) ->
                      M#module{mexps=M#module.mexps ++ Es};
                  (_, M) -> M
              end,
    lists:foldl(Collect, Mod, Attrs).

collect_function(Name, Meta, Def, Line) ->
    F = #function{name=Name,arity=function_arity(Def),anno=Line,
                  meta=Meta,def=Def},
    collect_fun_metas(Meta, F).

collect_fun_metas(Metas, Fun) ->
    Collect = fun ([doc|Ds], F) ->
                  F#function{docs=F#function.docs ++ Ds};
              (_, F) -> F
          end,
    lists:foldl(Collect, Fun, Metas).

function_arity([lambda,Args|_]) -> length(Args);
function_arity(['match-lambda',[Pat|_]|_]) -> length(Pat).

collect_macro(Name, Meta, Def, Line) ->
    F = #macro{name=Name,arity=1,anno=Line,meta=Meta,def=Def},
    collect_mac_metas(Meta, F).

collect_mac_metas(Metas, Mac) ->
    Collect = fun ([doc|Ds], M) ->
                  M#macro{docs=M#macro.docs ++ Ds};
              (_, M) -> M
          end,
    lists:foldl(Collect, Mac, Metas).

%% generate_funcs(Docs) -> [FunctionDoc].

generate_funcs(#docs{module=#module{fexps=Fexps},funcs=Funcs}) ->
    Fdoc = fun (#function{name=Name,arity=Arity,anno=Anno,docs=Docs}=F) ->
                   {{function,Name,Arity},
                    Anno,
                    generate_func_sig(F),
                    #{<<"en">> => iolist_to_binary(Docs)},
                    #{}}
           end,
    [ Fdoc(F) || F <- Funcs, exported_func(F, Fexps)].

generate_func_sig(#function{name=Name,arity=Arity,def=Def}) ->
    BinSig = generate_sig(Name, Arity, Def),
    [BinSig].

generate_sig(Name, Arity, Def) ->
    Sig = case Def of
              [lambda,Args|_] ->
                  lfe_io:format1("~w ~w", [Name,Args]);
              _ -> lists:concat([Name,"/",Arity])
          end,
    iolist_to_binary(Sig).

exported_func(#function{name=N,arity=A}, Fexps) ->
    lists:member([N,A], Fexps).

%% generate_macros(Docs) -> [MacroDoc].

generate_macros(#docs{module=#module{mexps=Mexps},macs=Macros}) ->
    Mdoc = fun (#macro{name=Name,arity=Arity,anno=Anno,docs=Docs}=M) ->
                   {{macro,Name,Arity},
                    Anno,
                    generate_macro_sig(M),
                    #{<<"en">> => iolist_to_binary(Docs)},
                    #{}}
           end,
    [ Mdoc(M) || M <- Macros, exported_macro(M, Mexps) ].

generate_macro_sig(#macro{name=Name,arity=Arity,def=Def}) ->
    BinSig = generate_sig(Name, Arity, Def),
    [BinSig].

exported_macro(#macro{name=N}, Mexps) ->
    lists:member(N, Mexps).

%% get_module_docs(Module | Binary) -> {ok,Chunk} | {error,What}.

get_module_docs(Mod) when is_atom(Mod) ->
    case code:get_object_code(Mod) of
        {Mod,Bin,_} ->
            get_module_chunk(Bin);
        error -> {error,module}                 %Could not find the module
    end;
get_module_docs(Bin) when is_binary(Bin) ->
    get_module_chunk(Bin).

get_module_chunk(Bin) ->
    case beam_lib:chunks(Bin, ["Docs"], []) of
        {ok,{_,[{"Docs",Chunk}]}} ->
            {ok,binary_to_term(Chunk)};
        _ -> {error,docs}                       %Could not find the docs chunk
    end.
