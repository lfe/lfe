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

-include("lfe_docs.hrl").

%% Internal lfe_doc records.
-record(docs, {copts=[],
               module=[],
               macs=[],
               funcs=[]
              }).

-record(module, {name=[],                       %Name
                 anno=[],                       %Annotations/line
                 docs=[],                       %Documentation
                 meta=[],                       %Metadata
                 atts=[],                       %Attributes
                 fexps=[],                      %Exported functions
                 mexps=[]                       %Exported macros
                }).

-record(function, {name=[],                     %Name
                   arity=[],                    %Arity
                   anno=[],                     %Annotations/line
                   docs=[],                     %Documentation
                   spec=none,                   %Spec
                   meta=[],                     %Meta information
                   def=[]                       %Definition
                  }).

-record(macro, {name=[],                        %Name
                arity=[],                       %Arity, always 1 for macros
                anno=[],                        %Annotations/line
                docs=[],                        %Documentation
                spec=none,                      %Spec
                meta=[],                        %Meta information
                def=[]                          %Definition
               }).

make_chunk(Code, CompilerOpts) ->
    {ok,DocInfo} = make_docs_info(Code, CompilerOpts),
    Chunk = {"Docs",erlang:term_to_binary(DocInfo)},
    {ok,Chunk}.

make_docs_info(Code, CompilerOpts) ->
    DS0 = #docs{copts=CompilerOpts,module=#module{}},
    DS1 = collect_forms(Code, DS0),
    #module{anno=Anno,docs=Mdocs} = DS1#docs.module,
    Funcs = generate_functions(DS1),
    Macros = generate_macros(DS1),
    DocInfo = docs_v1(Anno, Mdocs, #{}, Funcs ++ Macros),
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
                  ([export|Es], #module{fexps=Fes}=M) ->
                      M#module{fexps=collect_mod_exports(Fes, Es)};
                  (['export-macro'|Es], #module{mexps=Mes}=M) ->
                      M#module{mexps=collect_mod_exports(Mes, Es)};
                  (_, M) -> M
              end,
    lists:foldl(Collect, Mod, Attrs).

%% Must handle exporting all for functions and macros.
collect_mod_exports(_Exps, [all]) -> all;
collect_mod_exports(all, _Es) -> all;
collect_mod_exports(Exps, Es) -> Exps ++ Es.

collect_function(Name, Meta, Def, Line) ->
    F = #function{name=Name,
                  arity=function_arity(Def),
                  anno=Line,
                  meta=Meta,
                  def=Def},
    collect_fun_metas(Meta, F).

collect_fun_metas(Metas, Fun) ->
    Collect = fun ([doc|Ds], F) ->
                      F#function{docs=F#function.docs ++ Ds};
                  ([spec|Ss], F) ->
                      F#function{spec=Ss};
                  (_, F) -> F
          end,
    lists:foldl(Collect, Fun, Metas).

function_arity([lambda,Args|_]) -> length(Args);
function_arity(['match-lambda',[Pat|_]|_]) -> length(Pat).

collect_macro(Name, Meta, Def, Line) ->
    F = #macro{name=Name,
               arity=1,                         %Default for all macros
               anno=Line,
               meta=Meta,
               def=Def},
    collect_mac_metas(Meta, F).

collect_mac_metas(Metas, Mac) ->
    Collect = fun ([doc|Ds], M) ->
                  M#macro{docs=M#macro.docs ++ Ds};
              (_, M) -> M
          end,
    lists:foldl(Collect, Mac, Metas).

%% generate_functions(Docs) -> [FunctionDoc].

generate_functions(#docs{module=#module{fexps=Fexps},funcs=Funcs}) ->
    Fdoc = fun (#function{name=Name,arity=Arity,anno=Anno,docs=Docs}=F) ->
                   Sig = generate_sig(F),
                   Spec = generate_spec(F),
                   docs_v1_entry(function, Name, Arity, Anno, [Sig], Docs, Spec)
           end,
    [ Fdoc(F) || F <- Funcs, exported_function(F, Fexps)].

exported_function(_F, all) -> true;             %All functions are exported.
exported_function(#function{name=N,arity=A}, Fexps) ->
    lists:member([N,A], Fexps).

%% generate_macros(Docs) -> [MacroDoc].

generate_macros(#docs{module=#module{mexps=Mexps},macs=Macros}) ->
    Mdoc = fun (#macro{name=Name,arity=Arity,anno=Anno,docs=Docs}=M) ->
                   Sig = generate_sig(M),
                   Spec = generate_spec(M),
                   docs_v1_entry(macro, Name, Arity, Anno, [Sig], Docs, Spec)
           end,
    [ Mdoc(M) || M <- Macros, exported_macro(M, Mexps) ].

exported_macro(_M, all) -> true;                %All macros are exported.
exported_macro(#macro{name=N}, Mexps) ->
    lists:member(N, Mexps).

%% generate_sig(FunctionMacro) -> Signature.

generate_sig(#function{name=Name,arity=Arity,def=Def}) ->
    generate_sig(Name, Arity, Def);
generate_sig(#macro{name=Name,arity=Arity,def=Def}) ->
    generate_sig(Name, Arity, Def).

generate_sig(Name, Arity, _Def) ->
    Sig = lists:concat([Name,"/",Arity]),
    iolist_to_binary(Sig).

%% generate_spec(FunctionMacro) -> ErlangSpec.

generate_spec(#function{name=Name,arity=Arity,anno=Line,spec=Spec}) ->
    generate_spec(Name, Arity, Line, Spec);
generate_spec(#macro{name=Name,arity=Arity,anno=Line,spec=Spec}) ->
    generate_spec(Name, Arity, Line, Spec).

generate_spec(_Name, _Arity, _Line, none) ->
    #{};
generate_spec(Name, Arity, Line, Spec) ->
    %% Translate the LFE spec to Erlang format.
    Sdef = {{Name,Arity},lfe_types:to_func_spec_list(Spec, Line)},
    #{signature => [{attribute,Line,spec,Sdef}]}.

%% docs_v1(Anno, ModuleDocs, MetaData, Docs) -> #docs_v1{}.
%% docs_v1_entry(Kind, Name, Arity, Anno, Signature, Doc, MetaData) ->
%%     Docs_V1_Entry.

docs_v1(Anno, ModDoc, Metadata, Docs) ->
    Doc = docs_v1_doc(ModDoc),
    Meta = maps:merge(Metadata, #{otp_doc_vsn => ?CURR_DOC_VERSION}),
    #docs_v1{anno=Anno,
             beam_language=lfe,
             format= ?LFE_FORMAT,
             module_doc=Doc,
             metadata=Meta,
             docs=Docs}.

docs_v1_entry(Kind, Name, Arity, Anno, Sig, DocContent, Meta) ->
    Doc = docs_v1_doc(DocContent),
    {{Kind,Name,Arity}, Anno, Sig, Doc, Meta}.

docs_v1_doc([]) ->
    none;
docs_v1_doc(DocContent) ->
    #{<<"en">> => iolist_to_binary(DocContent)}.

%% get_module_doc(Module | Binary) -> {ok,Chunk} | {error,What}.
%%  Get the module doc chunk. If EEP48 is defined we can use the code
%%  module to do most of the work.

-ifdef(EEP48).

get_module_docs(Mod) when is_atom(Mod) ->
    code:get_doc(Mod);
get_module_docs(Bin) when is_binary(Bin) ->
    get_module_chunk(Bin).

-else.

get_module_docs(Mod) when is_atom(Mod) ->
    case code:get_object_code(Mod) of
        {Mod,Bin,_} ->
            get_module_chunk(Bin);
        error -> {error,non_existing}           %Could not find the module
    end;
get_module_docs(Bin) when is_binary(Bin) ->
    get_module_chunk(Bin).

-endif.

get_module_chunk(Bin) ->
    case beam_lib:chunks(Bin, ["Docs"], []) of
        {ok,{_,[{"Docs",Chunk}]}} ->
            {ok,binary_to_term(Chunk)};
        {error,beam_lib,Error} ->
            {error,Error}
    end.
