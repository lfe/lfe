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
%% The module documentation now comes from 'moduledoc' attributes. We
%% accept many of them and all their docs are appended. Function and
%% macro documentation come from 'doc' attributes before them which is
%% kept in the new #docs doc field. This is unset (set to [] which is
%% the blank docs field in function and macro) for every form except
%% for 'spec' which can come together with a 'doc' before a 'function'
%% or 'macro'.
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
               doc=[],                         %Last doc before something
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
    %% io:format("fs ~p\n", [Funcs]),
    Macros = generate_macros(DS1),
    %% io:format("ms ~p\n", [Macros]),
    DocInfo = docs_v1(Anno, Mdocs, #{}, Funcs ++ Macros),
    {ok,DocInfo}.

%% collect_forms(Code, DocsState) -> DocsState.
%%  Note that this collects the normalise forms, the norms.

collect_forms(Code, DS) -> lists:foldl(fun collect_form/2, DS, Code).

collect_form(['module',Line,Name], #docs{module=M0}=DS) ->
    M1 = M0#module{name=Name,anno=Line},
    DS#docs{module=M1,doc=[]};
collect_form(['moduledoc',_Line,Doc], #docs{module=M0}=DS) ->
    %% io:format("do ~p\n", [{M0#module.docs,Doc}]),
    M1 = M0#module{docs=M0#module.docs ++ Doc},
    DS#docs{module=M1,doc=[]};
collect_form(['doc',_Line,Doc], DS) ->
    DS#docs{doc=Doc};
collect_form(['export',_Line,Exports], #docs{module=M0}=DS) ->
    M1 = M0#module{fexps=collect_mod_exports(M0#module.fexps, Exports)},
    DS#docs{module=M1,doc=[]};
collect_form(['export-macro',_Line,Exports], #docs{module=M0}=DS) ->
    M1 = M0#module{mexps=collect_mod_exports(M0#module.mexps, Exports)},
    DS#docs{module=M1,doc=[]};
collect_form([spec,Line,[Name,Arity],Spec], #docs{funcs=Funcs0}=DS) ->
    Funcs1 = collect_func_spec(Funcs0, Name, Arity, Line, Spec),
    %% Leave doc as function def can follow.
    DS#docs{funcs=Funcs1};
collect_form(['function',Line,Name,Def], #docs{funcs=Funcs0,doc=Doc}=DS) ->
    Arity = function_arity(Def),
    Funcs1 = collect_func_def(Funcs0, Name, Arity, Line, Doc, Def),
    DS#docs{funcs=Funcs1,doc=[]};
collect_form(['macro',Line,Name,Def], #docs{macs=Macs,doc=Doc}=DS) ->
    M = collect_macro(Name, Line, Doc, Def),
    DS#docs{macs=Macs ++ [M],doc=[]};

%% collect_form({['define-module',Name,Meta,Atts],Line}, #docs{module=M0}=DS) ->
%%     M1 = M0#module{name=Name,anno=Line},
%%     M2 = collect_mod_attrs(Atts, M1),
%%     M3 = collect_mod_metas(Meta, M2),
%%     DS#docs{module=M3};
%% collect_form({['extend-module',Meta,Atts],_Line}, #docs{module=M0}=DS) ->
%%     M1 = collect_mod_attrs(Atts, M0),
%%     M2 = collect_mod_metas(Meta, M1),
%%     DS#docs{module=M2};
%% collect_form({['define-function',Name,Meta,Def],Line}, #docs{funcs=Fs}=DS) ->
%%     F = collect_function(Name, Meta, Def, Line),
%%     DS#docs{funcs=Fs ++ [F]};
%% collect_form({['define-macro',Name,Meta,Def],Line}, #docs{macs=Ms}=DS) ->
%%     M = collect_macro(Name, Meta, Def, Line),
%%     DS#docs{macs=Ms ++ [M]};

collect_form(_, DS) -> DS#docs{doc=[]}.

%% collect_mod_metas(Metas, Mod) ->
%%     Collect = fun ([doc|Ds], M) ->
%%                   M#module{docs=M#module.docs ++ Ds};
%%               (_, M) -> M
%%           end,
%%     lists:foldl(Collect, Mod, Metas).

%% collect_mod_attrs(Attrs, Mod) ->
%%     Collect = fun ([doc|Ds], M) ->
%%                       M#module{docs=M#module.docs ++ Ds};
%%                   ([export|Es], #module{fexps=Fes}=M) ->
%%                       M#module{fexps=collect_mod_exports(Fes, Es)};
%%                   (['export-macro'|Es], #module{mexps=Mes}=M) ->
%%                       M#module{mexps=collect_mod_exports(Mes, Es)};
%%                   (_, M) -> M
%%               end,
%%     lists:foldl(Collect, Mod, Attrs).

%% Must handle exporting all for functions and macros.
collect_mod_exports(_Exps, all) -> all;
collect_mod_exports(all, _Es) -> all;
collect_mod_exports(Exps, Es) -> Exps ++ Es.

%% collect_func_def(Functions, Name, Arity, Line, Docs, Def) -> Functions.
%% collect_func_spec(Functions, Name, Arity, Line, Spec) -> Functions.
%%  Find or create the function definition and add the given field.

collect_func_def([#function{name=Name,arity=Arity}=F|Funcs],
                 Name, Arity, Line, Docs, Def) ->
    [F#function{anno=Line,docs=Docs,def=Def}|Funcs];
collect_func_def([F|Funcs], Name, Arity, Line, Docs, Def) ->
    [F|collect_func_def(Funcs, Name, Arity, Line, Docs, Def)];
collect_func_def([], Name, Arity, Line, Docs, Def) ->
    [#function{name=Name,arity=Arity,anno=Line,docs=Docs,def=Def}].

collect_func_spec([#function{name=Name,arity=Arity}=F|Funcs],
                  Name, Arity, _Line, Spec) ->
    [F#function{spec=Spec}|Funcs];
collect_func_spec([F|Funcs], Name, Arity, Line, Spec) ->
    [F|collect_func_spec(Funcs, Name, Arity, Line, Spec)];
collect_func_spec([], Name, Arity, Line, Spec) ->
    [#function{name=Name,arity=Arity,anno=Line,spec=Spec}].

function_arity([lambda,Args|_]) -> length(Args);
function_arity(['match-lambda',[Pat|_]|_]) -> length(Pat).

%% collect_macro(Name, Line, Docs, Def) -> Macro.

collect_macro(Name, Line, Docs, Def) ->
    #macro{name=Name,arity=1,anno=Line,docs=Docs,def=Def}.

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
