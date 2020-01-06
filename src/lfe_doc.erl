%% Copyright (c) 2016 Eric Bailey
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

%% File    : lfe_doc.erl
%% Author  : Eric Bailey
%% Purpose : Lisp Flavoured Erlang (LFE) documentation parser.

%% There is no guarantee the internal formats will not change
%% but the interface functions should stay the same.

-module(lfe_doc).

-export([format_error/1]).

-export([extract_module_docs/1,extract_module_docs/2,save_module_docs/3]).

%% Access functions for documentation in modules.
-export([get_module_docs/1,module_doc/1,mf_docs/1,mf_doc_type/1,
         function_docs/3,macro_docs/2,
         function_name/1,function_arity/1,function_line/1,
         function_patterns/1, function_doc/1,
         macro_name/1,macro_line/1,macro_patterns/1,macro_doc/1]).

-export([format_docs/1]).

-import(lists, [member/2,filter/2,foldl/3,foldr/3,reverse/1]).

-include("lfe_comp.hrl").
-include("lfe_doc.hrl").

-ifdef(EUNIT).
-export([collect_docs/2,pprint/2]).             %Used by prop_lfe_doc

-include_lib("eunit/include/eunit.hrl").

-define(QC_OPTS, [{on_output,fun pprint/2},{numtests,1000},{max_size,10}]).
-define(QC(T,P), {timeout,30,{T,?_assert(proper:quickcheck(P, ?QC_OPTS))}}).
-endif.

%% Errors
format_error(_) -> "doc error".

%% extract_module_docs(Defs, CompInfo) -> {ok,Docs} | {error,Errors,[]}.
%%  Parse a module's docstrings and return the docs.

-spec extract_module_docs(Defs, Cinfo) -> {ok,Docs} | {error,Errors,[]} when
      Defs   :: [{Form,Line}],
      Form   :: [_],
      Line   :: non_neg_integer(),
      Cinfo  :: #cinfo{},
      Docs   :: [doc()],
      Errors :: nonempty_list({error,Line,Error}),
      Error  :: any().

extract_module_docs(Defs) ->                    %Just give a default #cinfo{}
    extract_module_docs(Defs, #cinfo{}).

extract_module_docs([], _Ci)  -> {ok,[]};
extract_module_docs(Defs, Ci) ->
    {Mdoc,Docs} = do_forms(Defs),
    Errors = filter(fun (#doc{}) -> false;
                        (_)      -> true
                    end, Docs),
    ?DEBUG("#doc: ~p\n", [{Mdoc,Docs}], Ci#cinfo.opts),
    ?IF([] =:= Errors,
        {ok,{Mdoc,Docs}},
        {error,Errors,[]}).

do_forms(Fs) -> foldl(fun do_form/2, {[],[]}, Fs).

do_form({['define-module',_,Meta,Atts],_}, {Mdoc,Docs}) ->
    {do_module_def(Meta, Atts, Mdoc),Docs};
do_form({['extend-module',Meta,Atts],_}, {Mdoc,Docs}) ->
    {do_module_def(Meta, Atts, Mdoc),Docs};
do_form({['define-function',Name,Meta,Def],Line}, {Mdoc,Docs}) ->
    {Mdoc,do_function(Name, Def, Meta, Line, Docs)};
do_form({['define-macro',Name,Meta,Def],Line}, {Mdoc,Docs}) ->
    {Mdoc,do_macro(Name, Def, Meta, Line, Docs)};
do_form(_, Doc) -> Doc.                         %Ignore eval-when-compile

do_module_def(Meta, Atts, Mdoc0) ->
    Mdoc1 = collect_docs(Meta, Mdoc0),          %First take meta docs
    collect_docs(Atts, Mdoc1).                  %then the attribute docs

collect_docs(As, Mdoc) ->
    %% Collect all the docs in all the doc metas/attributes.
    Afun = fun ([doc|Docs], Md) ->
                   Dfun = fun (D, M) -> M ++ [string_to_binary(D)] end,
                   foldl(Dfun, Md, Docs);
               (_, Md) -> Md
           end,
    foldl(Afun, Mdoc, As).

do_function(Name, Def, Meta, Line, Docs) ->
    %% Must get patterns and arity before we can check if excluded.
    {Arity,Pats} = get_function_patterns(Def),
    ?IF(exclude(Name, Arity, Meta),
        Docs,
        begin
            Fdoc = make_function_doc(Name, Arity, Pats, Meta, Line),
            [Fdoc|Docs]
        end).

do_macro(Name, Def, Meta, Line, Docs) ->
    %% We only need the name to check for exclusion.
    ?IF(exclude(Name, Meta),
        Docs,
        begin
            Pats = get_macro_patterns(Def),
            Mdoc = make_macro_doc(Name, Pats, Meta, Line),
            [Mdoc|Docs]
        end).

%% exclude(Name, Arity, Meta) -> boolean().
%% exclude(Name, Meta) -> boolean().
%%  Return true if a function should be excluded from the docs chunk.
%%  $handle_undefined_function/2 needs special handling as it is
%%  automatically generated but can also be defined by the user. So we
%%  only include it is it has user documentation.

-spec exclude(Name, Arity, Meta) -> boolean() when
      Name  :: atom(),
      Arity :: non_neg_integer(),
      Meta  :: list().
-spec exclude(Name, Meta) -> boolean() when
      Name  :: atom(),
      Meta  :: list().

exclude('LFE-EXPAND-EXPORTED-MACRO', 3, _)  -> true;
exclude('$handle_undefined_function', 2, _) ->  %Should check for doc string
    true;
exclude(_, _, _) -> false.

exclude('MODULE', _) -> true;
exclude(_, _)        -> false.

%% get_function_patterns(LambdaForm) -> {Arity,Patterns}.
%% get_macro_patterns(LambdaForm)    -> Patterns.
%%  Given a {match-,}lambda form, attempt to return its patterns (or
%%  arglist).  N.B. A guard is appended to its pattern and Patterns is
%%  a list of lists.  A macro definition must have two args, the pattern
%%  and the environment.

-spec get_function_patterns(LambdaForm) -> {Arity,Patterns} when
      LambdaForm :: nonempty_list(),
      Arity      :: non_neg_integer(),
      Patterns   :: nonempty_list({pattern(),guard()}).
-spec get_macro_patterns(LambdaForm) -> Patterns when
      LambdaForm :: nonempty_list(),
      Patterns   :: nonempty_list({pattern(),guard()}).

get_function_patterns([lambda,Args|_]) -> {length(Args),[{Args,[]}]};
get_function_patterns(['match-lambda',[Pat|_]=Cl|Cls]) ->
    {length(Pat),do_function_patterns([Cl|Cls], [])}.

do_function_patterns([[Pat,['when'|Guard]|_]|Cls], Acc) ->
    do_function_patterns(Cls, [{Pat,Guard}|Acc]);
do_function_patterns([[Pat|_]|Cls], Acc) ->
    do_function_patterns(Cls, [{Pat,[]}|Acc]);
do_function_patterns([], Acc) -> reverse(Acc).

get_macro_patterns([lambda,[Args,_Env]|_]) -> [Args];
get_macro_patterns(['match-lambda'|Cls])   -> do_macro_patterns(Cls, []).

do_macro_patterns([[[Pat,_Env],['when'|Guard]|_]|Cls], Acc) ->
    do_macro_patterns(Cls, [{Pat,Guard}|Acc]);
do_macro_patterns([[[Pat,_Env]|_]|Cls], Acc) ->
    do_macro_patterns(Cls, [{Pat,[]}|Acc]);
do_macro_patterns([], Acc) -> reverse(Acc).

%% make_function_doc(Name, Arity, Patterns, Doc, Line) -> doc().
%% make_macro_doc(Name, Patterns, Doc, Line) -> doc().
%%  Convenience constructor for #doc{}, which is defined in src/lfe_doc.hrl.

-spec make_function_doc(Name, Arity, Patterns, Meta, Line) -> doc() when
      Name     :: atom(),
      Arity    :: non_neg_integer(),
      Patterns :: [{[],[]}],
      Meta     :: [any()],
      Line     :: pos_integer().

-spec make_macro_doc(Name, Patterns, Meta, Line) -> doc() when
      Name     :: atom(),
      Patterns :: [{[],[]}],
      Meta     :: [any()],
      Line     :: pos_integer().

make_function_doc(Name, Arity, Patterns, Meta, Line) ->
    Docs = collect_docs(Meta, []),
    #doc{type=function,name={Name,Arity},patterns=Patterns,doc=Docs,line=Line}.

make_macro_doc(Name, Patterns, Meta, Line) ->
    Docs = collect_docs(Meta, []),
    #doc{type=macro,name=Name,patterns=Patterns,doc=Docs,line=Line}.

string_to_binary(Str) when is_list(Str) ->
    unicode:characters_to_binary(Str, utf8, utf8);
string_to_binary(Bin) -> Bin.

%% save_module_docs(Beam, ModDocs, CompInfo) -> Mod.
%%  Add the "LDoc" chunk to a module's .beam binary.

-spec save_module_docs(Beam, Docs, Cinfo) -> {ok,Beam} | {error,Errors} when
      Beam   :: binary(),
      Docs   :: {[doc()],[doc()]},
      Cinfo  :: #cinfo{},
      Errors :: [{_,_,_}].

save_module_docs(Beam, {Mdoc,Fdocs0}, _Ci) ->
    Fdocs1 = exports_attributes(Beam, Fdocs0),
    %% Modified from elixir_module
    LDoc = term_to_binary(#lfe_docs_v1{
                             docs=Fdocs1,
                             moduledoc=format_docs(Mdoc)
                            }),
    {ok,add_beam_chunk(Beam, "LDoc", LDoc)};
save_module_docs(_, _, _) -> {error,[{none,lfe_doc,save_chunk}]}.

%% exports_attributes(Beam, MacFuncDocs) -> MacFuncDocs.
%%  Return the exported macro and function docs seeting exported=true.

exports_attributes(Beam, Fdocs) ->
    Crefs = [exports,attributes],
    {ok,{_,[{exports,Expf},{attributes,Atts}]}} = beam_lib:chunks(Beam, Crefs),
    Expm  = proplists:get_value('export-macro', Atts, []),
    foldl(do_exports(Expf, Expm), [], Fdocs).

%% do_exports(Expf, Expm) -> Fun.
%%  Close over Expf and Expm then return the folding function for
%%  exports/1.  We only included exported functions and macros.  The
%%  export-macro attribute is not necessarily sorted.

do_exports(Expf, Expm) ->
    fun (#doc{type=function,name=FA,doc=Ds}=Doc, Docs) ->
            ?IF(member(FA, Expf),
                [Doc#doc{exported=true,doc=format_docs(Ds)}|Docs],
                Docs);
        (#doc{type=macro,name=M,doc=Ds}=Doc, Docs) ->
            ?IF(member(M, Expm),
                [Doc#doc{exported=true,doc=format_docs(Ds)}|Docs],
                Docs)
    end.

%% add_beam_chunk(Bin, Id, ChunkData) -> Bin.
%%  Add a custom chunk to a .beam binary. Modified from elixir_module.

add_beam_chunk(Bin, Id, ChunkData)
  when is_binary(Bin), is_list(Id), is_binary(ChunkData) ->
    {ok,_,Chunks} = beam_lib:all_chunks(Bin),
    {ok,NewBin}   = beam_lib:build_module([{Id,ChunkData}|Chunks]),
    NewBin.

%% format_docs([DocString]) -> [DocLine].
%%  Take a list of doc strings and generate a list of indented doc
%%  lines. Each doc string is indented separately. Should it be so?

format_docs(Ds) -> lists:flatmap(fun format_doc/1, Ds).

format_doc(D) ->
    %% Split the string into separate lines, also trims trailing blanks.
    Ls = re:split(D, <<"[ \t]*\n">>, [trim]),
    format_doc_lines(Ls).

format_doc_lines([<<>>|Ls0]) ->                 %First line empty
    case skip_empty_lines(Ls0) of               %Skip lines until text
        {_,[L|_]=Ls1} ->
            C = count_spaces(L),                %Use indentation of this line
            format_doc_lines(Ls1, C);
        {_,[]} -> []
    end;
format_doc_lines([L1|Ls0]) ->                   %First line not empty
    case skip_empty_lines(Ls0) of
        {Els,[L|_]=Ls1} ->
            C = count_spaces(L),                %Use indentation of this line
            %% Include first line as is.
            [L1] ++ Els ++ format_doc_lines(Ls1, C);
        {Els,[]} -> [L1|Els]
    end;
format_doc_lines([]) -> [].

format_doc_lines(Ls, C) -> lists:map(fun (L) -> skip_spaces(L, C) end, Ls).

count_spaces(L) ->
    {match,[{_,C}]} = re:run(L, <<"^ *">>, []),
    C.

skip_spaces(<<$\s,L/binary>>, C) when C > 0 ->
    skip_spaces(L, C-1);
skip_spaces(L, _) -> L.                         %C =:= 0 or no space

skip_empty_lines(Ls) -> lists:splitwith(fun (L) -> L =:= <<>> end, Ls).

%% Access functions for the module doc chunk.

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
    case beam_lib:chunks(Bin, ["LDoc"], []) of
        {ok,{_,[{"LDoc",Chunk}]}} ->
            {ok,binary_to_term(Chunk)};
        _ -> {error,docs}                       %Could not find the docs chunk
    end.

%% module_doc(Chunk) -> [binary()].
%% mf_docs(Chunk) -> [MacFuncDoc].
%% mf_doc_type(MacFuncDoc) -> function | macro.
%% function_docs(Chunk) -> [FunctionDoc].
%% macro_docs(Chunk) -> [MacroDoc].

module_doc(#lfe_docs_v1{moduledoc=Moddoc}) -> Moddoc.

mf_docs(#lfe_docs_v1{docs=Docs}) -> Docs.

mf_doc_type(#doc{name={_,_}}) -> function;
mf_doc_type(#doc{name=N}) when is_atom(N) -> macro.

function_docs(Fun, Ar, #lfe_docs_v1{docs=Docs}) ->
    case lists:keysearch({Fun,Ar}, #doc.name, Docs) of
        {value,Fdoc} -> {ok,Fdoc};
        false -> error
    end.

macro_docs(Mac, #lfe_docs_v1{docs=Docs}) ->
    case lists:keysearch(Mac, #doc.name, Docs) of
        {value,Mdoc} -> {ok,Mdoc};
        false -> error
    end.

%% function_name(FunctionDoc) -> Name.
%% function_arity(FunctionDoc) -> Arity.
%% function_line(FunctionDoc) -> LineNo.
%% function_patterns(FunctionDoc) -> [Pattern].
%% function_doc(FunctionDoc) -> [DocString].
%%  Extract fields from a function doc structure.

function_name(#doc{name={Name,_}}) -> Name.
function_arity(#doc{name={_,Ar}}) -> Ar.
function_line(#doc{line=Line}) -> Line.
function_patterns(#doc{name={_,_},patterns=Ps}) -> Ps.
function_doc(#doc{name={_,_},doc=Ds}) -> Ds.

%% macro_name(MacroDoc) -> Name.
%% macro_line(MacroDoc) -> LineNo.
%% macro_patterns(MacroDoc) -> [Pattern].
%% macro_doc(MacroDoc) -> [DocString].
%%  Extract fields from a macr doc structure.

macro_name(#doc{name=Name}) -> Name.
macro_line(#doc{line=Line}) -> Line.
macro_patterns(#doc{name=N,patterns=Ps}) when is_atom(N) -> Ps.
macro_doc(#doc{name=N,doc=Ds}) when is_atom(N) -> Ds.

%%%===================================================================
%%% EUnit tests
%%%===================================================================

-ifdef(EUNIT).
parse_test_() ->
    [ ?QC(<<"A lambda definition is parsed correctly.">>,
          prop_lfe_doc:prop_define_lambda())
    , ?QC(<<"A match-lambda definition is parsed correctly.">>,
          prop_lfe_doc:prop_define_match())
    ].

pprint(_Format, [{Def,_Line}]) -> lfe_io:format(user, "~p\n", [Def]);
pprint(Format, Data) -> lfe_io:format(user, Format, Data).
-endif.
