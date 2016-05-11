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

%% The functions herein are used internally by the compiler.
%% There is no guarantee the API will not change dramatically in future.

-module(lfe_doc).

-export([format_error/1]).

-export([module/2,function_patterns/1,macro_patterns/1,add_docs_module/2]).

-import(beam_lib, [all_chunks/1,build_module/1,chunks/2]).
-import(lists, [member/2,filter/2,foldl/3,reverse/1]).
-import(proplists, [delete/2,get_value/3]).

-include("lfe_comp.hrl").
-include("lfe_doc.hrl").

-ifdef(EUNIT).
-export([string_to_binary/1, pprint/2]).        %Used by prop_lfe_doc

-include_lib("eunit/include/eunit.hrl").

-define(QC_OPTS, [{on_output,fun pprint/2},{numtests,1000},{max_size,10}]).
-define(QC(T,P), {timeout,30,{T,?_assert(proper:quickcheck(P, ?QC_OPTS))}}).
-endif.

%% Errors
-spec format_error({bad_lambda,Name::atom(),Lambda::list()}) -> string().
format_error({bad_lambda,Name,Lambda}) ->
    lfe_io:format1("bad lambda: ~p\n    ~P", [Name,Lambda,10]).

%% module(Defs, CompInfo) -> {ok,Docs} | {error,Errors,[]}.
%%  Parse a module's docstrings and return the docs.

-spec module(Defs, Cinfo) -> {ok,Docs} | {error,Errors,[]} when
      Defs   :: [{Form,Line}],
      Form   :: [_],
      Line   :: non_neg_integer(),
      Cinfo  :: #cinfo{},
      Docs   :: [doc()],
      Errors :: nonempty_list({error,Line,Error}),
      Error  :: {bad_lambda,Form}.

module([], _Ci)   -> {ok,[]};
module(Defs, _Ci) ->
    {Mdoc,Docs} = do_forms([], [], Defs),
    Errors = filter(fun (#doc{}) -> false;
                        (_) -> true
                    end, Docs),
    ?IF([] =:= Errors,
        {ok,{Mdoc,Docs}},
        {error,Errors,[]}).

do_forms(Mdoc0, Docs, [{['define-module',_|Mdef],_}|Defs]) ->
    Mdoc1 = do_module_def(Mdef, Mdoc0),
    do_forms(Mdoc1, Docs, Defs);
do_forms(Mdoc0, Docs, [{['extend-module'|Mdef],_}|Defs]) ->
    Mdoc1 = do_module_def(Mdef, Mdoc0),
    do_forms(Mdoc1, Docs, Defs);
do_forms(Mdoc, Docs0, [{['define-function',Name,DocStr,Body],Line}|Defs]) ->
    Docs1 = do_function(Docs0, Name, Body, DocStr, Line),
    do_forms(Mdoc, Docs1, Defs);
do_forms(Mdoc, Docs0, [{['define-macro',Name,DocStr,Body],Line}|Defs]) ->
    Docs1 = do_macro(Docs0, Name, Body, DocStr, Line),
    do_forms(Mdoc, Docs1, Defs);
do_forms(Mdoc, Docs, [_|Defs]) -> do_forms(Mdoc, Docs, Defs);
do_forms(Mdoc, Docs, [])       -> {Mdoc,Docs}.

do_module_def([Doc|As]=Mdef, Mdoc0) ->
    case lfe_lib:is_doc_string(Doc) of
	true ->
	    Mdoc1 = Mdoc0 ++ [string_to_binary(Doc)],
	    collect_mdocs(As, Mdoc1);
	false -> collect_mdocs(Mdef, Mdoc0)
    end.

collect_mdocs(As, Mdoc) ->
    %% Collect all the docs in all the doc attributes.
    Fun = fun ([doc|Docs], Md) ->
		  foldl(fun (D, M) -> M ++ [string_to_binary(D)] end,
			Md, Docs);
	      (_, Md) -> Md
	  end,
    foldl(Fun, Mdoc, As).

do_function(Docs, Name, Body, DocStr, Line) ->
    %% Must get patterns and arity before we can check if excluded.
    case function_patterns(Body) of
        no ->
            Error = {Line,?MODULE,{bad_lambda,Name,Body}},
            [Error|Docs];
        {yes,Arity,Patterns} ->
            ?IF(exclude(Name, Arity, DocStr),
                Docs,
                begin
                    Doc = make_doc(function, {Name,Arity},
                                   Patterns, DocStr, Line),
                    [Doc|Docs]
                end)
    end.

do_macro(Docs, Name, Body, DocStr, Line) ->
    %% We only need the name to check for exclusion.
    ?IF(exclude(Name, DocStr),
        Docs,
        case macro_patterns(Body) of
            no ->
                Error = {Line,?MODULE,{bad_lambda,Name,Body}},
                [Error|Docs];
            {yes,Patterns} ->
                Doc = make_doc(macro, Name, Patterns, DocStr, Line),
                [Doc|Docs]
        end).

%% exclude(Name, Arity, DocStr) -> boolean().
%% exclude(Name, DocStr) -> boolean().
%%  Return true if a function should be excluded from the docs chunk.
%%  $handle_undefined_function/2 needs special handling as it is
%%  automatically generated but can also be defined by the user. So we
%%  only include it is it has user documentation.

-spec exclude(Name, Arity, DocStr) -> boolean() when
      Name  :: atom(),
      Arity :: non_neg_integer(),
      DocStr :: binary() | string().
-spec exclude(Name, DocStr) -> boolean() when
      Name  :: atom(),
      DocStr :: binary() | string().

exclude('LFE-EXPAND-EXPORTED-MACRO', 3, _) -> true;
exclude('$handle_undefined_function', 2, DS) ->
    (DS == []) or (DS == <<"">>);
exclude(_, _, _) -> false.

exclude('MODULE', _)                        -> true;
exclude(_, _)                               -> false.

%% function_patterns(LambdaForm) -> no | {yes,Arity,Patterns}.
%% macro_patterns(LambdaForm) -> no | {yes,Patterns}.
%%  Given a {match-,}lambda form, attempt to return its patterns (or
%%  arglist).  N.B. A guard is appended to its pattern and Patterns is
%%  a list of lists.  A macro definition must have 2 args, the pattern
%%  and the environment.

-spec function_patterns(LambdaForm) -> 'no' | {'yes',Arity,Patterns} when
      LambdaForm :: nonempty_list(),
      Arity      :: non_neg_integer(),
      Patterns   :: nonempty_list(pattern()).
-spec macro_patterns(LambdaForm) -> 'no' | {'yes',Patterns} when
      LambdaForm :: nonempty_list(),
      Patterns   :: nonempty_list(pattern()).

function_patterns([lambda,Args|_]) ->
    {yes,length(Args),[Args]};
function_patterns(['match-lambda',[Pat|_]=Cl|Cls]) ->
    do_function_patterns(length(Pat), [], [Cl|Cls]);
function_patterns(_) -> no.

do_function_patterns(N, Acc, [[Pat,['when'|_]=Guard|_]|Cls]) ->
    do_function_patterns(N, [Pat++[Guard]|Acc], Cls);
do_function_patterns(N, Acc, [[Pat|_]|Cls]) ->
    do_function_patterns(N, [Pat|Acc], Cls);
do_function_patterns(N, Acc, []) -> {yes,N,reverse(Acc)}.

macro_patterns([lambda,[Args,_Env]|_]) -> {yes,[Args]};
macro_patterns(['match-lambda'|Cls])   -> do_macro_patterns([], Cls);
macro_patterns(_)                      -> no.

do_macro_patterns(Acc, [[[Pat,_Env],['when'|_]=Guard|_]|Cls]) ->
    do_macro_patterns([Pat++[Guard]|Acc], Cls);
do_macro_patterns(Acc, [[[Pat,_Env]|_]|Cls]) ->
    do_macro_patterns([Pat|Acc], Cls);
do_macro_patterns(Acc, []) -> {yes,reverse(Acc)};
do_macro_patterns(_, _)    -> no.

%% make_doc(Type, Name, Arity, Patterns, Doc, Line) -> doc().
%%  Convenience constructor for #doc{}, which is defined in src/lfe_doc.hrl.

-spec make_doc(Type, Name, Patterns, Doc, Line) -> doc() when
      Type     :: function | macro,
      Name     :: name(),
      Patterns :: [[]],
      Doc      :: binary() | string(),
      Line     :: pos_integer().

make_doc(Type, Name, Patterns, Doc, Line) ->
    Bdoc = string_to_binary(Doc),
    #doc{type=Type,name=Name,patterns=Patterns,doc=Bdoc,line=Line}.

string_to_binary(Str) when is_list(Str) ->
    unicode:characters_to_binary(Str, utf8, utf8);
string_to_binary(Bin) -> Bin.

%% add_docs_module(Mod, CompInfo) -> Mod.
%%  Add the "LDoc" chunk to a module's .beam binary.

-spec add_docs_module(Mod, Cinfo) -> Mod | error when
      Mod   :: #module{code :: binary(), docs :: [doc()]},
      Cinfo :: #cinfo{}.

add_docs_module(#module{code=Beam,docs={Mdoc0,Fdocs0}}=Mod, _Ci) ->
    Mdoc1 = [ D || D <- Mdoc0, D =/= <<>> ],
    Fdocs1 = exports_attributes(Beam, Fdocs0),
    %% Modified from elixir_module
    LDoc = term_to_binary(#lfe_docs_v1{
                             docs=Fdocs1,
                             moduledoc=Mdoc1
                             %% callback_docs=CallbackDocs,
                             %% type_docs=TypeDocs
                            }),
    Mod#module{code=add_beam_chunk(Beam, "LDoc", LDoc),docs={Mdoc1,Fdocs1}};
add_docs_module(_, _) -> error.

%% exports_attributes(Mod) -> {ModDoc,Mod}.
%%  Iterate over Mod's 'docs' and set their 'exported' values appropriately.
%%  ModDoc is a given module's 'doc' or <<>>.

-spec exports_attributes(Beam, Fdocs) -> {ModDoc,Mod} when
      Beam   :: binary(),
      Fdocs  :: [doc()],
      ModDoc :: [binary()],
      Mod    :: [doc()].

exports_attributes(Beam, Fdocs) ->
    ChunkRefs = [exports,attributes],
    {ok,{_,[{exports,Expf},{attributes,Atts}]}} = chunks(Beam, ChunkRefs),
    Expm  = get_value('export-macro', Atts, []),
    foldl(do_exports(Expf, Expm), [], Fdocs).

%% do_exports(Expf, Expm) -> Fun.
%%  Close over Expf and Expm then return the folding function for
%%  exports/1.  We only included exported functions and macros.  The
%%  export-macro attribute is not necessarily sorted.

-spec do_exports(Expf, Expm) -> Fun when
      Expf :: [{atom(),non_neg_integer()}],
      Expm :: [atom()],
      Fun  :: fun((doc(), [doc()]) -> [doc()]).

do_exports(Expf, Expm) ->
    fun (#doc{type=function,name=FA}=Doc, Docs) ->
            ?IF(member(FA, Expf),
                [Doc#doc{exported=true}|Docs],
                Docs);
        (#doc{type=macro,name=M}=Doc, Docs) ->
            ?IF(member(M, Expm),
                [Doc#doc{exported=true}|Docs],
                Docs)
    end.

%% add_beam_chunk(Bin, Id, ChunkData) -> Bin.
%%  Add a custom chunk to a .beam binary. Modified from elixir_module.

-spec add_beam_chunk(Bin, Id, ChunkData) -> Bin when
      Bin       :: binary(),
      Id        :: string(),
      ChunkData :: binary().

add_beam_chunk(Bin, Id, ChunkData)
  when is_binary(Bin), is_list(Id), is_binary(ChunkData) ->
    {ok,_,Chunks} = all_chunks(Bin),
    {ok,NewBin}   = build_module([{Id,ChunkData}|Chunks]),
    NewBin.


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

pprint(Format, [{Def,_Line}]) -> lfe_io:format(user, "~p\n", [Def]);
pprint(Format, Data) -> lfe_io:format(user, Format, Data).
-endif.
