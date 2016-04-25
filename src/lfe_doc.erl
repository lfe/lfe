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

-export([module/1,function_patterns/1,macro_patterns/1,add_docs_module/1]).

-import(beam_lib, [all_chunks/1,build_module/1,chunks/2]).
-import(lfe_lib, [is_proper_list/1,is_symb_list/1]).
-import(lists, [member/2,filter/2,foldl/3,reverse/1]).
-import(proplists, [delete/2,get_value/3]).

-include("lfe_comp.hrl").
-include("lfe_doc.hrl").

-ifdef(TEST).
%% used by prop_lfe_doc:do_validate/2
-export([string_to_binary/1]).

-include_lib("eunit/include/eunit.hrl").

-define(QC_OPTS, [{on_output,fun pprint/2},{numtests,1000},{max_size,10}]).
-define(QC(T,P), {timeout,30,{T,?_assert(proper:quickcheck(P, ?QC_OPTS))}}).
-endif.

%% Errors
-spec format_error({bad_lambda,Name::atom(),Lambda::list()}) -> string().
format_error({bad_lambda,Name,Lambda}) ->
    lfe_io:format1("bad lambda: ~p\n    ~P", [Name,Lambda,10]).

%% module(Mod) -> Mod | {error,Errors,[]}.
%%  Parse a module's docstrings and populate Mod#module.docs.

-spec module(Mod0) -> Mod1 | {error,Errors,[]} when
      Mod0   :: #module{code::Defs},
      Defs   :: [{Form,Line}],
      Form   :: [_],
      Mod1   :: #module{docs::Docs},
      Line   :: non_neg_integer(),
      Docs   :: [doc()],
      Errors :: nonempty_list({error,Line,Error}),
      Error  :: {bad_lambda,Form}.
module(#module{code=[]}=Mod)   -> Mod#module{docs=[]};
module(#module{code=Defs}=Mod) ->
    Docs = do_module([], Defs),
    Errors = filter(fun (#doc{}) -> false; (_) -> true end, Docs),
    ?IF([] =:= Errors,
        Mod#module{docs=Docs},
        {error,Errors,[]}).

-spec do_module(Docs, Defs) -> Docs | {error,Line,Error} when
      Docs :: [doc()],
      Defs :: [{[_],Line}],
      Line :: non_neg_integer(),
      Error :: {bad_lambda,[_]}.
do_module(Docs, [{['define-function',Name,Body,DocStr],Line}|Defs]) ->
    do_function(Docs, Name, Body, DocStr, Line, Defs);
do_module(Docs, [{['define-macro',Name,Body,DocStr],Line}|Defs]) ->
    do_macro(Docs, Name, Body, DocStr, Line, Defs);
do_module(Docs, [_|Defs]) -> do_module(Docs, Defs);
do_module(Docs, [])       -> Docs.

do_function(Docs, Name, Body, DocStr, Line, Defs) ->
    %% Must get patterns and arity before we can check if excluded.
    case function_patterns(Body) of
	no ->
	    Error = {Line,?MODULE,{bad_lambda,Name,Body}},
	    do_module([Error|Docs], Defs);
	{yes,Arity,Patterns} ->
	    ?IF(exclude(Name, Arity, DocStr),
		do_module(Docs, Defs),
		begin
		    Doc = make_doc(function, Name, Arity,
				   Patterns, DocStr, Line),
		    do_module([Doc|Docs], Defs)
		end)
    end.

do_macro(Docs, Name, Body, DocStr, Line, Defs) ->
    %% We only need the name to check for exclusion.
    ?IF(exclude(Name, DocStr),
	do_module(Docs, Defs),
	case macro_patterns(Body) of
	    no ->
		Error = {Line,?MODULE,{bad_lambda,Name,Body}},
		do_module([Error|Docs], Defs);
	    {yes,Patterns} ->
		Doc = make_doc(macro, Name, 0,
			       Patterns, DocStr, Line),
		do_module([Doc|Docs], Defs)
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

%% macro_patterns(LambdaForm) -> no | {yes,Patterns}.
%% function_patterns(LambdaForm) -> no | {yes,Arity,Patterns}.
%%  Given a {match-,}lambda form, attempt to return its patterns (or
%%  arglist).  N.B. A guard is appended to its pattern and Patterns is
%%  a list of lists.  A macro definition must have 2 args, the pattern
%%  and the environment.

-spec function_patterns(LambdaForm) -> 'no' | {'yes',Arity,Patterns} when
      LambdaForm :: nonempty_list(),
      Arity      :: non_neg_integer(),
      Patterns   :: nonempty_list(pattern()).

function_patterns([lambda,Args|_]) ->
    ?IF(is_symb_list(Args), {yes,length(Args),[Args]}, no);
function_patterns(['match-lambda',[Pat|_]=Cl|Cls]) ->
    do_function_patterns(length(Pat), [], [Cl|Cls]);
function_patterns(_) -> no.

do_function_patterns(N, Acc, [[Pat,['when'|_]=Guard|_]|Cls]) ->
    do_function_patterns(N, [Pat++[Guard]|Acc], Cls);
do_function_patterns(N, Acc, [[Pat|_]|Cls]) ->
    do_function_patterns(N, [Pat|Acc], Cls);
do_function_patterns(N, Acc, []) -> {yes,N,reverse(Acc)}.

-spec macro_patterns(LambdaForm) -> 'no' | {'yes',Patterns} when
      LambdaForm :: nonempty_list(),
      Patterns   :: nonempty_list(pattern()).

macro_patterns([lambda,[Args,_Env]|_]) ->
    {yes,[Args]};
macro_patterns(['match-lambda'|Cls]) ->
    do_macro_patterns([], Cls);
macro_patterns(_) -> no.

do_macro_patterns(Acc, [[[Pat,_Env],['when'|_]=Guard|_]|Cls]) ->
    do_macro_patterns([Pat++[Guard]|Acc], Cls);
do_macro_patterns(Acc, [[[Pat,_Env]|_]|Cls]) ->
    do_macro_patterns([Pat|Acc], Cls);
do_macro_patterns(Acc, []) -> {yes,reverse(Acc)};
do_macro_patterns(_, _) -> no.

%% make_doc(Type, Name, Arity, Patterns, Doc, Line) -> doc().
%%  Convenience constructor for #doc{}, which is defined in src/lfe_doc.hrl.

-spec make_doc(Type, Name, Arity, Patterns, Doc, Line) -> doc() when
      Type     :: 'function' | 'macro',
      Name     :: atom(),
      Arity    :: non_neg_integer(),
      Patterns :: [[]],
      Doc      :: binary() | string(),
      Line     :: pos_integer().
make_doc(Type, Name, Arity, Patterns, Doc, Line) when is_list(Doc) ->
    make_doc(Type, Name, Arity, Patterns, string_to_binary(Doc), Line);
make_doc(Type, Name, Arity, Patterns, Doc, Line) when is_binary(Doc) ->
    #doc{type=Type,name=Name,arity=Arity,patterns=Patterns,doc=Doc,line=Line}.

-spec string_to_binary(string()) -> binary().
string_to_binary(Str) -> unicode:characters_to_binary(Str, utf8, utf8).

%% add_docs_module(Mod) -> Mod.
%%  Add the "LDoc" chunk to a module's .beam binary.

-spec add_docs_module(Mod) -> Mod when
      Mod :: #module{code :: binary(), docs :: [doc()]}.
add_docs_module(#module{}=Mod0) ->
    {ModDoc,#module{code=Bin,docs=Docs}=Mod1} = exports_attributes(Mod0),
    %% Modified from elixir_module
    LDoc = term_to_binary(#lfe_docs_v1{
                             docs=Docs,
                             moduledoc=ModDoc
                             %% callback_docs=CallbackDocs,
                             %% type_docs=TypeDocs
                            }),
    Mod1#module{code=add_beam_chunk(Bin, "LDoc", LDoc)};
add_docs_module(_) -> error.

%% exports_attributes(Mod) -> {ModDoc,Mod}.
%%  Iterate over Mod's 'docs' and set their 'exported' values appropriately.
%%  ModDoc is a given module's 'doc' or <<>>.

-spec exports_attributes(Mod) -> {ModDoc,Mod} when
      Mod    :: #module{},
      ModDoc :: binary().
exports_attributes(#module{name=Name,code=Beam,docs=Docs0}=Mod) ->
    ChunkRefs = [exports,attributes],
    {ok,{Name,[{exports,Expf},{attributes,Attr}]}} = chunks(Beam, ChunkRefs),
    Expm  = get_value('export-macro', Attr, []),
    MDoc  = iolist_to_binary(get_value(doc, Attr, "")),
    Docs1 = foldl(do_exports(Expf, Expm), [], Docs0),
    {MDoc,Mod#module{docs=Docs1}}.

%% do_exports(Expf, Expm) -> Fun.
%%  Close over Expf and Expm then return the folding function for
%%  exports/1.  We only included exported functions and macros.  The
%%  export-macro attribute is not necessarily sorted.
-spec do_exports(Expf, Expm) -> Fun when
      Expf :: [{atom(),non_neg_integer()}],
      Expm :: [atom()],
      Fun  :: fun((doc(), [doc()]) -> [doc()]).

do_exports(Expf, Expm) ->
    fun (#doc{type=function,name=F,arity=A}=Doc, Docs) ->
            ?IF(member({F,A}, Expf),
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

-ifdef(TEST).
parse_test_() ->
    [ ?QC(<<"A lambda definition is parsed correctly.">>,
          prop_lfe_doc:prop_define_lambda())
    , ?QC(<<"A match-lambda definition is parsed correctly.">>,
          prop_lfe_doc:prop_define_match())
    ].

pprint(Format, Data) -> lfe_io:format(user, Format, Data).
-endif.
