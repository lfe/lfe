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
%% Purpose : Lisp Flavoured Erlang documentation parser.

%% The functions herein are used internally by the compiler.
%% There is no guarantee the API will not change dramatically in future.

-module(lfe_doc).

-export([format_error/1]).

-export([module/1,patterns/1,add_docs_module/1]).

-import(beam_lib, [all_chunks/1,build_module/1,chunks/2]).
-import(lfe_lib, [is_proper_list/1,is_symb_list/1]).
-import(lists, [filter/2,foldl/3,reverse/1]).
-import(ordsets, [is_element/2]).
-import(proplists, [delete/2,get_value/3]).

-include("lfe_comp.hrl").
-include("lfe_doc.hrl").

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Skip if exclude/1 is true, otherwise check for bad patterns.
-define(DO_MOD(Docs,Type,Name,Body,DocStr,Line,Defs),
        ?IF(exclude(Name), do_module(Docs, Defs),
            case patterns(Body) of
                no ->
                    Error = {Line,?MODULE,{bad_lambda,Name,Body}},
                    do_module([Error|Docs], Defs);
                {yes,Arity,Patterns} ->
                    ?IF(exclude({Name,Arity}), do_module(Docs, Defs),
                        begin
                            Doc = make_doc(Type, Name, Arity,
                                           Patterns, DocStr, Line),
                            do_module([Doc|Docs], Defs)
                        end)
            end)).

%% Errors
format_error({bad_lambda,Name,Lambda}) ->
    lfe_io:format1("bad lambda: ~p\n    ~P", [Name,Lambda,10]).

-spec module(#module{code::Defs}) -> #module{docs::Docs} | {error,Reason} when
      Defs   :: [{[_],Line}],
      Line   :: non_neg_integer(),
      Docs   :: [doc()],
      Reason :: term().
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
    ?DO_MOD(Docs,function,Name,Body,DocStr,Line,Defs);
do_module(Docs, [{['define-macro',Name,Body,DocStr],Line}|Defs]) ->
    ?DO_MOD(Docs,macro,Name,Body,DocStr,Line,Defs);
do_module(Docs, [_|Defs]) -> do_module(Docs, Defs);
do_module(Docs, [])       -> Docs.

%% exclude(Name | {Name,Arity}) -> boolean().
%%  Return true if a function should be excluded from the docs chunk.

-spec exclude(Name | {Name,Arity}) -> boolean() when
      Name  :: atom(),
      Arity :: non_neg_integer().
exclude({'LFE-EXPAND-EXPORTED-MACRO',3}) -> true;
exclude('MODULE')                        -> true;
exclude(_)                               -> false.

%% patterns(LambdaForm) -> no | {yes,Arity,Patterns}.
%%  Given a {match-,}lambda form, attempt to return its patterns (or arglist).
%%  N.B. Guards are appended to patterns and Patterns is always a list of lists.

-spec patterns(LambdaForm) -> 'no' | {'yes',Arity,Patterns} when
      LambdaForm :: nonempty_list(),
      Arity      :: non_neg_integer(),
      Patterns   :: nonempty_list(pattern()).
patterns([lambda,Args|_]) ->
    ?IF(is_symb_list(Args), {yes,length(Args),[Args]}, no);
patterns(['match-lambda',[[[list|Pat],'$ENV'],['when'|_]=Guard|_]|Cls]) ->
    ?IF(is_proper_list(Pat),
        do_patterns(length(Pat), [Pat++[Guard]], Cls),
        do_patterns(255, [Pat], Cls));
patterns(['match-lambda',[[Pat,'$ENV'],['when'|_]=Guard|_]|Cls]) ->
    ?IF(is_proper_list(Pat),
        do_patterns(length(Pat), [Pat++[Guard]], Cls),
        do_patterns(255, [Pat], Cls));
patterns(['match-lambda',[[[list|Pat],'$ENV']|_]|Cls]) ->
    ?IF(is_proper_list(Pat), do_patterns(length(Pat), [Pat], Cls), no);
patterns(['match-lambda',[[Pat,'$ENV']|_]|Cls]) ->
    ?IF(is_proper_list(Pat),
        do_patterns(length(Pat), [Pat], Cls),
        do_patterns(255, [Pat], Cls));
patterns(['match-lambda',[Pat,['when'|_]=Guard|_]|Cls]) ->
    ?IF(is_proper_list(Pat), do_patterns(length(Pat), [Pat++[Guard]], Cls), no);
patterns(['match-lambda',[Pat|_]|Cls]) ->
    ?IF(is_proper_list(Pat), do_patterns(length(Pat), [Pat], Cls), no);
patterns(_) -> no.

do_patterns(N, Acc, [[[[list|Pat],'$ENV'],['when'|_]=Guard|_]|Cls]) ->
    ?IF(is_proper_list(Pat),
        do_patterns(?IF(N =:= length(Pat), N, 255), [Pat++[Guard]|Acc], Cls),
        do_patterns(255, [Pat++[Guard]|Acc], Cls));
do_patterns(N, Acc, [[[Pat,'$ENV'],['when'|_]=Guard|_]|Cls]) ->
    ?IF(is_proper_list(Pat),
        do_patterns(?IF(N =:= length(Pat), N, 255), [Pat++[Guard]|Acc], Cls),
        do_patterns(255, [Pat++[Guard]|Acc], Cls));
do_patterns(N, Acc, [[[[list|Pat],'$ENV']|_]|Cls]) ->
    ?IF(is_proper_list(Pat),
        do_patterns(?IF(N =:= length(Pat), N, 255), [Pat|Acc], Cls),
        do_patterns(255, [Pat|Acc], Cls));
do_patterns(N, Acc, [[[Pat,'$ENV']|_]|Cls]) ->
    ?IF(is_proper_list(Pat),
        do_patterns(?IF(N =:= length(Pat), N, 255), [Pat|Acc], Cls),
        do_patterns(255, [Pat|Acc], Cls));
do_patterns(N, Acc, [[Pat,['when'|_]=Guard|_]|Cls]) ->
    ?IF(is_proper_list(Pat),
        do_patterns(?IF(N =:= length(Pat), N, 255), [Pat++[Guard]|Acc], Cls),
        no);
do_patterns(N, Acc, [[Pat|_]|Cls]) ->
    ?IF(is_proper_list(Pat),
        do_patterns(?IF(N =:= length(Pat), N, 255), [Pat|Acc], Cls),
        no);
do_patterns(N, Acc, []) -> {yes,N,reverse(Acc)};
do_patterns(_, _, _) -> no.

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

string_to_binary(Str) -> unicode:characters_to_binary(Str, utf8, utf8).

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
%%  ModDoc is a given module's 'doc' or <<"">>.

-spec exports_attributes(Mod) -> Mod when
      Mod :: #module{}.
exports_attributes(#module{name=Name,code=Beam,docs=Docs0}=Mod) ->
    ChunkRefs = [exports,attributes],
    {ok,{Name,[{exports,Expt},{attributes,Attr}]}} = chunks(Beam, ChunkRefs),
    Expm  = get_value('export-macro', Attr, []),
    MDoc  = iolist_to_binary(get_value(doc, Attr, "")),
    Docs1 = foldl(do_exports(Expt, Expm), [], Docs0),
    {MDoc,Mod#module{docs=Docs1}}.

%% do_exports(Expt, Expm) -> Fun.
%%  Close over Expt and Expm then return the folding function for exports/1.

-spec do_exports(Expt, Expm) -> Fun when
      Expt :: [{atom(),non_neg_integer()}],
      Expm :: [atom()],
      Fun  :: fun((doc(), [doc()]) -> [doc()]).
do_exports(Expt, Expm) ->
    fun (#doc{type=function,name=F,arity=A}=Doc, Docs) ->
            [Doc#doc{exported=is_element({F,A}, Expt)}|Docs];
        (#doc{type=macro,name=M}=Doc, Docs) ->
            [Doc#doc{exported=is_element(M, Expm)}|Docs]
    end.

%% Modified from elixir_module: Adds custom chunk to a .beam binary
add_beam_chunk(Bin, Id, ChunkData)
  when is_binary(Bin), is_list(Id), is_binary(ChunkData) ->
    {ok,_,Chunks} = all_chunks(Bin),
    {ok,NewBin}   = build_module([{Id,ChunkData}|Chunks]),
    NewBin.


%%%===================================================================
%%% Property-based tests
%%%===================================================================

-ifdef(TEST).
-define(QC_OPTS, [{on_output,fun pprint/2},{numtests,1000},{max_size,10}]).
-define(QC(T,P), {timeout,30,{T,?_assert(proper:quickcheck(P, ?QC_OPTS))}}).


%%%===================================================================
%%% EUnit tests
%%%===================================================================

parse_test_() ->
    [ ?QC(<<"A lambda definition is parsed correctly.">>,
          prop_define_lambda())
    , ?QC(<<"A match-lambda definition is parsed correctly.">>,
          prop_define_match())
    ].

%%%===================================================================
%%% Properties
%%%===================================================================

prop_define_lambda() -> ?FORALL(Def, define_lambda(), validate(Def)).

prop_define_match() -> ?FORALL(Def, define_match(), validate(Def)).

validate({[_,_,[lambda,Args|_],_],_}=Def) -> do_validate(length(Args), Def);
validate({[_,_,['match-lambda',[Patt|_]|_],_],_}=Def) ->
    do_validate(length(Patt), Def).

do_validate(Arity,{[Define,Name,_,DocStr],Line}=Def) ->
    Type  = define_to_type(Define),
    Mod   = #module{code=[Def]},
    #module{docs=[#doc{type=Type,
                       exported=false,
                       name=Name,
                       arity=Arity,
                       %% patterns=_,
                       doc=Doc,
                       line=Line
                      }]} = module(Mod),
    string_to_binary(DocStr) =:= Doc.

define_to_type('define-function') -> function;
define_to_type('define-macro')    -> macro.


%%%===================================================================
%%% Definition shapes
%%%===================================================================

define_lambda() -> {[define(),atom1(),lambda(),docstring()],line()}.

define_match()  -> {[define(),atom1(),'match-lambda'(),docstring()],line()}.


%%%===================================================================
%%% Custom types
%%%===================================================================

%%% Definitions

define() -> oneof(['define-function','define-macro']).

lambda() -> [lambda,arglist_simple()|body()].

'match-lambda'() -> ['match-lambda'|non_empty(list(pattern_clause()))].

arglist_simple() -> list(atom1()).

atom1() -> oneof([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,'']).

body() -> non_empty(list(form())).

form() -> union([form_elem(),[atom1()|list(form_elem())]]).

form_elem() -> union([non_string_term(),printable_string(),atom1()]).

docstring() -> printable_string().

line() -> pos_integer().


%%% Patterns

pattern() -> union([non_string_term(),printable_string(),pattern_form()]).

pattern_form() ->
    [oneof(['=','++*',[],
            backquote,quote,
            binary,cons,list,map,tuple,
            match_fun()])
     | body()].

%% Don't waste atoms, since we're already running out.
%% match_fun() -> ?LET(F, printable_string(), list_to_atom("match-" ++ F)).
match_fun() -> 'match-record'.

pattern_clause() -> pattern_clause(random:uniform(10)).

pattern_clause(Arity) ->
    [arglist_patterns(Arity)|[oneof([guard(),form()])|body()]].

arglist_patterns(Arity) -> vector(Arity, pattern()).

guard() -> ['when'|non_empty(list(union([logical_clause(),comparison()])))].


%%% Logical clauses

logical_clause() ->
    X = union([atom1(),comparison()]),
    [logical_operator(),X|non_empty(list(X))].

logical_operator() -> oneof(['and','andalso','or','orelse']).


%%% Comparisons

comparison() -> [comparison_operator(),atom1()|list(atom1())].

comparison_operator() -> oneof(['==','=:=','=/=','<','>','=<','>=']).


%%% Strings and non-strings

non_string_term() ->
    union([atom1(),number(),[],bitstring(),binary(),boolean(),tuple()]).

printable_char() -> union([integer(32, 126),integer(160, 255)]).

printable_string() -> list(printable_char()).


%%%===================================================================
%%% Internal functions
%%%===================================================================

pprint(Format, Data) -> lfe_io:format(user, Format, Data).

-endif.
