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

%% File    : prop_lfe_doc.erl
%% Author  : Eric Bailey
%% Purpose : PropEr tests for the lfe_doc module.

-module(prop_lfe_doc).

-export([prop_define_lambda/0,prop_define_match/0]).

-import(lfe_doc, [module/2,string_to_binary/1]).

-include_lib("lfe/src/lfe_comp.hrl").
-include_lib("lfe/src/lfe_doc.hrl").
-include_lib("proper/include/proper.hrl").

%%%===================================================================
%%% Properties
%%%===================================================================

prop_define_lambda() -> ?FORALL(Def, define_lambda(), validate(Def)).

prop_define_match() -> ?FORALL(Def, define_match(), validate(Def)).

validate({['define-function',Name,_Doc,[lambda,Args|_]],_}=Def) ->
    do_validate({Name,arity([Args])}, Def);
validate({['define-function',Name,_Doc,['match-lambda',[Pat|_]|_]],_}=Def) ->
    do_validate({Name,arity([Pat])}, Def);
validate({['define-macro',Name,_Doc,['match-lambda'|_]],_}=Def) ->
    do_validate(Name, Def).

do_validate(Name,{[Define,_Name,DocStr,_Lambda],Line}=Def) ->
    Type = define_to_type(Define),
    case module([Def], #cinfo{}) of
        {ok,{[],[#doc{type=Type,name=Name,doc=Doc,line=Line}=Res]}} ->
            string_to_binary(DocStr) =:= Doc;
        _ ->
            false
    end.

define_to_type('define-function') -> function;
define_to_type('define-macro')    -> macro.


%%%===================================================================
%%% Definition shapes
%%%===================================================================

define_lambda() -> {['define-function',atom1(),docstring(),lambda()],line()}.

define_match() ->
    ?LET(D, define(), {[D,atom1(),docstring(),'match-lambda'(D)],line()}).


%%%===================================================================
%%% Custom types
%%%===================================================================

%%% Definitions

define() -> oneof(['define-function','define-macro']).

lambda() -> [lambda,arglist_simple()|body()].

'match-lambda'('define-function') ->
    ['match-lambda'|non_empty(list(function_pattern_clause()))];
'match-lambda'('define-macro') ->
    ['match-lambda'|non_empty(list(macro_pattern_clause()))].

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

match_fun() -> 'match-record'.

macro_pattern_clause() -> pattern_clause(random:uniform(10), true).

function_pattern_clause() -> pattern_clause(random:uniform(10), false).

pattern_clause(Arity, Macro) ->
    [arglist_patterns(Arity, Macro)|[oneof([guard(),form()])|body()]].

arglist_patterns(Arity, false) -> vector(Arity, pattern());
arglist_patterns(Arity, true)  -> [vector(Arity, pattern()),'$ENV'].

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


%%% Helper functions

-spec arity([pattern()]) -> non_neg_integer().
arity([[_|Args]|_])      -> do_arity(1, Args);
arity([[]|_])            -> 0.

-spec do_arity(non_neg_integer(), [pattern()]) -> non_neg_integer().
do_arity(N, [['when'|_]]) -> N;
do_arity(N, [_|Args])     -> do_arity(N+1, Args);
do_arity(N, [])           -> N.
