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

-include_lib("proper/include/proper.hrl").

%%%===================================================================
%%% Properties
%%%===================================================================

prop_define_lambda() -> ?FORALL(Def, define_lambda(), validate(Def)).

prop_define_match() -> ?FORALL(Def, define_match(), validate(Def)).

validate({['define-function',Name,_Doc,Def],_}=Func) ->
    validate_function(Name, function_arity(Def), Func);
validate({['define-macro',Name,_Doc,_Def],_}=Mac) ->
    validate_macro(Name, Mac).

function_arity([lambda,Args|_]) -> length(Args);
function_arity(['match-lambda',[Pat|_]|_]) -> length(Pat).

validate_function(Name, Arity, {[_Define,_Name,Meta,_Def],Line}=Func) ->
    case lfe_doc:extract_module_docs([Func]) of
        {ok,{[],[Fdoc]}} ->
            (lfe_doc:collect_docs(Meta, []) =:= lfe_doc:function_doc(Fdoc))
                and (Name =:= lfe_doc:function_name(Fdoc))
                and (Arity =:= lfe_doc:function_arity(Fdoc))
                and (Line =:= lfe_doc:function_line(Fdoc));
        _ -> false
    end.

validate_macro(Name, {[_Define,_Name,Meta,_Lambda],Line}=Mac) ->
    case lfe_doc:extract_module_docs([Mac]) of
        {ok,{[],[Mdoc]}} ->
            (lfe_doc:collect_docs(Meta, []) =:= lfe_doc:macro_doc(Mdoc))
                and (Name =:= lfe_doc:macro_name(Mdoc))
                and (Line =:= lfe_doc:macro_line(Mdoc));
        _ -> false
    end.

%%%===================================================================
%%% Definition shapes
%%%===================================================================

define_lambda() ->
    {['define-function',atom1(),meta_with_doc(),lambda()],line()}.

define_match() ->
    ?LET(D, define(), {[D,atom1(),meta_with_doc(),'match-lambda'(D)],line()}).


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

meta_with_doc() -> [[doc,docstring()]].

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

macro_pattern_clause() -> pattern_clause(rand_arity(), true).

function_pattern_clause() -> pattern_clause(rand_arity(), false).

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


%%% Rand compat

-ifdef(NEW_RAND).
rand_arity() -> rand:uniform(10).
-else.
rand_arity() -> random:uniform(10).
-endif.
