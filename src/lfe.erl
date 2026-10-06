%% Copyright (c) 2016-2026 Robert Virding
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

%% File    : lfe.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang standard library.

-module(lfe).

-moduledoc("""
Lisp Flavoured Erlang standard library.
""").

%% Standard lisp library.
-export([acons/3,pairlis/2,pairlis/3,
         assoc/2,'assoc-if'/2,'assoc-if-not'/2,
         rassoc/2,'rassoc-if'/2,'rassoc-if-not'/2,
         subst/3,'subst-if'/3,'subst-if-not'/3,sublis/2,
         eval/1,eval/2,
         'macro-function'/1,'macro-function'/2,
         macroexpand/1,macroexpand/2,
         'macroexpand-1'/1,'macroexpand-1'/2,
         'macroexpand-all'/1,'macroexpand-all'/2]).

-export(['LFE-EXPAND-EXPORTED-MACRO'/3]).

%% Metadata
-export([version/0]).

%% 'LFE-EXPAND-EXPORTED-MACRO'(Name, Args, Env) -> {yes,Expansion} | no.
%%  A smart implementation of this where we call the lfe_macro module
%%  to expand and check for us. Using an empty environment ensures
%%  only predefined macros are used.

'LFE-EXPAND-EXPORTED-MACRO'(Name, Args, _) ->
    lfe_macro:expand_expr([Name|Args], lfe_env:new()).

%% Standard lisp library functions.

%% Association list functions.

acons(Key, Value, Alist) -> [[Key|Value]|Alist].

pairlis(Keys, Values) -> pairlis(Keys, Values, []).

pairlis([Key|Keys], [Value|Values], Alist) ->
    [[Key|Value]|pairlis(Keys, Values, Alist)];
pairlis([], [], Alist) -> Alist.

assoc(Key, [[Key|_]=Pair|_]) -> Pair;
assoc(Key, [_|L]) -> assoc(Key, L);
assoc(_, []) -> [].

'assoc-if'(Pred, [[Key|_]=Pair|L]) ->
    case Pred(Key) of
        true -> Pair;
        false -> 'assoc-if'(Pred, L)
    end;
'assoc-if'(_, []) -> [].

'assoc-if-not'(Pred, [[Key|_]=Pair|L]) ->
    case Pred(Key) of
        false -> Pair;
        true -> 'assoc-if-not'(Pred, L)
    end;
'assoc-if-not'(_, []) -> [].

rassoc(Value, [[_|Value]=Pair|_]) -> Pair;
rassoc(Value, [_|L]) -> rassoc(Value, L);
rassoc(_, []) -> [].

'rassoc-if'(Pred, [[_|Value]=Pair|L]) ->
    case Pred(Value) of
        true -> Pair;
        false -> 'rassoc-if'(Pred, L)
    end;
'rassoc-if'(_, []) -> [].

'rassoc-if-not'(Pred, [[_|Value]=Pair|L]) ->
    case Pred(Value) of
        false -> Pair;
        true -> 'rassoc-if-not'(Pred, L)
    end;
'rassoc-if-not'(_, []) -> [].

%% subst(New, Old, Tree) -> Tree.

subst(New, Old, Old) -> New;
subst(New, Old, [H|T]) ->
    [subst(New, Old,H)|subst(New, Old, T)];
subst(_, _, Tree) -> Tree.

%% subst-if(New, Test, Tree) -> Tree.

'subst-if'(New, Test, Tree) ->
    case Test(Tree) of
        true -> New;
        false ->
            case Tree of
                [H|T] ->
                    ['subst-if'(New, Test, H)|'subst-if'(New, Test, T)];
                _ -> Tree
            end
    end.

%% subst-if-not(New, Test, Tree) -> Tree.

'subst-if-not'(New, Test, Tree) ->
    case Test(Tree) of
        false -> New;
        true ->
            case Tree of
                [H|T] ->
                    ['subst-if-not'(New, Test, H)|'subst-if-not'(New, Test, T)];
                _ -> Tree
            end
    end.

%% sublis(AList, Tree) -> Tree.

sublis(Alist, Tree) ->
    case assoc(Tree, Alist) of
        [_|New] -> New;                         %Found it
        [] ->                                   %Not there
            case Tree of
                [H|T] ->
                    [sublis(Alist, H)|sublis(Alist, T)];
                _ -> Tree
            end
    end.

%%  eval(Sexpr) -> Value.
%%  macro-function(Name [,Environment]) -> Macro | [].
%%  macroexpand(Form [,Environment]) -> Expansion | Form.
%%  macroexpand-1(Form [,Environment]) -> Expansion | Form.
%%  macroexpand-all(Form [,Environment]) -> Expansion | Form.

eval(Sexpr) -> eval(Sexpr, lfe_env:new()).      %Empty environment.
eval(Sexpr, Env) -> lfe_eval:expr(Sexpr, Env).

'macro-function'(Symb) -> 'macro-function'(Symb, lfe_env:new()).
'macro-function'(Symb, Env) ->
    case lfe_env:get_mbinding(Symb, Env) of
        {yes,Macro} ->
            Macro;
        no -> []
    end.

macroexpand(Form) -> macroexpand(Form, lfe_env:new()).
macroexpand(Form, Env) ->
    case lfe_macro:expand_expr(Form, Env) of
        {yes,Exp} -> Exp;
        no -> Form
    end.

'macroexpand-1'(Form) -> 'macroexpand-1'(Form, lfe_env:new()).
'macroexpand-1'(Form, Env) ->
    case lfe_macro:expand_expr_1(Form, Env) of
        {yes,Exp} -> Exp;
        no -> Form
    end.

'macroexpand-all'(Form) -> 'macroexpand-all'(Form, lfe_env:new()).
'macroexpand-all'(Form, Env) -> lfe_macro:expand_expr_all(Form, Env).

%% Metadata

version() ->
    {ok, [App]} = file:consult(code:where_is_file("lfe.app")),
    proplists:get_value(vsn, element(3, App)).
