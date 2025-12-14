%% Copyright (c) 2008-2026 Robert Virding
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

%% File    : lfe_macro_backquote.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang macro backquote expander.

%% Expand backquotes using an algorithm by André van Tonder.

-module(lfe_macro_backquote).

-include("lfe.hrl").
-include("lfe_comp.hrl").
-include("lfe_macro.hrl").

-export([expand/3]).

%%  By Andr� van Tonder
%%  Unoptimized.  See Dybvig source for optimized version.
%%  Resembles one by Richard Kelsey and Jonathan Rees.
%%   (define-syntax quasiquote
%%     (lambda (s)
%%       (define (qq-expand x level)
%%         (syntax-case x (quasiquote unquote unquote-splicing)
%%           (`x   (quasisyntax (list 'quasiquote
%%                                    #,(qq-expand (syntax x) (+ level 1)))))
%%           (,x (> level 0)
%%                 (quasisyntax (cons 'unquote
%%                                    #,(qq-expand (syntax x) (- level 1)))))
%%           (,@x (> level 0)
%%                 (quasisyntax (cons 'unquote-splicing
%%                                    #,(qq-expand (syntax x) (- level 1)))))
%%           (,x (= level 0)
%%                 (syntax x))
%%           (((unquote x ...) . y)
%%            (= level 0)
%%                 (quasisyntax (append (list x ...)
%%                                      #,(qq-expand (syntax y) 0))))
%%           (((unquote-splicing x ...) . y)
%%            (= level 0)
%%                 (quasisyntax (append (append x ...)
%%                                      #,(qq-expand (syntax y) 0))))
%%           ((x . y)
%%                 (quasisyntax (cons  #,(qq-expand (syntax x) level)
%%                                     #,(qq-expand (syntax y) level))))
%%           (#(x ...)
%%                 (quasisyntax (list->vector #,(qq-expand (syntax (x ...))
%%                                                         level))))
%%           (x    (syntax 'x))))
%%       (syntax-case s ()
%%         ((_ x) (qq-expand (syntax x) 0)))))

%% expand(Exp, Env, MacState) -> Exp.
%%  Not very efficient quasiquote expander, but very compact code.  Is
%%  R6RS compliant and can handle comma (unquote) and comma-at
%%  (unquote-splicing) with more than one argument properly.  Actually
%%  with simple cons/append optimisers code now quite good.

expand(Exp, _Env, St) ->
    {yes,expand(Exp, 0),St}.

expand([backquote,X], N) ->
    [list,[quote,backquote],expand(X, N+1)];
expand([comma|X], N) when N > 0 ->
    exp_bq_cons([quote,comma], expand(X, N-1));
expand([comma,X], 0) -> X;
expand(['comma-at'|X], N) when N > 0 ->
    exp_bq_cons([quote,'comma-at'], expand(X, N-1));
%% Next 2 handle case of splicing into a list.
expand([[comma|X]|Y], 0) ->
    exp_bq_append([list|X], expand(Y, 0));
expand([['comma-at'|X]|Y], 0) ->
    exp_bq_append(['++'|X], expand(Y, 0));
expand([X|Y], N) ->                      %The general list case
    exp_bq_cons(expand(X, N), expand(Y, N));
expand(X, N) when is_tuple(X) ->
    %% Straight [list_to_tuple,expand(tuple_to_list(X), N)]
    %% inefficient and [tuple|tl(expand(tuple_to_list(X), N))]
    %% can't handle splicing!
    case expand(tuple_to_list(X), N) of
        [list|Es] -> [tuple|Es];                %No splicing
        [cons|_]=E -> [list_to_tuple,E];        %Have splicing
        [] -> [tuple]                           %The empty tuple
    end;
expand(X, N) when ?IS_MAP(X) ->
    %% Splicing at top-level almost meaningless here, with [list|...]
    %% we have no splicing, while with [cons|...] we have splicing
    case exp_bq_map_pairs(maps:to_list(X), N) of
        [list|KVs] -> [map|KVs];                %No splicing
        %% [cons|_]=E ->                        %Have splicing
        %%      [call,?Q(maps),?Q(from_list)|E];
        [] -> [map]                             %The empty map
    end;
expand(X, _) when is_atom(X) -> [quote,X];
expand(X, _) -> X.                       %Self quoting

exp_bq_append(['++',L], R) ->                   %Catch single comma-at
    exp_bq_append(L, R);
exp_bq_append([], R) -> R;
exp_bq_append(L, []) -> L;
%% Will these 2 cases move code errors illegally?
exp_bq_append([list,L], [list|R]) -> [list,L|R];
exp_bq_append([list,L], R) -> [cons,L,R];
%%exp_bq_append(['++'|L], R) -> ['++'|L ++ [R]];
%%exp_bq_append(L, ['++'|R]) -> ['++',L|R];
exp_bq_append(L, R) -> ['++',L,R].

exp_bq_cons([quote,L], [quote,R]) -> [quote,[L|R]];
exp_bq_cons(L, [list|R]) -> [list,L|R];
exp_bq_cons(L, []) -> [list,L];
exp_bq_cons(L, R) -> [cons,L,R].

-ifdef(HAS_MAPS).
exp_bq_map_pairs(Ps, N) ->
    KVs = lists:foldr(fun ({K,V}, Acc) -> [K,V|Acc] end, [], Ps),
    expand(KVs, N).
-else.
exp_bq_map_pairs(_, _) -> [list].
-endif.
