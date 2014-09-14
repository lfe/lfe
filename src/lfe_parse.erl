%% Copyright (c) 2008-2013 Robert Virding
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

%% File    : lfe_parse.erl
%% Author  : Robert Virding
%% Purpose : A simple Sexpr parser.
%% A simple sexpr parser. It is both re-entrant and returns excess
%% tokens. The main engine is pretty naively coded at the moment.

-module(lfe_parse).

-export([sexpr/1,sexpr/2,format_error/1]).

-import(lists, [reverse/1,reverse/2]).

%% Define IS_MAP/1 macro for is_map/1 bif.
-ifdef(HAS_MAPS).
-define(IS_MAP(T), is_map(T)).
-else.
-define(IS_MAP(T), false).
-endif.

%% We define the syntax as an LL(1) and write/generate a parser for
%% it. We also define the grammar with the same form as for yecc even
%% though we have no automatic generator.
%%
%% Terminals
%%    symbol number string fun '(' ')' '[' ']' '.' '\'' ',' '@' ',@' '`' '#('
%%       '#B(' '#M(' '#\''.
%%
%% Nonterminals form sexpr list list_tail proper_list .
%%
%%  0 form -> sexpr : '$1'.
%%  1 sexpr -> symbol : val('$1').
%%  2 sexpr -> number : val('$1').
%%  3 sexpr -> string : val('$1').
%%  4 sexpr -> '#\'' : make_fun(val('$1')).
%%  5 sexpr -> '\'' sexpr : [quote,'$2'].
%%  6 sexpr -> '`' sexpr : [backquote,'$2'].
%%  7 sexpr -> ',' sexpr : [unquote,'$2'].
%%  8 sexpr -> ',@' sexpr : ['unquote-splicing','$2'].
%%  9 sexpr -> ( list ) : '$2'.
%% 10 sexpr -> [ list ] : '$2'.
%% 11 sexpr -> '#(' proper_list ')' : list_to_tuple('$2').
%% 12 sexpr -> '#B(' proper_list ')' :
%%        case catch lfe_eval:expr([binary|'$2']) of
%%            Bin when is_bitstring(Bin) -> Bin;
%%            _ -> return_error(line('$1'))
%%        end
%% 13 sexpr -> '#M(' proper_list ')' :
%%        case catch maps:from_list(pair_list('$2')) of
%%            Map when is_map(Map) -> Map;
%%            _ -> return_error(line('$1'))
%%        end
%% 14 list -> sexpr list_tail : ['$1'|'$2].
%% 15 list -> empty : [].
%% 16 list_tail -> sexpr list_tail : ['$1'|'$2'].
%% 17 list_tail -> . sexpr : '$2'.
%% 18 list_tail -> empty : [].
%% 19 proper_list -> sexpr proper_list : ['$1'|'$2'].
%% 20 proper_list -> empty : [].

%% The computed First and Follow sets for the productions. This is the
%% only really tricky bit.
%%
%% First(f) = {symbol number string #' ( [ ' ` , ,@ #( #B( #M(}
%% First(s) = {symbol number string #' ( [ ' ` , ,@ #( #B( #M(}
%% First(l) = {symbol number string #' ( [ ' ` , ,@ #( #B( #M( empty}
%% First(t) = {symbol number string #' ( [ . ' ` , ,@ #( #B( #M( empty}
%% First(p) = {symbol number string #' ( [ ' ` , ,@ #( #B( #M( empty}
%% Follow(f) = empty
%% Follow(s) = {symbol number string #' ( [ ) ] ' ` , ,@ #( #B( #M(}
%% Follow(l) = {symbol number string #' ( [ ) ] ' ` , ,@ #( #B( #M(}
%% Follow(t) = {symbol number string #' ( [ ) ] ' ` , ,@ #( #B( #M(}
%% Follow(p) = {symbol number string #' ( [ ) ] ' ` , ,@ #( #B( #M(}

%% The table (tedious).
%% Top  symbol      (         )       [       ]       .       '`,,@   #(#B(#M(
%%  f   f->s      f->s              f->s                      f->s    f->s
%%  s   s->sym    s->( l )          s->[ s ]                  s->' s  s->( p )
%%  l   l->s t    l->s t     l->e   l->s t   l->e             l->s t  l->s t
%%  t   t->s t    t->s t     t->e   t->s t   t->e   t->. s    t->s t  t->s t
%%  p   p->s p    p->s p     p->e   p->s p   p->e             p->s p  p->s p

%% The non-terminal types.
-define(FORM, 0).
-define(EXPR, 1).
-define(LIST, 2).
-define(TAIL, 3).
-define(PROP, 4).

%% Start non-terminal state.
start() -> ?FORM.

%% The reductions, we are naive and straight forward here.
reduce(0, Vs) -> Vs;                            %f->s
reduce(1, [T|Vs]) -> [val(T)|Vs];               %s->symbol
reduce(2, [T|Vs]) -> [val(T)|Vs];               %s->number
reduce(3, [T|Vs]) -> [val(T)|Vs];               %s->string
reduce(4, [T|Vs]) ->                            %s->fun
    [make_fun(val(T))|Vs];
reduce(5, [S,_|Vs]) -> [[quote,S]|Vs];          %s->' s
reduce(6, [S,_|Vs]) -> [[backquote,S]|Vs];      %s->` s
reduce(7, [S,_|Vs]) -> [[unquote,S]|Vs];        %s->, s
reduce(8, [S,_|Vs]) ->                          %s->,@ s
    [['unquote-splicing',S]|Vs];
reduce(9, [_,L,_|Vs]) -> [L|Vs];                %s->( s )
reduce(10, [_,L,_|Vs]) -> [L|Vs];               %s->[ s ]
reduce(11, [_,L,_|Vs]) ->                       %s->#( p )
    [list_to_tuple(L)|Vs];
reduce(12, [_,L,B|Vs]) ->                       %s->#B( p )
    case catch lfe_eval:literal([binary|L]) of
        Bin when is_bitstring(Bin) -> [Bin|Vs];
        _ -> {error,line(B),{illegal,binary}}
    end;
reduce(13, [_,L,B|Vs]) ->                       %s->#M( p )
    case catch maps:from_list(pair_list(L)) of
        Map when ?IS_MAP(Map) -> [Map|Vs];
        _ -> {error,line(B),{illegal,map}}
    end;
reduce(14, [T,H|Vs]) -> [[H|T]|Vs];             %l->s t
reduce(15, Vs) -> [[]|Vs];                      %l->empty
reduce(16, [T,H|Vs]) -> [[H|T]|Vs];             %t->s t
reduce(17, [T,_|Vs]) -> [T|Vs];                 %t->. s
reduce(18, Vs) -> [[]|Vs];                      %t->empty
reduce(19, [T,H|Vs]) -> [[H|T]|Vs];             %p->s t
reduce(20, Vs) -> [[]|Vs].                      %p->empty

%% The table, this gets pretty big but is very straight forward.
table(?FORM, symbol) -> [?EXPR];
table(?FORM, number) -> [?EXPR];
table(?FORM, string) -> [?EXPR];
table(?FORM, '#\'') -> [?EXPR];
table(?FORM, '\'') -> [?EXPR];
table(?FORM, '`') -> [?EXPR];
table(?FORM, ',') -> [?EXPR];
table(?FORM, ',@') -> [?EXPR];
table(?FORM, '(') -> [?EXPR];
table(?FORM, '[') -> [?EXPR];
table(?FORM, '#(') -> [?EXPR];
table(?FORM, '#B(') -> [?EXPR];
table(?FORM, '#M(') -> [?EXPR];

table(?EXPR, symbol) -> [symbol,{reduce,1}];
table(?EXPR, number) -> [number,{reduce,2}];
table(?EXPR, string) -> [string,{reduce,3}];
table(?EXPR, '#\'') -> ['#\'',{reduce,4}];
table(?EXPR, '\'') -> ['\'',?EXPR,{reduce,5}];
table(?EXPR, '`') -> ['`',?EXPR,{reduce,6}];
table(?EXPR, ',') -> [',',?EXPR,{reduce,7}];
table(?EXPR, ',@') -> [',@',?EXPR,{reduce,8}];
table(?EXPR, '(') -> ['(',?LIST,')',{reduce,9}];
table(?EXPR, '[') -> ['[',?LIST,']',{reduce,10}];
table(?EXPR, '#(') -> ['#(',?PROP,')',{reduce,11}];
table(?EXPR, '#B(') -> ['#B(',?PROP,')',{reduce,12}];
table(?EXPR, '#M(') -> ['#M(',?PROP,')',{reduce,13}];

table(?LIST, symbol) -> [?EXPR,?TAIL,{reduce,14}];
table(?LIST, number) -> [?EXPR,?TAIL,{reduce,14}];
table(?LIST, string) -> [?EXPR,?TAIL,{reduce,14}];
table(?LIST, '#\'') -> [?EXPR,?TAIL,{reduce,14}];
table(?LIST, '\'') -> [?EXPR,?TAIL,{reduce,14}];
table(?LIST, '`') -> [?EXPR,?TAIL,{reduce,14}];
table(?LIST, ',') -> [?EXPR,?TAIL,{reduce,14}];
table(?LIST, ',@') -> [?EXPR,?TAIL,{reduce,14}];
table(?LIST, '(') -> [?EXPR,?TAIL,{reduce,14}];
table(?LIST, '[') -> [?EXPR,?TAIL,{reduce,14}];
table(?LIST, '#(') -> [?EXPR,?TAIL,{reduce,14}];
table(?LIST, '#B(') -> [?EXPR,?TAIL,{reduce,14}];
table(?LIST, '#M(') -> [?EXPR,?TAIL,{reduce,14}];
table(?LIST, ')') -> [{reduce,15}];
table(?LIST, ']') -> [{reduce,15}];

table(?TAIL, symbol) -> [?EXPR,?TAIL,{reduce,16}];
table(?TAIL, number) -> [?EXPR,?TAIL,{reduce,16}];
table(?TAIL, string) -> [?EXPR,?TAIL,{reduce,16}];
table(?TAIL, '#\'') -> [?EXPR,?TAIL,{reduce,16}];
table(?TAIL, '\'') -> [?EXPR,?TAIL,{reduce,16}];
table(?TAIL, '`') -> [?EXPR,?TAIL,{reduce,16}];
table(?TAIL, ',') -> [?EXPR,?TAIL,{reduce,16}];
table(?TAIL, ',@') -> [?EXPR,?TAIL,{reduce,16}];
table(?TAIL, '(') -> [?EXPR,?TAIL,{reduce,16}];
table(?TAIL, '[') -> [?EXPR,?TAIL,{reduce,16}];
table(?TAIL, '#(') -> [?EXPR,?TAIL,{reduce,16}];
table(?TAIL, '#B(') -> [?EXPR,?TAIL,{reduce,16}];
table(?TAIL, '#M(') -> [?EXPR,?TAIL,{reduce,16}];
table(?TAIL, '.') -> ['.',?EXPR,{reduce,17}];
table(?TAIL, ')') -> [{reduce,18}];
table(?TAIL, ']') -> [{reduce,18}];

table(?PROP, symbol) -> [?EXPR,?PROP,{reduce,19}];
table(?PROP, number) -> [?EXPR,?PROP,{reduce,19}];
table(?PROP, string) -> [?EXPR,?PROP,{reduce,19}];
table(?PROP, '#\'') -> [?EXPR,?PROP,{reduce,19}];
table(?PROP, '\'') -> [?EXPR,?PROP,{reduce,19}];
table(?PROP, '`') -> [?EXPR,?PROP,{reduce,19}];
table(?PROP, ',') -> [?EXPR,?PROP,{reduce,19}];
table(?PROP, ',@') -> [?EXPR,?PROP,{reduce,19}];
table(?PROP, '(') -> [?EXPR,?PROP,{reduce,19}];
table(?PROP, '[') -> [?EXPR,?PROP,{reduce,19}];
table(?PROP, '#(') -> [?EXPR,?PROP,{reduce,19}];
table(?PROP, '#B(') -> [?EXPR,?PROP,{reduce,19}];
table(?PROP, '#M(') -> [?EXPR,?PROP,{reduce,19}];
table(?PROP, ')') -> [{reduce,20}];
table(?PROP, ']') -> [{reduce,20}];

table(_, _) -> error.

%% sexpr(Tokens) ->
%%      {ok,Line,Sexpr,Rest} | {more,Continuation} | {error,Error,Rest}.
%% sexpr(Continuation, Tokens) ->
%%      {ok,Line,Sexpr,Rest} | {more,Continuation} | {error,Error,Rest}.

sexpr(Ts) -> sexpr([], Ts).            %Start with empty state

sexpr(Cont, Ts) -> parse1(Cont, Ts).

-record(lp, {l=none,st=[],vs=[]}).     %Line, States, Values

%% parse1(Tokens) ->
%%      {ok,Line,Sexpr,Rest} | {more,Continuation} | {error,Error,Rest}.
%% parse1(Continuation, Tokens) ->
%%      {ok,Line,Sexpr,Rest} | {more,Continuation} | {error,Error,Rest}.
%% This is the opt-level of the LL engine. It
%% initialises/packs/unpacks the continuation information.

parse1([], Ts) ->                           %First call
    Start = start(),                        %The start state.
    parse1(#lp{l=none,st=[Start],vs=[]}, Ts);
parse1(#lp{l=none}=Lp, [T|_]=Ts) ->         %Guarantee a start line
    parse1(Lp#lp{l=line(T)}, Ts);
parse1(#lp{l=L,st=St0,vs=Vs0}, Ts) ->
    case parse2(Ts, St0, Vs0) of
    {done,Rest,[],[V]} -> {ok,L,V,Rest};
    {more,[],St1,Vs1} -> {more,#lp{l=L,st=St1,vs=Vs1}};
    {error,Line,Error,Rest,_,_} ->
        %% Can't really continue from errors here.
        {error,{Line,?MODULE,Error},Rest}
    end.

%% parse2(Tokens, StateStack, ValueStack) ->
%%     {done,Ts,Sstack,Vstack} | {more,Ts,Sstack,Vstack} |
%%     {error,Line,Error,Ts,Sstack,Vstack}.
%% Main loop of the parser engine. Handle any reductions on the top of
%% the StateStack, then try to match type of next token with top
%% state. If we have a match, it is a terminal, then push token onto
%% value stack, else try to find new state(s) from table using current
%% state and token type and push them onto state stack. Continue until
%% no states left.

parse2(Ts, [{reduce,R}|St], Vs0) ->
    %% io:fwrite("p: ~p\n", [{R,Vs}]),
    %% Try to reduce values and push value on value stack.
    case reduce(R, Vs0) of
    {error,L,E} -> {error,L,E,Ts,St,Vs0};
    Vs1 -> parse2(Ts, St, Vs1)
    end;
parse2(Ts, [], Vs) -> {done,Ts,[],Vs};          %All done
parse2([T|Ts]=Ts0, [S|St]=St0, Vs) ->
    %% io:fwrite("p: ~p\n", [{St0,Ts0}]),
    %% Try to match token type against state on stack.
    case type(T) of
    S -> parse2(Ts, St, [T|Vs]);                %Match
    Type ->                                     %Try to predict
        case table(S, Type) of
        error -> {error,line(T),{illegal,Type},Ts0,St0,Vs};
        Top -> parse2(Ts0, Top ++ St, Vs)
        end
    end;
parse2([], St, Vs) ->                           %Need more tokens
    {more,[],St,Vs};
parse2({eof,L}=Ts, St, Vs) ->                   %No more tokens
    {error,L,{missing,token},Ts,St,Vs}.

%% Access the fields of a token.
type(T) -> element(1, T).
line(T) -> element(2, T).
val(T) -> element(3, T).

%% make_fun(String) -> FunList.
%%  Convert a fun string to a fun sexpr.
%%    "F/A" -> ['fun', F, A].
%%    "M:F/A" -> ['fun', M, F, A].

make_fun("=:=/2") ->
    ['fun', '=:=', 2];
make_fun(FunStr) ->
    J = string:rchr(FunStr, $/),
    A = list_to_integer(string:substr(FunStr, J + 1)),
    case string:chr(FunStr, $:) of
    0 ->
        F = list_to_atom(string:substr(FunStr, 1, J - 1)),
        ['fun', F, A];
    I ->
        F = list_to_atom(string:substr(FunStr, I + 1, J - I - 1)),
        M = list_to_atom(string:substr(FunStr, 1, I - 1)),
        ['fun', M, F, A]
    end.

%% pair_list(List) -> [{A,B}].
%%  Generate a list of tuple pairs from the elements. An error if odd
%%  number of elements in list.

pair_list([A,B|L]) -> [{A,B}|pair_list(L)];
pair_list([]) -> [].

%% format_error(Error) -> String.
%%  Format errors to printable string.

format_error({missing,Tok}) ->
    io_lib:fwrite("missing ~p", [Tok]);
format_error({illegal,What}) ->
    io_lib:fwrite("illegal ~p", [What]).
