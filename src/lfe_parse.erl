%% Copyright (c) 2009-2017 Robert Virding
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

-module(lfe_parse).

-export([format_error/1]).

%% The root symbol entry points
-export([form/1,form/2]).

%% User code. This is placed here to allow extra attributes.

-define(CATCH(E, Error), try E catch _:_ -> Error end).

%% For backwards compatibility
-export([sexpr/1,sexpr/2]).

sexpr(Ts) -> form(Ts).
sexpr(Cont, Ts) -> form(Cont, Ts).

%% make_fun(String) -> FunList.
%%  Convert a fun string to a fun sexpr.
%%    "F/A" -> [function, F, A].
%%    "M:F/A" -> [function, M, F, A].

make_fun("=:=/2") ->
    [function,'=:=',2];
make_fun(FunStr) ->
    J = string:rchr(FunStr, $/),
    A = list_to_integer(string:substr(FunStr, J + 1)),
    case string:chr(FunStr, $:) of
        0 ->
            F = list_to_atom(string:substr(FunStr, 1, J - 1)),
            [function,F,A];
        I ->
            F = list_to_atom(string:substr(FunStr, I + 1, J - I - 1)),
            M = list_to_atom(string:substr(FunStr, 1, I - 1)),
            [function,M,F,A]
    end.

%% make_bin(Line, Segments) -> Binary.
%%  Make a binary from the segments.

make_bin(Line, Segs) ->
    ?CATCH(lfe_eval:expr([binary|Segs]),
           return_error(Line, "bad binary")).

%% make_map(Line, Elements) -> Map.
%%  Make a map from the key/value elements.

make_map(Line, Es) ->
    ?CATCH(maps:from_list(pair_list(Es)),
           return_error(Line, "bad map")).

%% pair_list(List) -> [{A,B}].
%%  Generate a list of tuple pairs from the elements. An error if odd
%%  number of elements in list.

pair_list([A,B|L]) -> [{A,B}|pair_list(L)];
pair_list([]) -> [].

%% eval_expr(Line, Expr) -> Val.
%%  Evaluate #. expression.

eval_expr(Line, Expr) ->
    ?CATCH(lfe_eval:expr(Expr),
           return_error(Line, "bad #. expression")).

-record(spell1, {line=none,st=[],vs=[]}).     %Line, States, Values

%% parse1(Continuation, Tokens) ->
%%      {ok,Line,Sexpr,Rest} | {more,Continuation} | {error,Error,Rest}.
%%  This is the opt-level of the LL engine. It
%%  initialises/packs/unpacks the continuation information.

parse1([], Ts) ->                               %First call
    Start = start(),                            %The start state.
    parse1(#spell1{line=none,st=[Start],vs=[]}, Ts);
parse1(#spell1{line=none}=Lp, [T|_]=Ts) ->         %Guarantee a start line
    parse1(Lp#spell1{line=line(T)}, Ts);
parse1(#spell1{line=L,st=St0,vs=Vs0}, Ts) ->
    try
        parse2(Ts, St0, Vs0) of
        {done,Rest,[],[V]} -> {ok,L,V,Rest};
        {more,[],St1,Vs1} -> {more,#spell1{line=L,st=St1,vs=Vs1}};
        {error,Line,Error,Rest,_,_} ->
            %% Can't really continue from errors here.
            {error,{Line,?MODULE,Error},Rest}
    catch
        throw:{spell1_error,Error} ->
            {error,Error,[]}
    end.

%% parse2(Tokens, StateStack, ValueStack) ->
%%     {done,Ts,Sstack,Vstack} | {more,Ts,Sstack,Vstack} |
%%     {error,Line,Error,Ts,Sstack,Vstack}.
%%  Main loop of the parser engine. Handle any reductions on the top
%%  of the StateStack, then try to match type of next token with top
%%  state. If we have a match, it is a terminal, then push token onto
%%  value stack, else try to find new state(s) from table using
%%  current state and token type and push them onto state
%%  stack. Continue until no states left.

parse2(Ts, [{reduce,R}|St], Vs0) ->
    %% io:fwrite("p1: ~p\n", [{Ts,R,Vs0}]),
    %% Try to reduce values and push value on value stack.
    case reduce(R, Vs0) of
        {error,L,E} -> {error,L,E,Ts,St,Vs0};
        Vs1 -> parse2(Ts, St, Vs1)
    end;
parse2(Ts, [], Vs) -> {done,Ts,[],Vs};          %All done
parse2([T|Ts]=Ts0, [S|St]=St0, Vs) ->
    %% io:fwrite("p3: ~p\n", [{Ts0,St0,Vs}]),
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
    {error,L,missing_token,Ts,St,Vs}.

%% Access the fields of a token.
-compile({nowarn_unused_function, type/1}).
-compile({nowarn_unused_function, line/1}).
-compile({nowarn_unused_function, value/1}).
type(T) -> element(1, T).
line(T) -> element(2, T).
value(T) -> element(3, T).

%% The root symbol entry points.
form(Ts) -> parse1([], Ts).
form(Cont, Ts) -> parse1(Cont, Ts).

%% The table.
start() -> form.

table(list_tail, '#\'') -> [sexpr,list_tail,{reduce,18}];
table(list_tail, '#(') -> [sexpr,list_tail,{reduce,18}];
table(list_tail, '#.') -> [sexpr,list_tail,{reduce,18}];
table(list_tail, '#B(') -> [sexpr,list_tail,{reduce,18}];
table(list_tail, '#M(') -> [sexpr,list_tail,{reduce,18}];
table(list_tail, ')') -> [{reduce,20}];
table(list_tail, ']') -> [{reduce,20}];
table(list_tail, '\'') -> [sexpr,list_tail,{reduce,18}];
table(list_tail, '(') -> [sexpr,list_tail,{reduce,18}];
table(list_tail, ',') -> [sexpr,list_tail,{reduce,18}];
table(list_tail, ',@') -> [sexpr,list_tail,{reduce,18}];
table(list_tail, '.') -> ['.',sexpr,{reduce,19}];
table(list_tail, '[') -> [sexpr,list_tail,{reduce,18}];
table(list_tail, '`') -> [sexpr,list_tail,{reduce,18}];
table(list_tail, binary) -> [sexpr,list_tail,{reduce,18}];
table(list_tail, number) -> [sexpr,list_tail,{reduce,18}];
table(list_tail, string) -> [sexpr,list_tail,{reduce,18}];
table(list_tail, symbol) -> [sexpr,list_tail,{reduce,18}];
table(sexpr, '#\'') -> ['#\'',{reduce,5}];
table(sexpr, '#(') -> ['#(',proper_list,')',{reduce,13}];
table(sexpr, '#.') -> ['#.',sexpr,{reduce,6}];
table(sexpr, '#B(') -> ['#B(',proper_list,')',{reduce,14}];
table(sexpr, '#M(') -> ['#M(',proper_list,')',{reduce,15}];
table(sexpr, '\'') -> ['\'',sexpr,{reduce,7}];
table(sexpr, '(') -> ['(',list,')',{reduce,11}];
table(sexpr, ',') -> [',',sexpr,{reduce,9}];
table(sexpr, ',@') -> [',@',sexpr,{reduce,10}];
table(sexpr, '[') -> ['[',list,']',{reduce,12}];
table(sexpr, '`') -> ['`',sexpr,{reduce,8}];
table(sexpr, binary) -> [binary,{reduce,4}];
table(sexpr, number) -> [number,{reduce,2}];
table(sexpr, string) -> [string,{reduce,3}];
table(sexpr, symbol) -> [symbol,{reduce,1}];
table(list, '#\'') -> [sexpr,list_tail,{reduce,16}];
table(list, '#(') -> [sexpr,list_tail,{reduce,16}];
table(list, '#.') -> [sexpr,list_tail,{reduce,16}];
table(list, '#B(') -> [sexpr,list_tail,{reduce,16}];
table(list, '#M(') -> [sexpr,list_tail,{reduce,16}];
table(list, ')') -> [{reduce,17}];
table(list, ']') -> [{reduce,17}];
table(list, '\'') -> [sexpr,list_tail,{reduce,16}];
table(list, '(') -> [sexpr,list_tail,{reduce,16}];
table(list, ',') -> [sexpr,list_tail,{reduce,16}];
table(list, ',@') -> [sexpr,list_tail,{reduce,16}];
table(list, '[') -> [sexpr,list_tail,{reduce,16}];
table(list, '`') -> [sexpr,list_tail,{reduce,16}];
table(list, binary) -> [sexpr,list_tail,{reduce,16}];
table(list, number) -> [sexpr,list_tail,{reduce,16}];
table(list, string) -> [sexpr,list_tail,{reduce,16}];
table(list, symbol) -> [sexpr,list_tail,{reduce,16}];
table(form, '#\'') -> [sexpr,{reduce,0}];
table(form, '#(') -> [sexpr,{reduce,0}];
table(form, '#.') -> [sexpr,{reduce,0}];
table(form, '#B(') -> [sexpr,{reduce,0}];
table(form, '#M(') -> [sexpr,{reduce,0}];
table(form, '\'') -> [sexpr,{reduce,0}];
table(form, '(') -> [sexpr,{reduce,0}];
table(form, ',') -> [sexpr,{reduce,0}];
table(form, ',@') -> [sexpr,{reduce,0}];
table(form, '[') -> [sexpr,{reduce,0}];
table(form, '`') -> [sexpr,{reduce,0}];
table(form, binary) -> [sexpr,{reduce,0}];
table(form, number) -> [sexpr,{reduce,0}];
table(form, string) -> [sexpr,{reduce,0}];
table(form, symbol) -> [sexpr,{reduce,0}];
table(proper_list, '#\'') -> [sexpr,proper_list,{reduce,21}];
table(proper_list, '#(') -> [sexpr,proper_list,{reduce,21}];
table(proper_list, '#.') -> [sexpr,proper_list,{reduce,21}];
table(proper_list, '#B(') -> [sexpr,proper_list,{reduce,21}];
table(proper_list, '#M(') -> [sexpr,proper_list,{reduce,21}];
table(proper_list, ')') -> [{reduce,22}];
table(proper_list, '\'') -> [sexpr,proper_list,{reduce,21}];
table(proper_list, '(') -> [sexpr,proper_list,{reduce,21}];
table(proper_list, ',') -> [sexpr,proper_list,{reduce,21}];
table(proper_list, ',@') -> [sexpr,proper_list,{reduce,21}];
table(proper_list, '[') -> [sexpr,proper_list,{reduce,21}];
table(proper_list, '`') -> [sexpr,proper_list,{reduce,21}];
table(proper_list, binary) -> [sexpr,proper_list,{reduce,21}];
table(proper_list, number) -> [sexpr,proper_list,{reduce,21}];
table(proper_list, string) -> [sexpr,proper_list,{reduce,21}];
table(proper_list, symbol) -> [sexpr,proper_list,{reduce,21}];
table(_, _) -> error.

%% The reductions, we are naive and straight forward here.
reduce(0, [__1|__Vs]) -> [ begin  __1 end | __Vs];
reduce(1, [__1|__Vs]) -> [ begin  value (__1) end | __Vs];
reduce(2, [__1|__Vs]) -> [ begin  value (__1) end | __Vs];
reduce(3, [__1|__Vs]) -> [ begin  value (__1) end | __Vs];
reduce(4, [__1|__Vs]) -> [ begin  value (__1) end | __Vs];
reduce(5, [__1|__Vs]) -> [ begin  make_fun (value (__1)) end | __Vs];
reduce(6, [__2,__1|__Vs]) -> [ begin  eval_expr (line (__1), __2) end | __Vs];
reduce(7, [__2,__1|__Vs]) -> [ begin  [quote, __2] end | __Vs];
reduce(8, [__2,__1|__Vs]) -> [ begin  [backquote, __2] end | __Vs];
reduce(9, [__2,__1|__Vs]) -> [ begin  [comma, __2] end | __Vs];
reduce(10, [__2,__1|__Vs]) -> [ begin  ['comma-at', __2] end | __Vs];
reduce(11, [__3,__2,__1|__Vs]) -> [ begin  __2 end | __Vs];
reduce(12, [__3,__2,__1|__Vs]) -> [ begin  __2 end | __Vs];
reduce(13, [__3,__2,__1|__Vs]) -> [ begin  list_to_tuple (__2) end | __Vs];
reduce(14, [__3,__2,__1|__Vs]) -> [ begin  make_bin (line (__1), __2) end | __Vs];
reduce(15, [__3,__2,__1|__Vs]) -> [ begin  make_map (line (__1), __2) end | __Vs];
reduce(16, [__2,__1|__Vs]) -> [ begin  [__1 | __2] end | __Vs];
reduce(17, __Vs) -> [ begin  [] end | __Vs];
reduce(18, [__2,__1|__Vs]) -> [ begin  [__1 | __2] end | __Vs];
reduce(19, [__2,__1|__Vs]) -> [ begin  __2 end | __Vs];
reduce(20, __Vs) -> [ begin  [] end | __Vs];
reduce(21, [__2,__1|__Vs]) -> [ begin  [__1 | __2] end | __Vs];
reduce(22, __Vs) -> [ begin  [] end | __Vs];
reduce(_, _) -> error(function_clause).

%% format_error(Error) -> String.
%%  Format errors to printable string.

format_error(missing_token) -> "missing token";
format_error({illegal,What}) ->
    io_lib:fwrite("illegal ~p", [What]);
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        false -> io_lib:write(Message)
    end.

%% return_error(Error).
%%  To be used in grammar files to throw an error message to the
%%  parser toplevel. Doesn't have to be exported!

-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().

return_error(Line, Message) ->
    throw({spell1_error, {Line, ?MODULE, Message}}).
