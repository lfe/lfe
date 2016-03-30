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
%% Purpose : Lisp Flavoured Erlang docstring parser.

%% TODO: write explanation.


-module(lfe_doc).

-export([module/1,patterns/1]).

-import(lfe_lib, [is_symb_list/1,is_proper_list/1]).

-import(lists, [reverse/1]).

-include("lfe_comp.hrl").

%% Errors
%% format_error({bad_form,Type}) ->
%%     lfe_io:format1("bad form: ~w", [Type]);
%% format_error({bad_env_form,Type}) ->
%%     lfe_io:format1("bad environment form: ~w", [Type]);
%% format_error({expand_macro,Call,_}) ->
%%     %% Can be very big so only print limited depth.
%%     lfe_io:format1("error expanding ~P", [Call,10]).

module({ok,Mod,[],Warns})   -> {ok,Mod,[],Warns,[]};
module({ok,Mod,Defs,Warns}) -> {ok,Mod,Defs,Warns,do_module([], Defs)}.

do_module(Docs, []) -> Docs;
do_module(Docs, [{['define-function',Name,Body,DocStr],_Line}|Defs]) ->
    {yes,Arity,Patterns} = patterns(Body),
    Doc = make_doc(function, Name, Arity, Patterns, DocStr),
    do_module([Doc|Docs], Defs);
do_module(Docs, [{['define-macro',Name,Body,DocStr],_Line}|Defs]) ->
    {yes,Arity,Patterns} = patterns(Body),
    Doc = make_doc(macro, Name, Arity, Patterns, DocStr),
    do_module([Doc|Docs], Defs);
do_module(Docs, [_|Defs]) -> do_module(Docs, Defs).

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

-spec make_doc(Type, Name, Arity, Patterns, Doc) -> doc() when
      Type     :: 'function' | 'macro',
      Name     :: atom(),
      Arity    :: non_neg_integer(),
      Patterns :: [[]],
      Doc      :: binary().
make_doc(Type, Name, Arity, Patterns, Doc0) when is_list(Doc0) ->
    Doc1 = unicode:characters_to_binary(Doc0, utf8, utf8),
    make_doc(Type, Name, Arity, Patterns, Doc1);
make_doc(Type, Name, Arity, Patterns, Doc) when is_binary(Doc) ->
    #doc{type=Type,name=Name,arity=Arity,patterns=Patterns,doc=Doc}.
