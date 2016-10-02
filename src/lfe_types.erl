%% Copyright (c) 2016 Robert Virding
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

%% File    : lfe_types.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang type formatting.

%% Handling types in LFE including functions for converting between
%% Erlang and LFE type formats.

-module(lfe_types).

-export([from_type_def/1,from_type_defs/1,to_type_def/2,to_type_defs/2,
         from_func_type/1,from_func_types/1,to_func_type/2,to_func_types/2]).

-compile(export_all).

-include("lfe.hrl").

%% from_type_def(AST) -> Def.
%%  Translate an Erlang type definition to LFE. Currently it does
%%  nothing.  We don't differentiate here between pre-defined or user
%%  defined types.

from_type_def({type,_L,tuple,any}) -> [tuple];
from_type_def({type,_L,Type,Args}) when is_list(Args) ->
    [Type|from_type_defs(Args)];
from_type_def({user_type,_L,Type,Args}) when is_list(Args) ->
    [Type|from_type_defs(Args)];
from_type_def({ann_type,_L,[_Var,Type]}) ->     %Annotated types lose variable
    from_type_def(Type);
from_type_def({remote_type,_L,[{atom,_,M},{atom,_,T},Args]}) ->
    Type = list_to_atom(lists:concat([M,":",T])),
    [Type|from_type_defs(Args)];
from_type_def({var,_L,Var}) -> Var;             %A type variable
from_type_def({atom,_L,Atom}) -> [quote,Atom];  %Literal atom
from_type_def({integer,_L,Int}) -> Int.         %Literal integer

from_type_defs(Ts) ->
    lists:map(fun from_type_def/1, Ts).

%% to_type_def(Def, Line) -> AST.

to_type_def([quote,Val], Line) ->                       %Quoted atom literal
    to_lit(Val, Line);
to_type_def([tuple], Line) ->                   %Undefined tuple
    {type,Line,tuple,any};
to_type_def([tuple|Args], Line) ->
    {type,Line,tuple,to_type_defs(Args, Line)};
to_type_def([Type|Args], Line) ->
    Dargs = to_type_defs(Args, Line),
    case string:tokens(atom_to_list(Type), ":") of
        [M,T] ->                                %Remote type
            {remote_type,Line,
             [{atom,Line,list_to_atom(M)},{atom,Line,list_to_atom(T)},Dargs]};
        _ ->                                    %This will also catch a:b:c
            %% Get the right tag here.
            Tag = case erl_internal:is_type(Type, length(Args)) of
                      true -> type;
                      false -> user_type
                  end,
            {Tag,Line,Type,Dargs}
        end;
to_type_def(Val, Line) when is_integer(Val) ->  %Literal integer value
    to_lit(Val, Line);
to_type_def(Val, Line) when is_atom(Val) ->     %Variable
    {var,Line,Val}.

to_lit(Val, Line) when is_atom(Val) -> {atom,Line,Val};
to_lit(Val, Line) when is_integer(Val) -> {integer,Line,Val}.

to_type_defs(Ds, Line) ->
    lists:map(fun (D) -> to_type_def(D, Line) end, Ds).

%% from_func_type(AST) -> Type.
%% to_func_type(Type, Line) -> AST.

from_func_type({type,_L,'fun',[Prod,Ret]}) ->
    [from_func_prod(Prod),from_type_def(Ret)];
from_func_type({type,_L,bounded_fun,[Fun,Cs]}) ->
    from_func_type(Fun) ++ [from_func_constraints(Cs)].

from_func_types(Ss) ->
    lists:map(fun from_func_type/1, Ss).

from_func_prod({type,_L,product,Args}) when is_list(Args) ->
    from_type_defs(Args).                       %Function arguments

from_func_constraint({type,_,constraint,[{atom,_,is_subtype},St]}) ->
    from_subtype(St).

from_func_constraints(Cs) ->
    lists:map(fun from_func_constraint/1, Cs).

from_subtype([{var,_,Var},Type]) -> [Var,from_type_def(Type)].

to_func_type([Prod,Ret], Line) ->
    to_func_type(Prod, Ret, Line);
to_func_type([Prod,Ret,[]], Line) ->            %Future proof
    to_func_type(Prod, Ret, Line);
to_func_type([Prod,Ret,Cs], Line) ->
    Fun = to_func_type(Prod, Ret, Line),
    Constr = to_func_constraints(Cs, Line),
    {type,Line,bounded_fun,[Fun,Constr]}.

to_func_type(Prod, Ret, Line) ->
    {type,Line,'fun',[to_func_prod(Prod, Line),to_type_def(Ret, Line)]}.

to_func_types(Fts, Line) ->
    lists:map(fun (Ft) -> to_func_type(Ft, Line) end, Fts).

to_func_prod(Args, Line) ->
    {type,Line,product,to_type_defs(Args, Line)}.

to_func_constraints(Cs, Line) ->
    [ to_func_constraint(C, Line) || C <- Cs ].

to_func_constraint([Var,Type], Line) ->
    {type,Line,constraint,[{atom,Line,is_subtype},
                           [{var,Line,Var},to_type_def(Type, Line)]]}.
