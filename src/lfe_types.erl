%% Copyright (c) 2016-2017 Robert Virding
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
%% Erlang and LFE type syntaxes.
%%
%% We can correctly do most types except for maps where we lose the
%% distinction between assoc and exact pairs.

-module(lfe_types).

-export([format_error/1]).

-export([from_type_def/1,from_type_defs/1,to_type_def/2,to_type_defs/2,
         check_type_def/3,check_type_defs/3]).

-export([from_func_spec_list/1,to_func_spec_list/2,
         check_func_spec_list/3]).

-export([is_predefined_type/2]).

-compile(export_all).

-include("lfe.hrl").

%% format_error(Error) -> String.
%%  Do we really need this here?

format_error({bad_type,T}) ->
    lfe_io:format1("bad ~w type definition", [T]);
format_error({type_syntax,T}) ->
    lfe_io:format1(<<"bad ~w type">>, [T]);
format_error({undefined_type,{T,A}}) ->
    lfe_io:format1(<<"type ~w/~w undefined">>, [T,A]);
format_error({bad_spec,S}) ->
    lfe_io:format1("bad function spec: ~w", [S]).

%% from_type_def(AST) -> Def.
%%  Translate an Erlang type definition to LFE. This takes the Erlang
%%  AST form of a type definition and translates to the LFE type
%%  syntax. No AST there of course.

%% Our special cases.
from_type_def({type,_L,union,Types}) ->         %Special case union
    ['UNION'|from_type_defs(Types)];
from_type_def({type,_L,tuple,any}) -> [tuple];
from_type_def({type,_L,binary,Bits}) when Bits =/= [] ->
     [bitstring|from_type_defs(Bits)];          %Flip binary<->bitstring here
%% from_type_def({type,_L,bitstring,[]}) -> [bitstring,[]];
from_type_def({type,_L,map,Pairs}) ->
    [map|from_map_pairs(Pairs)];
from_type_def({type,_L,record,[{atom,_L,Name}|Fields]}) ->
    [record,Name|from_rec_fields(Fields)];
from_type_def({type,_L,'fun',[Args,Ret]}) ->
    [lambda,from_lambda_args(Args),from_type_def(Ret)];
%% The standard erlang types.
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
from_type_def({atom,_L,Atom}) -> ?Q(Atom);      %Literal atom
from_type_def({integer,_L,Int}) -> Int.         %Literal integer

from_type_defs(Ts) ->
    lists:map(fun from_type_def/1, Ts).

from_map_pairs(Pairs) ->
    %% Lose distinction between assoc and exact pairs.
    Fun = fun ({type,_L,_P,Types}) -> from_type_defs(Types) end,
    lists:map(Fun, Pairs).

from_rec_fields(Fields) ->
    Fun = fun ({type,_L,field_type,[{atom,_,Name},Type]}) ->
                  [Name,from_type_def(Type)]
          end,
    [ Fun(F) || F <- Fields ].

from_lambda_args({type,_L,any}) -> any;         %Any arity
from_lambda_args(Args) -> from_func_prod(Args).

%% to_type_def(Def, Line) -> AST.
%%  Translate a type definition from the LFE type syntax to the Erlang
%%  AST. We must explicitly handle our special cases.

%% Our special cases.
to_type_def(['UNION'|Types], Line) ->           %Union
    {type,Line,union,to_type_defs(Types, Line)};
to_type_def([range,I1,I2], Line) ->
    {type,Line,range,to_type_defs([I1,I2], Line)};
to_type_def([bitstring,I1,I2], Line) ->         %Flip binary<->bitstring here
    {type,Line,binary,to_type_defs([I1,I2], Line)};
to_type_def([tuple], Line) ->                   %Undefined tuple
    {type,Line,tuple,any};
to_type_def([tuple|Args], Line) ->              %Not a user defined type
    {type,Line,tuple,to_type_defs(Args, Line)};
to_type_def([map|Pairs], Line) ->
    {type,Line,map,to_map_pairs(Pairs, Line)};
to_type_def([record,Name|Fields], Line) ->
    {type,Line,record,[to_lit(Name, Line)|to_rec_fields(Fields, Line)]};
to_type_def([lambda,Args,Ret], Line) ->
    {type,Line,'fun',[to_lambda_args(Args, Line),to_type_def(Ret, Line)]};
to_type_def(?Q(Val), Line) ->                   %Quoted atom literal
    to_lit(Val, Line);
to_type_def([call,?Q(M),?Q(T)|Args], Line) ->
    %% Special case mod:fun expands to (call 'mod 'fun)
    Dargs = to_type_defs(Args, Line),
    {remote_type,Line,[{atom,Line,M},{atom,Line,T},Dargs]};
%% The standard erlang types.
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

to_type_defs(Ds, Line) ->
    lists:map(fun (D) -> to_type_def(D, Line) end, Ds).

to_lit(Val, Line) when is_atom(Val) -> {atom,Line,Val};
to_lit(Val, Line) when is_integer(Val) -> {integer,Line,Val}.

to_map_pairs(Pairs, Line) ->
    %% Have lost distinction between assoc and exact pairs.
    Fun = fun (Pair) ->
                  {type,Line,map_field_assoc,to_type_defs(Pair, Line)}
          end,
    [ Fun(P) || P <- Pairs ].

to_rec_fields(Fs, Line) ->
    Fun = fun ([F,Type]) ->
                  {type,Line,field_type,
                   [to_lit(F, Line),to_type_def(Type, Line)]}
          end,
    [ Fun(F) || F <- Fs ].

to_lambda_args(any, Line) -> {type,Line,any};
to_lambda_args(Args, Line) -> to_func_prod(Args, Line).

%% check_type_def(Def, KnownTypes, TypeVars) ->
%%     {ok,TypeVars} | {error,Error,TypeVars}.
%%  Check a type definition. TypeVars is an orddict of variable names
%%  and usage counts. Errors returned are:
%%  {bad_type,Type}     - error in the type definition
%%  {undefined_type,Type} - referring to an undefined type

%% Our special cases.
check_type_def(['UNION'|Types], Kts, Tvs) ->
    check_type_defs(Types, Kts, Tvs);
check_type_def([range,I1,I2], _Kts, Tvs) ->
    if is_integer(I1) and is_integer(I2) and (I1 =< I2) ->
            {ok,Tvs};
       true -> type_syntax_error(range, Tvs)
    end;
check_type_def([tuple|Ts], Kts, Tvs) ->
    check_type_defs(Ts, Kts, Tvs);
check_type_def([bitstring,I1,I2], _Kts, Tvs) ->
    if is_integer(I1) and is_integer(I2) and (I1 >= 0) and (I2 >= 0) ->
            {ok,Tvs};
       true -> type_syntax_error(bitstring, Tvs)
    end;
check_type_def([map|Pairs], Kts, Tvs) ->
    check_map_pairs(Pairs, Kts, Tvs);
check_type_def([record,Name|Fields], Kts, Tvs) ->
    if is_atom(Name) -> check_record_fields(Fields, Kts, Tvs);
       true -> type_syntax_error(record, Tvs)
    end;
check_type_def([lambda,Args,Ret], Kts, Tvs0) ->
    case check_lambda_args(Args, Kts, Tvs0) of
        {ok,Tvs1} -> check_type_def(Ret, Kts, Tvs1);
        Error -> Error
    end;
check_type_def(?Q(Val), _Kts, Tvs) -> check_type_lit(Val, Tvs);
check_type_def([call,?Q(M),?Q(T)|Args], Kts, Tvs) when is_atom(M), is_atom(T) ->
    check_type_defs(Args, Kts, Tvs);
%% The standard Erlang types.
check_type_def([Type|Args], Kts, Tvs0) when is_atom(Type) ->
    case check_type_defs(Args, Kts, Tvs0) of
        {ok,Tvs1} ->
            case string:tokens(atom_to_list(Type), ":") of
                [_M,_T] -> {ok,Tvs1};           %Remote so we just accept it
                _ ->
                    Arity = length(Args),       %It's a proper list
                    case lists:member({Type,Arity}, Kts)
                        or is_predefined_type(Type, Arity) of
                        true -> {ok,Tvs1};
                        false -> undefined_type_error(Type, Arity, Tvs1)
                    end
            end;
        Error -> Error
    end;
%% Only integers and atoms (type variables) legally left now.
check_type_def(Val, _Kts, Tvs) when is_integer(Val) -> {ok,Tvs};
check_type_def(Val, _Kts, Tvs) when is_atom(Val) ->
    %% It's a type variable.
    {ok,orddict:update_counter(Val, 1, Tvs)};
check_type_def(Def, _Kts, Tvs) ->
    bad_type_error(Def, Tvs).

check_type_defs(Defs, Kts, Tvs) ->
    check_type_list(fun check_type_def/3, Defs, Kts, Tvs).

check_type_lit(Val, Tvs) when is_integer(Val) ; is_atom(Val) -> {ok,Tvs};
check_type_lit(Val, Tvs) -> bad_type_error(Val, Tvs).

check_map_pairs(Pairs, Kts, Tvs) ->
    check_type_list(fun check_map_pair/3, Pairs, Kts, Tvs).

check_map_pair([K,V], Kts, Tvs0) ->
    case check_type_def(K, Kts, Tvs0) of
        {ok,Tvs1} -> check_type_def(V, Kts, Tvs1);
        Error -> Error
    end;
check_map_pair(Other, _Kts, Tvs) ->
    bad_type_error(Other, Tvs).

check_record_fields(Fs, Kts, Tvs) ->
    check_type_list(fun check_record_field/3, Fs, Kts, Tvs).

check_record_field([F,T], Kts, Tvs) when is_atom(F) ->
    check_type_def(T, Kts, Tvs);
check_record_field(Other, _Kts, Tvs) ->
    bad_type_error(Other, Tvs).

check_lambda_args(any, _Kts, Tvs) -> {ok,Tvs};
check_lambda_args(Args, Kts, Tvs) ->
    check_type_defs(Args, Kts, Tvs).

check_type_list(Check, [E|Es], Kts, Tvs0) ->
    case Check(E, Kts, Tvs0) of
        {ok,Tvs1} -> check_type_list(Check, Es, Kts, Tvs1);
        Error -> Error
    end;
check_type_list(_Check, [], _Kts, Tvs) -> {ok,Tvs};
check_type_list(_Check, Other, _Kts, Tvs) ->     %Not a proper list
    bad_type_error(Other, Tvs).

bad_type_error(Type, Tvs) -> {error,{bad_type,Type},Tvs}.

type_syntax_error(Type, Tvs) -> {error,{type_syntax,Type},Tvs}.

undefined_type_error(Type, Ar, Tvs) -> {error,{undefined_type,{Type,Ar}},Tvs}.

%% from_func_spec_list([FuncType]) -> Type.

from_func_spec_list(Ss) ->
    Fun = fun ({type,_L,'fun',_}=Type) ->
                  from_func_spec(Type) ++ [[]];
              ({type,_L,bounded_fun,[Fun,Cs]}) ->
                  from_func_spec(Fun) ++ [from_func_constraints(Cs)]
          end,
    lists:map(Fun, Ss).

from_func_spec({type,_L,'fun',[Prod,Ret]}) ->
    [from_func_prod(Prod),from_type_def(Ret)].

from_func_prod({type,_L,product,Args}) when is_list(Args) ->
    from_type_defs(Args).                       %Function arguments

from_func_constraint({type,_,constraint,[{atom,_,is_subtype},St]}) ->
    from_subtype(St).

from_func_constraints(Cs) ->
    lists:map(fun from_func_constraint/1, Cs).

from_subtype([{var,_,Var},Type]) -> [Var,from_type_def(Type)].

%% to_func_spec_list(Type, Line) -> AST.

to_func_spec_list(Fts, Line) ->
    lists:map(fun (Ft) -> to_func_spec(Ft, Line) end, Fts).

to_func_spec([Prod,Ret], Line) ->
    to_func_spec(Prod, Ret, Line);
to_func_spec([Prod,Ret,[]], Line) ->            %Future proof
    to_func_spec(Prod, Ret, Line);
to_func_spec([Prod,Ret,Cs], Line) ->
    Fun = to_func_spec(Prod, Ret, Line),
    Constr = to_func_constraints(Cs, Line),
    {type,Line,bounded_fun,[Fun,Constr]}.

to_func_spec(Prod, Ret, Line) ->
    {type,Line,'fun',[to_func_prod(Prod, Line),to_type_def(Ret, Line)]}.

to_func_prod(Args, Line) ->
    {type,Line,product,to_type_defs(Args, Line)}.

to_func_constraints(Cs, Line) ->
    [ to_func_constraint(C, Line) || C <- Cs ].

to_func_constraint([Var,Type], Line) ->
    {type,Line,constraint,[{atom,Line,is_subtype},
                           [{var,Line,Var},to_type_def(Type, Line)]]}.

%% check_func_spec_list([FuncType], Arity, KnownTypes, TypeVars) ->
%%     {ok,[TypeVars]} | {error,Error,[TypeVars]}.
%%  Check a list of function specs. TypeVars is an orddict of variable
%%  names and usage counts. Errors returned are:
%%  {bad_spec,Spec}     - error in the type definition

check_func_spec_list(Ss, Ar, Kts) ->
    check_spec_list(fun check_func_spec/3, Ss, Ar, Kts).

check_func_spec([Prod,Ret], Ar, Kts) ->
    check_func_spec([Prod,Ret,[]], Ar, Kts);
check_func_spec([Prod,Ret,Cs], Ar, Kts) ->
    Tvs0 = [],
    case check_func_prod(Prod, Ar, Kts, Tvs0) of
        {ok,Tvs1} ->
            case check_type_def(Ret, Kts, Tvs1) of
                {ok,Tvs2} ->
                    check_func_constraints(Cs, Kts, Tvs2);
                Error -> Error
            end;
        Error -> Error
    end;
check_func_spec(Other, _Ar, _Kts) ->
    bad_spec_error(Other, []).

check_func_prod(Args, Ar, Kts, Tvs0) ->
    %% This checks both the list and the types.
    case check_type_defs(Args, Kts, Tvs0) of
        {ok,Tvs1} ->
            if length(Args) =:= Ar -> {ok,Tvs1};
               true -> bad_spec_error(Args, Tvs1)
            end;
        Error -> Error
    end.

check_func_constraints([[Var,Type]|Cs], Kts, Tvs0) when is_atom(Var) ->
    Tvs1 = orddict:update_counter(Var, 1, Tvs0),
    case check_type_def(Type, Kts, Tvs1) of
        {ok,Tvs2} -> check_func_constraints(Cs, Kts, Tvs2);
        Error -> Error
    end;
check_func_constraints([], _Kts, Tvs) -> {ok,Tvs};
check_func_constraints(Other, _Kts, Tvs) ->
    bad_spec_error(Other, Tvs).

check_spec_list(Check, Es, Ar, Kts) ->
    check_spec_list(Check, Es, Ar, Kts, []).

check_spec_list(Check, [E|Es], Ar, Kts, Tvss) ->
    case Check(E, Ar, Kts) of
        {ok,Tvs} -> check_spec_list(Check, Es, Ar, Kts, Tvss ++ [Tvs]);
        Error -> Error
    end;
check_spec_list(_Check, [], _Ar, _Kts, Tvss) -> {ok,Tvss};
check_spec_list(_Check, Other, _Ar, _Kts, Tvss) ->
    %% Not a proper list.
    bad_spec_error(Other, Tvss).

bad_spec_error(Val, Tvs) -> {error,{bad_spec,Val},Tvs}.

%% is_predefined_type(Name, Arity) -> bool().
%%  Check whether Name/Arity is a predefined type.

is_predefined_type('UNION', Ar) -> is_integer(Ar) and (Ar >= 0);
is_predefined_type(call, Ar) -> is_integer(Ar) and (Ar >= 0);
is_predefined_type(lambda, Ar) -> is_integer(Ar) and (Ar >= 0);
is_predefined_type(map, Ar) -> is_integer(Ar) and (Ar >= 0);
is_predefined_type(range, 2) -> true;
is_predefined_type(bitstring, 2) -> true;
is_predefined_type(tuple, Ar) -> is_integer(Ar) and (Ar >= 0);
is_predefined_type(Name, Arity) ->
    erl_internal:is_type(Name, Arity).
