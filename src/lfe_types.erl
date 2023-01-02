%% Copyright (c) 2016-2021 Robert Virding
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

-export([is_type_decl/1]).
-export([format_error/1]).

-export([from_type_def/1,from_type_defs/1,to_type_def/2,to_type_defs/2,
         check_type_def/3,check_type_defs/3]).

-export([from_func_spec_list/1,to_func_spec_list/2,
         check_func_spec_list/3]).

%% -compile(export_all).

-include("lfe.hrl").

%% format_error(Error) -> String.
%%  Do we really need this here?

format_error({bad_type_def,T}) ->
    lfe_io:format1("bad ~w type definition", [T]);
format_error({bad_type_syntax,T}) ->
    lfe_io:format1(<<"bad ~w type syntax">>, [T]);
format_error({bad_function_spec,S}) ->
    lfe_io:format1("bad function spec: ~w", [S]).

%% is_type_decl(Tag) -> boolean().
%%  Is Name a type declaration?

is_type_decl(type) -> true;
is_type_decl(opaque) -> true;
is_type_decl(_Other) -> false.

%% from_type_def(AST) -> Def.
%%  Translate an Erlang type definition to LFE. This takes the Erlang
%%  AST form of a type definition and translates to the LFE type
%%  syntax. No AST there of course.

%% Our special cases.
from_type_def({type,_L,union,Types}) ->         %Special case union
    ['UNION'|from_type_defs(Types)];
from_type_def({type,_L,tuple,any}) ->           %Special case tuple() -> (tuple)
    [tuple];
from_type_def({type,_L,tuple,Elems}) ->
    list_to_tuple(from_type_defs(Elems));
from_type_def({type,_L,binary,Bits}) when Bits =/= [] ->
    [bitstring|from_type_defs(Bits)];           %Flip binary<->bitstring here
%% from_type_def({type,_L,bitstring,[]}) -> [bitstring,[]];
from_type_def({type,_L,map,any}) ->             %Special case map() -> (map)
    [map];
from_type_def({type,_L,map,Pairs}) ->
    maps:from_list(from_map_pairs(Pairs));
from_type_def({type,_L1,record,[{atom,_L2,Name}|Fields]}) ->
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
%% Literal values.
from_type_def({var,_L,Var}) -> Var;             %A type variable
from_type_def({atom,_L,Atom}) -> ?Q(Atom);      %Literal atom
from_type_def({integer,_L,Int}) -> Int;         %Literal integer
from_type_def({float,_L,Float}) -> Float.       %Literal float

from_type_defs(Ts) ->
    lists:map(fun from_type_def/1, Ts).

from_map_pairs(Pairs) ->
    %% Lose distinction between assoc and exact pairs.
    Fun = fun ({type,_L,_P,[Kt,Vt]}) ->
                  {from_type_def(Kt),from_type_def(Vt)}
          end,
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
to_type_def([tuple], Line) ->                   %Special case (tuple) -> tuple()
    {type,Line,tuple,any};
to_type_def([tuple|Args], Line) ->              %Not a user defined type
    {type,Line,tuple,to_type_defs(Args, Line)};
to_type_def([map], Line) ->                     %Special case (map) -> map()
    {type,Line,map,any};
to_type_def([map|Elems], Line) ->
    {type,Line,map,to_map_pairs(to_pair_list(Elems), Line)};
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
to_type_def(Tup, Line) when is_tuple(Tup) ->
    {type,Line,tuple,to_type_defs(tuple_to_list(Tup), Line)};
to_type_def(Map, Line) when ?IS_MAP(Map) ->
    ToPairs = to_map_pairs(maps:to_list(Map), Line),
    {type,Line,map,ToPairs};
to_type_def(Val, Line) when is_integer(Val) ->  %Literal integer value
    to_lit(Val, Line);
to_type_def(Val, Line) when is_float(Val) ->    %Literal float value
    to_lit(Val, Line);
to_type_def(Val, Line) when is_atom(Val) ->     %Variable
    {var,Line,Val}.

to_type_defs(Ds, Line) ->
    lists:map(fun (D) -> to_type_def(D, Line) end, Ds).

to_lit(Val, Line) when is_atom(Val) -> {atom,Line,Val};
to_lit(Val, Line) when is_integer(Val) -> {integer,Line,Val};
to_lit(Val, Line) when is_float(Val) -> {float,Line,Val}.

to_pair_list([K,V|Rest]) ->
    [{K,V}|to_pair_list(Rest)];
to_pair_list([]) -> [].

to_map_pairs(Pairs, Line) ->
    %% Have lost distinction between assoc and exact pairs.
    Fun = fun ({K,V}) ->
                  {type,Line,map_field_assoc,to_type_defs([K,V], Line)}
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

%% check_type_defs(Defs, KnownRecords, TypeVars) ->
%%     {ok,TypeVars} | {error,Error,TypeVars}.
%% check_type_def(Def, KnownRecords, TypeVars) ->
%%     {ok,TypeVars} | {error,Error,TypeVars}.
%%  Check a type definition. TypeVars is an orddict of variable names
%%  and usage counts. Errors returned are:
%%  {bad_type_syntax,Type}  - error in the type syntax
%%  {bad_type_def,Type}     - error in the type definition

%% Our special cases.
check_type_def(['UNION'|Types], Recs, Tvs) ->
    check_type_defs(Types, Recs, Tvs);
check_type_def([range,I1,I2], _Recs, Tvs) ->
    if is_integer(I1) and is_integer(I2) and (I1 =< I2) ->
            {ok,Tvs};
       true -> bad_type_syntax_error(range, Tvs)
    end;
check_type_def([tuple|Ts], Recs, Tvs) ->
    check_type_defs(Ts, Recs, Tvs);
check_type_def([bitstring,I1,I2], _Recs, Tvs) ->
    if is_integer(I1) and is_integer(I2) and (I1 >= 0) and (I2 >= 0) ->
            {ok,Tvs};
       true -> bad_type_syntax_error(bitstring, Tvs)
    end;
check_type_def([map|Pairs], Recs, Tvs) ->
    check_map_pairs(Pairs, Recs, Tvs);
check_type_def([record,Name|Fields], Recs, Tvs) ->
    check_record(Name, Fields, Recs, Tvs);
    %% if is_atom(Name) -> check_record_fields(Fields, Recs, Tvs);
    %%    true -> bad_type_syntax_error(record, Tvs)
    %% end;
check_type_def([lambda,Args,Ret], Recs, Tvs0) ->
    case check_lambda_args(Args, Recs, Tvs0) of
        {ok,Tvs1} -> check_type_def(Ret, Recs, Tvs1);
        Error -> Error
    end;
check_type_def(?Q(Val), _Recs, Tvs) -> check_type_lit(Val, Tvs);
check_type_def([call,?Q(M),?Q(T)|Args], Recs, Tvs) when is_atom(M), is_atom(T) ->
    check_type_defs(Args, Recs, Tvs);
%% The standard Erlang types.
check_type_def([Type|Args], Recs, Tvs0) when is_atom(Type) ->
    check_type_defs(Args, Recs, Tvs0);
%% Only literal tuples,  maps, integers and atoms (type variables) left now.
check_type_def(Tup, Recs, Tvs) when is_tuple(Tup) ->
    check_type_defs(tuple_to_list(Tup), Recs, Tvs);
check_type_def(Map, Recs, Tvs) when ?IS_MAP(Map) ->
    ToPairs = fun ({K,V}) -> [K,V] end,           %Convert to list pairs
    check_map_pairs(lists:flatmap(ToPairs, maps:to_list(Map)), Recs, Tvs);
check_type_def(Val, _Recs, Tvs) when is_integer(Val) -> {ok,Tvs};
check_type_def(Val, _Recs, Tvs) when is_atom(Val) ->
    %% It's a type variable.
    {ok,orddict:update_counter(Val, 1, Tvs)};
check_type_def(Def, _Recs, Tvs) ->
    bad_type_def_error(Def, Tvs).

check_type_defs(Defs, Recs, Tvs) ->
    check_type_list(fun check_type_def/3, Defs, Recs, Tvs).

check_type_lit(Val, Tvs) when is_integer(Val) ; is_atom(Val) -> {ok,Tvs};
check_type_lit(Val, Tvs) -> bad_type_def_error(Val, Tvs).

check_map_pairs([K,V|Pairs], Recs, Tvs0) ->
    case check_map_pair(K, V, Recs, Tvs0) of
        {ok,Tvs1} ->
            check_map_pairs(Pairs, Recs, Tvs1);
        Error -> Error
    end;
check_map_pairs([], _Recs, Tvs) -> {ok,Tvs};
check_map_pairs(_Other, _Recs, Tvs) ->
    bad_type_syntax_error(map, Tvs).

check_map_pair(K, V, Recs, Tvs0) ->
    case check_type_def(K, Recs, Tvs0) of
        {ok,Tvs1} -> check_type_def(V, Recs, Tvs1);
        Error -> Error
    end.

%% check_record(Record, Fields, KnownRecords, TypeVars) ->
%%     {ok,TypeVars} | {error,Error,TypeVars}.

check_record(Name, Fields, Recs, Tvs) ->
    case orddict:is_key(Name, Recs) of
        true ->
            check_record_fields(Fields, Recs, Tvs);
        false ->
            if is_atom(Name) ->
                    undefined_record_error(Name, Tvs);
               true -> bad_type_syntax_error(record, Tvs)
            end
    end.

check_record_fields(Fs, Recs, Tvs) ->
    check_type_list(fun check_record_field/3, Fs, Recs, Tvs).

check_record_field([F,T], Recs, Tvs) when is_atom(F) ->
    check_type_def(T, Recs, Tvs);
check_record_field(Other, _Recs, Tvs) ->
    bad_type_def_error(Other, Tvs).

check_lambda_args(any, _Recs, Tvs) -> {ok,Tvs};
check_lambda_args(Args, Recs, Tvs) ->
    check_type_defs(Args, Recs, Tvs).

check_type_list(Check, [E|Es], Recs, Tvs0) ->
    case Check(E, Recs, Tvs0) of
        {ok,Tvs1} -> check_type_list(Check, Es, Recs, Tvs1);
        Error -> Error
    end;
check_type_list(_Check, [], _Recs, Tvs) -> {ok,Tvs};
check_type_list(_Check, Other, _Recs, Tvs) ->     %Not a proper list
    bad_type_def_error(Other, Tvs).

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

%% check_func_spec_list([FuncType], Arity, KnownRecords) ->
%%     {ok,[TypeVars]} | {error,Error,[TypeVars]}.
%% check_func_spec(FuncType, Arity, KnownRecords) ->
%%     {ok,TypeVars} | {error,Error,TypeVars}.
%%  Check a list of function specs. TypeVars is an orddict of variable
%%  names and usage counts. Errors returned are:
%%  {bad_function_spec,Spec}     - error in the type definition

check_func_spec_list(Ss, Ar, Recs) ->
    check_spec_list(fun check_func_spec/3, Ss, Ar, Recs).

check_func_spec([Prod,Ret], Ar, Recs) ->
    check_func_spec([Prod,Ret,[]], Ar, Recs);
check_func_spec([Prod,Ret,Cs], Ar, Recs) ->
    Tvs0 = [],
    case check_func_prod(Prod, Ar, Recs, Tvs0) of
        {ok,Tvs1} ->
            case check_type_def(Ret, Recs, Tvs1) of
                {ok,Tvs2} ->
                    check_func_constraints(Cs, Recs, Tvs2);
                Error -> Error
            end;
        Error -> Error
    end;
check_func_spec(Other, _Ar, _Recs) ->
    bad_function_spec_error(Other, []).

check_func_prod(Args, Ar, Recs, Tvs0) ->
    %% This checks both the list and the types.
    case check_type_defs(Args, Recs, Tvs0) of
        {ok,Tvs1} ->
            if length(Args) =:= Ar -> {ok,Tvs1};
               true -> bad_function_spec_error(Args, Tvs1)
            end;
        Error -> Error
    end.

check_func_constraints([[Var,Type]|Cs], Recs, Tvs0) when is_atom(Var) ->
    Tvs1 = orddict:update_counter(Var, 1, Tvs0),
    case check_type_def(Type, Recs, Tvs1) of
        {ok,Tvs2} -> check_func_constraints(Cs, Recs, Tvs2);
        Error -> Error
    end;
check_func_constraints([], _Recs, Tvs) -> {ok,Tvs};
check_func_constraints(Other, _Recs, Tvs) ->
    bad_function_spec_error(Other, Tvs).

check_spec_list(Check, Es, Ar, Recs) ->
    check_spec_list(Check, Es, Ar, Recs, []).

check_spec_list(Check, [E|Es], Ar, Recs, Tvss) ->
    case Check(E, Ar, Recs) of
        {ok,Tvs} -> check_spec_list(Check, Es, Ar, Recs, Tvss ++ [Tvs]);
        Error -> Error
    end;
check_spec_list(_Check, [], _Ar, _Recs, Tvss) -> {ok,Tvss};
check_spec_list(_Check, Other, _Ar, _Recs, Tvss) ->
    %% Not a proper list.
    bad_function_spec_error(Other, Tvss).

%% Return errors.

bad_function_spec_error(Val, Tvs) -> {error,{bad_function_spec,Val},Tvs}.

bad_type_def_error(Type, Tvs) -> {error,{bad_type_def,Type},Tvs}.

bad_type_syntax_error(Type, Tvs) -> {error,{bad_type_syntax,Type},Tvs}.

undefined_record_error(Rec, Tvs) -> {error,{undefined_record,Rec},Tvs}.
