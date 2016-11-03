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
%% Erlang and LFE type syntaxes.
%%
%% We can correctly do most types except for maps where we lose the
%% distinction between assoc and exact pairs.

-module(lfe_types).

-export([format_error/1]).

-export([from_type_def/1,from_type_defs/1,to_type_def/2,to_type_defs/2,
         from_func_type_list/1,to_func_type_list/2]).

-export([check_type_def/3,check_type_defs/3]).

-export([is_predefined_type/2]).

-compile(export_all).

-include("lfe.hrl").

%% format_error(Error) -> String.
%%  Do we really need this here?

format_error({bad_type,T}) ->
    lfe_io:format1("bad type: ~w", [T]);
format_error({unknown_type,T}) ->
    lfe_io:format1("unknown type: ~w", [T]).

%% from_type_def(AST) -> Def.
%%  Translate an Erlang type definition to LFE. This takes the Erlang
%%  AST form of a type definition and translates to the LFE type
%%  syntax. No AST there of course.

from_type_def({type,_L,union,Types}) ->         %Special case union
    ['UNION'|from_type_defs(Types)];
from_type_def({type,_L,tuple,any}) -> [tuple];
from_type_def({type,_L,map,Pairs}) ->
    [map|from_map_pairs(Pairs)];
from_type_def({type,_L,record,[{atom,_L,Name}|Fields]}) ->
    [record,Name|from_rec_fields(Fields)];
from_type_def({type,_L,'fun',[Args,Ret]}) ->
    [lambda,from_lambda_args(Args),from_type_def(Ret)];
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

%% check_type_def(Def, KnownTypes, Parameters) -> ok | {error,Error}.
%%  Check a type definition. Errors returned are:
%%  {bad_type,Type}     - error in the type definition
%%  {unknown_type,Type} - referring to an unknown type

check_type_def(['UNION'|Types], Kts, Ps) ->
    check_type_defs(Types, Kts, Ps);
check_type_def([tuple|Ts], Kts, Ps) ->
    check_type_defs(Ts, Kts, Ps);
check_type_def([map|Pairs], Kts, Ps) ->
    check_map_pairs(Pairs, Kts, Ps);
check_type_def([record,Name|Fields], Kts, Ps) ->
    if is_atom(Name) -> check_record_fields(Fields, Kts, Ps);
       true -> bad_type_error(Name)
    end;
check_type_def([lambda,Args,Ret], Kts, Ps) ->
    case check_lambda_args(Args, Kts, Ps) of
        ok -> check_type_def(Ret, Kts, Ps);
        Error -> Error
    end;
check_type_def(?Q(Val), _Kts, _Ps) -> check_type_lit(Val);
check_type_def([call,?Q(M),?Q(T)|Args], Kts, Ps) when is_atom(M), is_atom(T) ->
    check_type_defs(Args, Kts, Ps);
check_type_def([Type|Args], Kts, Ps) when is_atom(Type) ->
    case check_type_defs(Args, Kts, Ps) of
        ok ->
            case string:tokens(atom_to_list(Type), ":") of
                [_M,_T] -> ok;                  %Remote so we just accept it
                _ ->
                    Arity = length(Args),       %It's a proper list
                    case lists:member({Type,Arity}, Kts)
                        or is_predefined_type(Type, Arity) of
                        true -> ok;
                        false -> unknown_type_error([Type,Arity])
                    end
            end;
        Error -> Error
    end;
%% Only integers legally left now.
check_type_def(Val, _Kts, _Ps) when is_integer(Val) -> ok;
check_type_def(Val, _Kts, Ps) ->
    case lists:member(Val, Ps) of
	true -> ok;
	false -> bad_type_error(Val)
    end.

check_type_defs(Defs, Kts, Ps) ->
    check_type_list(fun check_type_def/3, Defs, Kts, Ps).

check_type_lit(Val) when is_integer(Val) ; is_atom(Val) -> ok;
check_type_lit(Val) -> bad_type_error(Val).

check_map_pairs(Pairs, Kts, Ps) ->
    check_type_list(fun check_map_pair/3, Pairs, Kts, Ps).

check_map_pair([K,V], Kts, Ps) ->
    case check_type_def(K, Kts, Ps) of
        ok -> check_type_def(V, Kts, Ps);
        Error -> Error
    end;
check_map_pair(Other, _Kts, _Ps) ->
    bad_type_error(Other).

check_record_fields(Fs, Kts, Ps) ->
    check_type_list(fun check_record_field/3, Fs, Kts, Ps).

check_record_field([F,T], Kts, Ps) when is_atom(F) ->
    check_type_def(T, Kts, Ps);
check_record_field(Other, _Kts, _Ps) ->
    bad_type_error(Other).

check_lambda_args(any, _Kts, _Ps) -> ok;
check_lambda_args(Args, Kts, Ps) ->
    check_type_defs(Args, Kts, Ps).

check_type_list(Check, [E|Es], Kts, Ps) ->
    case Check(E, Kts, Ps) of
        ok -> check_type_list(Check, Es, Kts, Ps);
        Error -> Error
    end;
check_type_list(_Check, [], _Kts, _Ps) -> ok;
check_type_list(_Check, Other, _Kts, _Ps) ->    %Not a proper list
    bad_type_error(Other).

bad_type_error(Val) -> {error,{bad_type,Val}}.

unknown_type_error(Val) -> {error,{unknown_type,Val}}.

safe_length([_|Es], L) -> safe_length(Es, L+1);
safe_length([], L) -> L;
safe_length(_, _) -> -1.                        %Never matches

%% to_type_def(Def, Line) -> AST.
%%  Translate a type definition from the LFE type syntax to the Erlang
%%  AST.

to_type_def(['UNION'|Types], Line) ->                   %Union
    {type,Line,union,to_type_defs(Types, Line)};
to_type_def([tuple], Line) ->                   %Undefined tuple
    {type,Line,tuple,any};
to_type_def([tuple|Args], Line) ->
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

%% from_func_type_list([FuncType]) -> Type.

from_func_type_list(Ss) ->
    Fun = fun ({type,_L,'fun',_}=Type) ->
                  from_func_type(Type) ++ [[]];
              ({type,_L,bounded_fun,[Fun,Cs]}) ->
                  from_func_type(Fun) ++ [from_func_constraints(Cs)]
          end,
    lists:map(Fun, Ss).

from_func_type({type,_L,'fun',[Prod,Ret]}) ->
    [from_func_prod(Prod),from_type_def(Ret)].

from_func_prod({type,_L,product,Args}) when is_list(Args) ->
    from_type_defs(Args).                       %Function arguments

from_func_constraint({type,_,constraint,[{atom,_,is_subtype},St]}) ->
    from_subtype(St).

from_func_constraints(Cs) ->
    lists:map(fun from_func_constraint/1, Cs).

from_subtype([{var,_,Var},Type]) -> [Var,from_type_def(Type)].

%% to_func_type_list(Type, Line) -> AST.

to_func_type_list(Fts, Line) ->
    lists:map(fun (Ft) -> to_func_type(Ft, Line) end, Fts).

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

to_func_prod(Args, Line) ->
    {type,Line,product,to_type_defs(Args, Line)}.

to_func_constraints(Cs, Line) ->
    [ to_func_constraint(C, Line) || C <- Cs ].

to_func_constraint([Var,Type], Line) ->
    {type,Line,constraint,[{atom,Line,is_subtype},
                           [{var,Line,Var},to_type_def(Type, Line)]]}.

%% is_predefined_type(Name, Arity) -> bool().
%%  Check whether Name/Arity is a predefined type.

is_predefined_type('UNION', _) -> true;
is_predefined_type(call, _) -> true;
is_predefined_type(lambda, _) -> true;
is_predefined_type(Name, Arity) ->
    erl_internal:is_type(Name, Arity).
