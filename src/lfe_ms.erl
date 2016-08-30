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

%% File    : lfe_ms.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang match specification expander.

%% Expand match specification into vanilla compatible data
%% structure. We assume that all macros in the match spec have
%% already been expanded. These functions are intended to be used
%% within macros so they return code which when evaluated return the
%% match-spec.
%%
%% Note that the vanilla match spec expander starts numbering dollar
%% variables from 1. We do the same to be compatible.

-module(lfe_ms).

-export([expand/1,format_error/1]).

-import(lists, [foldr/3,mapfoldl/3]).

%% ets:test_ms/2.

%% format_error(Error) -> ErrorString.

format_error(match_spec_head) -> "Illegal number of head arguments".

-define(Q(E), [quote,E]).                       %We do a lot of quoting!

-record(ms, {dc=1,                              %Dollar variable count from 1
             bs=[],                             %Variable/$var bindings
             where=guard                        %Where in spec head/guard/body
            }).

%% expand(MSBody) -> Expansion.
%%  Expand the match spec body.

expand(Cls) ->
    case catch clauses(Cls, #ms{}) of
        {error,E} -> error(E);                  %Signals errors
        {'EXIT',E} -> error(E);                 %Signals errors
        {Exp,_} -> Exp                          %Hurrah it worked
    end.

%% clauses(MSClauses, State) -> {Patterns,State}.

clauses([Cl0|Cls0], St0) ->
    {Cl1,St1} = clause(Cl0, St0),
    {Cls1,St2} = clauses(Cls0, St1),
    {[cons,Cl1,Cls1],St2};
clauses([], St) -> {[],St}.

%% clause(ClauseBody, State) -> {{Head,Guard,Body},State}.

clause([H0,['when'|G0]|B0], St0) ->
    St1 = St0#ms{dc=1,where=guard,bs=[]},    %Reset clause local data
    {H1,St2} = head(H0, St1),
    {G1,St3} = guard(G0, St2),
    {B1,St4} = body(B0, St3),
    {[tuple,H1,G1,B1],St4};
clause([H0|B0], St0) ->
    St1 = St0#ms{dc=1,where=guard,bs=[]},    %Reset clause local data
    {H1,St2} = head(H0, St1),
    {B1,St3} = body(B0, St2),
    {[tuple,H1,[],B1],St3}.

%% head(Patterns, State) -> {Pattern,State}.
%%  Expand a head which can only consist of one argument. Only allow
%%  aliasing at the top-level and only to a variable.

head(Pats, St0) ->
    St1 = St0#ms{where=head},                   %We are now in the head
    case Pats of                                %Test for top-level aliasing
        [['=',S,Pat]] when is_atom(S) ->
            St2 = new_binding(S, '$_', St1),
            pattern(Pat, St2);
        [['=',Pat,S]] when is_atom(S) ->
            St2 = new_binding(S, '$_', St1),
            pattern(Pat, St2);
        [Pat] -> pattern(Pat, St1);
        _ -> throw({error,match_spec_head})     %Wrong size
    end.

pattern('_', St) -> {?Q('_'),St};
pattern(Symb, St0) when is_atom(Symb) ->        %Variable
    {Dv,St1} = pat_binding(Symb, St0),
    {?Q(Dv),St1};
pattern([quote,_]=E, St) -> {E,St};
pattern([cons,H0,T0], St0) ->
    {H1,St1} = pattern(H0, St0),
    {T1,St2} = pattern(T0, St1),
    {[cons,H1,T1],St2};
pattern([list|Ps0], St0) ->
    {Ps1,St1} = pat_list(Ps0, St0),
    {[list|Ps1],St1};
pattern([tuple|Ps0], St0) ->
    {Ps1,St1} = pat_list(Ps0, St0),
    {[tuple|Ps1],St1};
%% Support old no constructor style list forms.
pattern([H0|T0], St0) ->
    {H1,St1} = pattern(H0, St0),
    {T1,St2} = pattern(T0, St1),
    {[H1,T1],St2};
pattern(E, St) -> {E,St}.                       %Atomic

pat_list(Ps, St) -> mapfoldl(fun pattern/2, St, Ps).

%% pat_binding(Var, Status) -> {DVar,Status}.
%%  Get dollar var for variable, creating a new one if neccessary.

pat_binding(Var, St0) ->
    case find_binding(Var, St0) of
        {ok,Dv} -> {Dv,St0};
        error ->
            {Dv,St1} = new_dollar(St0),
            {Dv,new_binding(Var, Dv, St1)}
    end.

%% guard(Tests, State) -> {Tests,State}.
%% body(Tests, State) -> {Tests,State}.
%%  The expression translation in the same except for which
%%  expressions/tests are allowed. We use the same functions but carry
%%  a 'where' field in the State to separate them.

guard(Ts, St0) ->
    St1 = St0#ms{where=guard},
    exprs(Ts, St1).

body(Es, St0) ->
    St1 = St0#ms{where=body},
    exprs(Es, St1).

%% exprs(Es, State) -> {Conses,State}.
%% expr(E, State) -> {E,State}.

exprs([E0|Es0], St0) ->
    {E1,St1} = expr(E0, St0),
    {Es1,St2} = exprs(Es0, St1),
    {[cons,E1,Es1],St2};
exprs([], St) -> {[],St}.

expr(S, St) when is_atom(S) ->                  %Variable
    case find_binding(S, St) of
        {ok,Dv}  -> {?Q(Dv),St};                %Head variable
        error -> {S,St}                         %Free variable, need binding
    end;
expr([quote,A]=E, St) when is_atom(A) ->        %Atom
    case atom_to_list(A) of
        [$$|_] -> {[tuple,?Q(const),E],St};     %Catch dollar variables
        _ -> {E,St}
    end;
expr([quote,T], St) when is_tuple(T) ->         %Must tuple tuples
    {[tuple,T],St};
expr([quote,_]=E, St) -> {E,St};                %No need for {const,E}?
expr([cons,H0,T0], St0) ->
    {H1,St1} = expr(H0, St0),
    {T1,St2} = expr(T0, St1),
    {[cons,H1,T1],St2};
expr([list|Es0], St0) ->
    {Es1,St1} = expr_list(Es0, St0),
    {[list|Es1],St1};
expr([tuple|Es0], St0) ->                       %Must tuple tuples
    {Es1,St1} = expr_list(Es0, St0),
    {[tuple,[tuple|Es1]],St1};                  %Yes this is what it is
expr([binary|Segs0], St0) ->
    {Segs1,St1} = expr_bitsegs(Segs0, St0),
    {[binary|Segs1],St1};
%% Special match spec calls.
expr([bindings], St) -> {?Q('$*'),St};          %Special calls
expr([object], St) -> {?Q('$_'),St};
%% General function calls.
expr([call,?Q(erlang),?Q(Op)|Es0], St0) when is_atom(Op) ->
    Ar = length(Es0),
    case is_ms_erlang_func(Op, Ar) of
        true ->
            {Es1,St1} = expr_list(Es0, St0),
            {[tuple,?Q(Op)|Es1],St1};
        false -> throw({error,{illegal_ms_func,{erlang,Op,Ar}}})
    end;
expr([Op|Es0], St0) when is_atom(Op) ->
    Ar = length(Es0),
    case is_ms_func(Op, Ar, St0#ms.where) of    %Need to know where we are!
        true ->
            {Es1,St1} = expr_list(Es0, St0),
            {[tuple,?Q(Op)|Es1],St1};
        false -> throw({error,{illegal_ms_func,{Op,Ar}}})
    end;
expr([_|_], _) -> throw({error,illegal_ms_call});
expr([], St) -> {[],St};
expr(T, St) when is_tuple(T) ->                 %Must tuple tuples
    {[tuple,T],St};
expr(E, St) -> {E,St}.                          %Atomic

expr_list(Es, St) -> mapfoldl(fun expr/2, St, Es).

expr_bitsegs(Ss, St) -> mapfoldl(fun expr_bitseg/2, St, Ss).

expr_bitseg([Val0|Specs0]=F, St0) ->
    case is_integer_list(F) of
        true -> {F,St0};
        false ->
            {Specs1,St1} = expr_bitspecs(Specs0, St0),
            case is_integer_list(Val0) of
                true -> {[Val0|Specs1],St1};
                false ->
                    {Val1,St2} = expr(Val0, St1),
                    {[Val1|Specs1],St2}
            end
    end;
expr_bitseg(Val, St) ->
    expr(Val, St).

expr_bitspecs(Specs, St) ->
    mapfoldl(fun ([size,Sz0], S0) ->
                     {Sz1,S1} = expr(Sz0, S0),
                     {[size,Sz1],S1};
                 (Sp, S) -> {Sp,S}
             end, St, Specs).

is_integer_list([I|Is]) when is_integer(I) ->
    is_integer_list(Is);
is_integer_list([]) -> true;
is_integer_list(_) -> false.

is_ms_erlang_func(N, A) ->
    is_ms_op(N, A) orelse is_ms_bif(N, A).

%% is_ms_func(Name, Arity, Where) -> bool().
%% Test if Name/Arity is legal function in Where (guard/body).

is_ms_func(N, A, guard) ->
    is_ms_op(N, A) orelse is_ms_bif(N, A) orelse is_ms_guard(N, A);
is_ms_func(N, A, body) ->
    is_ms_op(N, A) orelse is_ms_bif(N, A) orelse is_ms_action(N, A).

%% is_ms_guard(Name, Arity) -> bool().
%% is_ms_action(Name, Arity) -> bool().

is_ms_guard(get_tcw, 0) -> true;
is_ms_guard(_, _) -> false.

is_ms_action(caller, 0) -> true;
is_ms_action(disable_trace, 1) -> true;
is_ms_action(disable_trace, 2) -> true;
is_ms_action(display, 1) -> true;
is_ms_action(enable_trace, 1) -> true;
is_ms_action(enable_trace, 2) -> true;
is_ms_action(exception_trace, 0) -> true;
is_ms_action(get_seq_token, 0) -> true;
is_ms_action(process_dump,0) -> true;
is_ms_action(message, 1) -> true;
is_ms_action(return_trace, 0) -> true;
is_ms_action(set_seq_token, 2) -> true;
is_ms_action(set_tcw, 1) -> true;
is_ms_action(silent, 1) -> true;
is_ms_action(trace, 2) -> true;
is_ms_action(trace, 3) -> true;
is_ms_action(_, _) -> false.

%% is_ms_op(Name, Arity) -> bool().
%% Valid match-spec operators.

is_ms_op(Op, Ar) ->
    erl_internal:arith_op(Op, Ar)
    orelse erl_internal:bool_op(Op, Ar)
    orelse erl_internal:comp_op(Op, Ar).

%% is_ms_bif(Name, Arity) -> bool().
%% Valid match-spec bifs, both guard and body. All the standard ones
%% MINUS a few!

is_ms_bif(setelement, 3) -> true;        %Not true, dangerous!!!!!
is_ms_bif(bit_size, 1) -> false;
is_ms_bif(byte_size, 1) -> false;
is_ms_bif(tuple_size, 1) -> false;
is_ms_bif(binary_part, _) -> false;
is_ms_bif(N, Ar) ->
    erl_internal:guard_bif(N, Ar).

%% new_binding(Name, Value, State) -> State.
%% find_binding(Name, State) -> {ok,Value} | error.
%% fetch_binding(Name, State) -> Value.

new_binding(Var, Val, #ms{bs=Bs}=St) ->
    St#ms{bs=orddict:store(Var, Val, Bs)}.

find_binding(Var, #ms{bs=Bs}) ->
    orddict:find(Var, Bs).

%% fetch_binding(Var, #ms{bs=Bs}) ->
%%     orddict:fetch(Var, Bs).

new_dollar(St) ->
    C = St#ms.dc,
    {list_to_atom("$" ++ integer_to_list(C)),St#ms{dc=C+1}}.
