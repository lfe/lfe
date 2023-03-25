%% Copyright (c) 2008-2020 Robert Virding
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

-export([expand/1,expand/2,format_error/1]).

-import(lists, [foldr/3,mapfoldl/3]).

-include("lfe.hrl").

%% ets:test_ms/2.

%% format_error(Error) -> ErrorString.

format_error(match_spec_head) -> "Illegal number of head arguments".

-record(ms, {dc=1,                              %Dollar variable count from 1
             bs=[],                             %Variable/$var bindings
             dialect=ets,                       %Which dialect are we doing
             where=guard                        %Where in spec head/guard/body
            }).

%% expand(MSBody) -> Expansion.
%% expand(Dialect, MSBody) -> Expansion.
%%  Expand the match spec body.

expand(Cls) -> expand(table, Cls).

expand(Dialect, Cls) when Dialect =:= table ; Dialect =:= trace ->
    case catch clauses(Cls, #ms{dialect=Dialect}) of
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
            head_pattern(Pat, St2);
        [['=',Pat,S]] when is_atom(S) ->
            St2 = new_binding(S, '$_', St1),
            head_pattern(Pat, St2);
        [Pat] -> head_pattern(Pat, St1);
        _ -> throw({error,{match_spec_head,Pats}})     %Wrong size
    end.

%% head_pattern(Pattern, State) -> {Pattern,State}.
%%  Check the head pattern has the right format for the dialect.

head_pattern(Pat, St) ->                        %Just a variable
    check_head(Pat, St#ms.dialect),             %Correct format
    pattern(Pat, St).

check_head(Pat, _) when is_atom(Pat) -> ok;     %Variable
check_head(Pat, table) when is_tuple(Pat) -> ok;
check_head(?Q(Pat), table) when is_tuple(Pat) -> ok;
check_head([tuple|_], table) -> ok;
check_head(['record'|_], table) -> ok;
%% make-record has been deprecated but we sill accept it for now.
check_head(['make-record'|_], table) -> ok;
check_head(?Q(Pat), trace) when is_list(Pat) -> ok;
check_head([list|_], trace) -> ok;
check_head([cons|_], trace) -> ok;
check_head([], trace) -> ok;
check_head(Pat, _Type) ->
    throw({error,{match_spec_head,Pat}}).

%% pattern(Pattern, State) -> {Pattern,State}.

pattern('_', St) -> {?Q('_'),St};
pattern(Symb, St0) when is_atom(Symb) ->        %Variable
    {Dv,St1} = pat_binding(Symb, St0),
    {?Q(Dv),St1};
pattern(?Q(_)=E, St) -> {E,St};
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
pattern(['=',L0,R0], St0) ->                    %General aliasing
    {L1,St1} = pattern(L0, St0),
    {R1,St2} = pattern(R0, St1),
    {['=',L1,R1],St2};
pattern(['record',R|Fs0], St0) ->
    %% This is in a term but is going to be used as a pattern!
    {Fs1,St1} = pat_rec_fields(Fs0, St0),
    {['record',R|Fs1],St1};
%% make-record has been deprecated but we sill accept it for now.
pattern(['make-record',R|Fs], St) ->
    pattern(['record',R|Fs], St);
pattern(['record-index',R,F], St) ->
    {['record-index',R,F],St};
%% Support old no constructor style list forms.
pattern([H0|T0], St0) ->
    {H1,St1} = pattern(H0, St0),
    {T1,St2} = pattern(T0, St1),
    {[H1,T1],St2};
pattern(E, St) -> {E,St}.                       %Atomic

pat_list(Ps, St) -> mapfoldl(fun pattern/2, St, Ps).

%% pat_rec_fields(Fields, State) -> {Patterns,State}.
%% pat_rec_fields(Fields, DefValue, State) -> {Patterns,State}.
%%  Expand the record pattern fields adding a _ = '_' to set default
%%  value for fields.

pat_rec_fields(Fs, St) ->
    pat_rec_fields(Fs, ?Q('_'), St).

pat_rec_fields(['_',P|Fs], _DefV, St) ->
    pat_rec_fields(Fs, P, St);
pat_rec_fields([F,P0|Fs0], DefV, St0) when is_atom(F) ->
    %% Field names go straight through untouched.
    {P1,St1} = pattern(P0, St0),
    {Fs1,St2} = pat_rec_fields(Fs0, DefV, St1),
    {[F,P1|Fs1],St2};
pat_rec_fields([F0,P0|Fs0], DefV, St0) ->
    {F1,St1} = pattern(F0, St0),
    {P1,St2} = pattern(P0, St1),
    {Fs1,St3} = pat_rec_fields(Fs0, DefV, St2),
    {[F1,P1|Fs1],St3};
pat_rec_fields([], DefV, St) ->
    {['_',DefV],St}.

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
expr(?Q(A)=E, St) when is_atom(A) ->            %Atom
    case atom_to_list(A) of
        [$$|_] -> {[tuple,?Q(const),E],St};     %Catch dollar variables
        _ -> {E,St}
    end;
expr(?Q(T), St) when is_tuple(T) ->             %Must tuple tuples
    {[tuple,T],St};
expr(?Q(_)=E, St) -> {E,St};                    %No need for {const,E}?
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
%% Record special forms.
expr(['record',Name|Fs], St0) ->
    %% This is in a term and is going to be used as an expression!
    {Efs,St1} = expr_rec_fields(Fs, St0),
    {[tuple,['record',Name|Efs]],St1};          %Must tuple tuples
%% make-record has been deprecated but we sill accept it for now.
expr(['make-record',Name|Fs], St) ->
    expr(['record',Name|Fs], St);
expr(['is-record',E,Name], St0) ->
    {Ee,St1} = expr(E, St0),
    %% io:format(user, "is-record ~p ~p\n", [E,Name]),
    {[tuple,?Q('is_record'),Ee,?Q(Name)],St1};
    %% {[tuple,['is-record',Ee,Name]],St1};
expr(['record-index',Name,F], St) ->
    {['record-index',Name,F],St};
expr(['record-field',E,Name,F], St0) ->
    %% We must remove all checks and return simple call to element/2.
    {Ee,St1} = expr(E, St0),
    {[tuple,?Q(element),['record-index',Name,F],Ee],St1};
    %% {[tuple,['record-field',Ee,Name,F]],St1};
expr(['record-update',E,Name|Fs], St0) ->
    %% We must remove all checks and return simple nested setelement/3 calls.
    {Ee,St1} = expr(E, St0),
    {Efs,St2} = expr_rec_fields(Fs, St1),
    Set = expr_set_record(Efs, Ee, Name),
    {Set,St2};
    %% {[tuple,['record-update',Ee,Name|Efs]],St2};
%% Special match spec calls.
expr([bindings], St) -> {?Q('$*'),St};          %Special calls
expr([object], St) -> {?Q('$_'),St};
%% General function calls.
expr([call,?Q(erlang),?Q(Op)|Es0], St0) when is_atom(Op) ->
    Ar = length(Es0),
    case is_ms_erlang_func(Op, Ar, St0#ms.where) of
        true ->
            {Es1,St1} = expr_list(Es0, St0),
            {[tuple,?Q(Op)|Es1],St1};
        false -> illegal_func_error({erlang,Op,Ar})
    end;
expr([call,M,F|As], _St) ->
    illegal_func_error({M,F,length(As)});
expr([Op|Es0], St0) when is_atom(Op) ->
    Ar = length(Es0),
    case is_ms_func(Op, Ar, St0#ms.where) of    %Need to know where we are!
        true ->
            {Es1,St1} = expr_list(Es0, St0),
            {[tuple,?Q(Op)|Es1],St1};
        false -> illegal_func_error({Op,Ar})
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

%% expr_rec_fields(Fields, State) -> {Patterns,State}.

expr_rec_fields([F,V0|Fs0], St0) when is_atom(F) ->
    %% Field names go straight through untouched.
    {V1,St1} = expr(V0, St0),
    {Fs1,St2} = expr_rec_fields(Fs0, St1),
    {[F,V1|Fs1],St2};
expr_rec_fields([F0,V0|Fs0], St0) ->
    {F1,St1} = expr(F0, St0),
    {V1,St2} = expr(V0, St1),
    {Fs1,St3} = expr_rec_fields(Fs0, St2),
    {[F1,V1|Fs1],St3};
expr_rec_fields([], St) -> {[],St}.

%% expr_set_record(Fields, Expr, Record) -> SetRec.

expr_set_record([F,V|Fs], E0, R) ->
    E1= [tuple,?Q(setelement),['record-index',R,F],E0,V],
    expr_set_record(Fs, E1, R);
expr_set_record([], E, _) -> E.

is_integer_list([I|Is]) when is_integer(I) ->
    is_integer_list(Is);
is_integer_list([]) -> true;
is_integer_list(_) -> false.

illegal_func_error(Func) ->
    throw({error,{illegal_ms_func,Func}}).

%% We are very explicit in what operators and functions are allowed.

is_ms_test(is_atom,1) -> true;
is_ms_test(is_float,1) -> true;
is_ms_test(is_integer,1) -> true;
is_ms_test(is_list,1) -> true;
is_ms_test(is_number,1) -> true;
is_ms_test(is_pid,1) -> true;
is_ms_test(is_port,1) -> true;
is_ms_test(is_reference,1) -> true;
is_ms_test(is_tuple,1) -> true;
is_ms_test(is_map,1) -> true;
is_ms_test(is_binary,1) -> true;
is_ms_test(is_function,1) -> true;
is_ms_test(is_record,2) -> true;
is_ms_test(is_record,3) -> true;                %We get this one directly
is_ms_test(is_seq_trace,0) -> true;
is_ms_test(_,_) -> false.

is_erl_guard(abs,1) -> true;
is_erl_guard(element,2) -> true;
is_erl_guard(hd,1) -> true;
is_erl_guard(length,1) -> true;
is_erl_guard(node,0) -> true;
is_erl_guard(node,1) -> true;
is_erl_guard(round,1) -> true;
is_erl_guard(size,1) -> true;
is_erl_guard(map_size,1) -> true;
is_erl_guard(tl,1) -> true;
is_erl_guard(trunc,1) -> true;
is_erl_guard(self,0) -> true;
is_erl_guard(float,1) -> true;
is_erl_guard(_,_) -> false.

is_ms_guard(get_tcw, 0) -> true;                %MS pseudo guard function
is_ms_guard(N, A) -> is_erl_guard(N, A).

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

is_ms_bool('and',2) -> true;
is_ms_bool('or',2) -> true;
is_ms_bool('xor',2) -> true;
is_ms_bool('not',1) -> true;
is_ms_bool('andalso',2) -> true;
is_ms_bool('orelse',2) -> true;
is_ms_bool(_,_) -> false.

is_ms_arith('+',1) -> true;
is_ms_arith('+',2) -> true;
is_ms_arith('-',1) -> true;
is_ms_arith('-',2) -> true;
is_ms_arith('*',2) -> true;
is_ms_arith('/',2) -> true;
is_ms_arith('div',2) -> true;
is_ms_arith('rem',2) -> true;
is_ms_arith('band',2) -> true;
is_ms_arith('bor',2) -> true;
is_ms_arith('bxor',2) -> true;
is_ms_arith('bnot',1) -> true;
is_ms_arith('bsl',2) -> true;
is_ms_arith('bsr',2) -> true;
is_ms_arith(_,_) -> false.

is_ms_comp('>',2) -> true;
is_ms_comp('>=',2) -> true;
is_ms_comp('<',2) -> true;
is_ms_comp('=<',2) -> true;
is_ms_comp('==',2) -> true;
is_ms_comp('=:=',2) -> true;
is_ms_comp('/=',2) -> true;
is_ms_comp('=/=',2) -> true;
is_ms_comp(_,_) -> false.

is_ms_op(Op, Ar) ->
    is_ms_bool(Op, Ar) orelse is_ms_arith(Op, Ar) orelse is_ms_comp(Op, Ar).

is_ms_erlang_func(N, A, _) ->
    is_erl_guard(N, A) orelse is_ms_test(N, A) orelse is_ms_bool(N, A) orelse
        is_ms_arith(N, A) orelse is_ms_comp(N, A).

%% is_ms_func(Name, Arity, Where) -> bool().
%%  Test if Name/Arity is legal function in Where (guard/body).

is_ms_func(N, A, body) ->
    is_ms_action(N, A) orelse is_ms_guard(N, A) orelse is_ms_test(N, A) orelse
        is_ms_op(N, A);
is_ms_func(N, A, guard) ->
    is_ms_guard(N, A) orelse is_ms_test(N, A) orelse is_ms_op(N, A).

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
