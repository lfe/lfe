%% Copyright (c) 2008-2017 Robert Virding
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

%% File    : lfe_eval.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang interpreter.

%%% We follow Erlang here in many cases even though it is sometimes a
%%% bit strange. In a fun argument where when matching a binary we
%%% import the size of bitseg as a variable from the environment not
%%% just from earlier segments. No other argument variables are
%%% imported.

-module(lfe_eval).

-export([expr/1,expr/2,literal/1,literal/2,body/1,body/2,
         gexpr/1,gexpr/2,guard/1,guard/2,match/3,match_when/4,
         apply/2,apply/3,
         make_letrec_env/2,add_lexical_func/4,add_dynamic_func/4,
         format_error/1]).

%% Deprecated exports.
-export([eval/1,eval/2,eval_list/2]).

-import(lfe_env, [add_vbinding/3,add_vbindings/2,get_vbinding/2,
                  add_fbinding/4,add_fbindings/2,
                  add_ibinding/5]).

-import(lists, [reverse/1,all/2,map/2,foldl/3,foldr/3]).
-import(orddict, [find/2,fetch/2,store/3,is_key/2]).

-compile({no_auto_import,[apply/3]}).           %For our apply/3 function
-deprecated([eval/1,eval/2,eval_list/2]).

%% We do a lot of quoting!
-define(Q(E), [quote,E]).
-define(BQ(E), [backquote,E]).
-define(C(E), [comma,E]).
-define(C_A(E), ['comma-at',E]).

%% Define IS_MAP/1 macro for is_map/1 bif.
-ifdef(HAS_MAPS).
-define(IS_MAP(T), is_map(T)).
-else.
-define(IS_MAP(T), false).
-endif.

%% -compile([export_all]).

%% Errors.
format_error(badarg) -> <<"bad argument">>;
format_error({badmatch,Val}) ->
    lfe_io:format1(<<"bad match: ~w">>, [Val]);
format_error({unbound_symb,S}) ->
    lfe_io:format1(<<"symbol ~w is unbound">>, [S]);
format_error({undefined_func,{F,A}}) ->
    lfe_io:format1(<<"undefined function ~w/~w">>, [F,A]);
format_error(if_expression) -> <<"non-boolean if test">>;
format_error(function_clause) -> <<"no function clause matching">>;
format_error({case_clause,Val}) ->
    lfe_io:format1(<<"no case clause matching ~.P">>, [Val,10]);
format_error(illegal_guard) -> <<"illegal guard">>;
format_error(illegal_bitsize) -> <<"illegal bitsize">>;
format_error(illegal_bitseg) -> <<"illegal bitsegment">>;
format_error({illegal_pattern,Pat}) ->
    lfe_io:format1(<<"illegal pattern ~w">>, [Pat]);
format_error({illegal_literal,Lit}) ->
    lfe_io:format1(<<"illegal literal value ~w">>, [Lit]);
format_error({illegal_mapkey,Key}) ->
    lfe_io:format1(<<"illegal map key ~w">>, [Key]);
format_error(bad_arity) -> <<"arity mismatch">>;
format_error({argument_limit,Arity}) ->
    lfe_io:format1(<<"too many arguments ~w">>, [Arity]);
format_error({bad_form,Form}) ->
    lfe_io:format1(<<"bad ~w form">>, [Form]);
%% Everything we don't recognise or know about.
format_error(Error) ->
    lfe_io:prettyprint1(Error).

%% eval(Sexpr) -> Value.
%% eval(Sexpr, Env) -> Value.

eval(E) -> expr(E).

eval(E, Env) -> expr(E, Env).

%% expr(Sexpr) -> Value.
%% expr(Sexpr, Env) -> Value.
%%  Evaluate the sexpr, first expanding all macros.

expr(E) -> expr(E, lfe_env:new()).

expr(E, Env) ->
    Exp = lfe_macro:expand_expr_all(E, Env),
    %% lfe_io:fwrite("e: ~p\n", [{E,Exp,Env}]),
    eval_expr(Exp, Env).

%% literal(Literal) -> Value.
%% literal(Literal, Env) -> Value.
%% body(Body) -> Value.
%% body(Body, Env) -> Value.
%% gexpr(GuardTest) -> Value.
%% gexpr(GuardTest, Env) -> Value.
%% guard(Guard) -> true | false.
%% guard(Guard, Env) -> true | false.

literal(L) -> literal(L, lfe_env:new()).

literal(L, Env) -> eval_lit(L, Env).

body(B) -> body(B, lfe_env:new()).

body(B, Env) -> eval_body(B, Env).

gexpr(Gt) -> gexpr(Gt, lfe_env:new()).

gexpr(Gt, Env) -> eval_gexpr(Gt, Env).

guard(G) -> guard(G, lfe_env:new()).

guard(G, Env) -> eval_guard(G, Env).

%% apply(Function, Args) -> Expr.
%% apply(Function, Args, Env) -> Expr.
%%  This is applying interpreted Erlang functions, for applying funs
%%  use normal apply. Name scoping stops us from using apply/s
%%  internally. Args should already be evaluated.

apply(F, Args) ->
    apply(F, Args, lfe_env:new()).

apply(F, Args, Env) ->
    eval_apply_expr(F, Args, Env).              %Env at function def

%% eval_expr(Sexpr, Environment) -> Value.
%%  Evaluate a sexpr in the current environment. Try to catch core
%%  forms by just name and check arguments arguments later. Otherwise
%%  users can redefine core forms with different number of arguments.

%% Handle the Core data special forms.
eval_expr(?Q(E), _) -> E;
eval_expr([cons,H,T], Env) ->
    [eval_expr(H, Env)|eval_expr(T, Env)];
eval_expr([car,E], Env) -> hd(eval_expr(E, Env)); %Provide lisp names
eval_expr([cdr,E], Env) -> tl(eval_expr(E, Env));
eval_expr([list|Es], Env) -> eval_list(Es, Env);
eval_expr([tuple|Es], Env) -> list_to_tuple(eval_list(Es, Env));
eval_expr([tref,Tup,I], Env) ->
    element(eval_expr(I, Env), eval_expr(Tup, Env));
eval_expr([tset,Tup,I,V], Env) ->
    setelement(eval_expr(I, Env), eval_expr(Tup, Env), eval_expr(V, Env));
eval_expr([binary|Bs], Env) -> eval_binary(Bs, Env);
eval_expr([map|As], Env) ->
    Pairs = map_pairs(As, Env),
    maps:from_list(Pairs);
eval_expr(['mref',Map,K], Env) ->
    Key = map_key(K, Env),
    maps:get(Key, eval_expr(Map, Env));
eval_expr(['mset',M|As], Env) ->
    Map   = eval_expr(M, Env),
    Pairs = map_pairs(As, Env),
    foldl(fun maps_put/2, Map, Pairs);
eval_expr(['mupd',M|As], Env) ->
    Map   = eval_expr(M, Env),
    Pairs = map_pairs(As, Env),
    foldl(fun maps_update/2, Map, Pairs);
eval_expr(['map-get',Map,K], Env) ->
    eval_expr([mref,Map,K], Env);
eval_expr(['map-set',M|As], Env) ->
    eval_expr([mset,M|As], Env);
eval_expr(['map-update',M|As], Env) ->
    eval_expr([mupd,M|As], Env);
eval_expr([function,Fun,Ar], Env) ->
    %% Build a lambda which can be applied.
    Vs = new_vars(Ar),
    eval_lambda([lambda,Vs,[Fun|Vs]], Env);
eval_expr([function,M,F,Ar], _) ->
    erlang:make_fun(M, F, Ar);
%% Handle the Core closure special forms.
eval_expr([lambda|_]=Lambda, Env) ->
    eval_lambda(Lambda, Env);
eval_expr(['match-lambda'|_]=Mlambda, Env) ->
    eval_match_lambda(Mlambda, Env);
eval_expr(['let'|Body], Env) ->
    eval_let(Body, Env);
eval_expr(['let-function'|Body], Env) ->
    eval_let_function(Body, Env);
eval_expr(['letrec-function'|Body], Env) ->
    eval_letrec_function(Body, Env);
%% Handle the Core control special forms.
eval_expr(['progn'|Body], Env) ->
    eval_body(Body, Env);
eval_expr(['if'|Body], Env) ->
    eval_if(Body, Env);
eval_expr(['case'|Body], Env) ->
    eval_case(Body, Env);
eval_expr(['receive'|Body], Env) ->
    eval_receive(Body, Env);
eval_expr(['catch'|Body], Env) ->
    catch eval_body(Body, Env);
eval_expr(['try'|Body], Env) ->
    eval_try(Body, Env);
eval_expr([funcall,F|As], Env) ->
    eval_apply_expr(eval_expr(F, Env), eval_list(As, Env), Env);
eval_expr([call|Body], Env) ->
    eval_call(Body, Env);
%% General functions calls.
eval_expr([Fun|Es], Env) when is_atom(Fun) ->
    %% Note that macros have already been expanded here.
    Ar = length(Es),                            %Arity
    case get_fbinding(Fun, Ar, Env) of
        {yes,M,F} -> erlang:apply(M, F, eval_list(Es, Env));
        {yes,F} -> eval_apply(F, eval_list(Es, Env), Env);
        no -> undefined_func_error(Fun, Ar)
    end;
eval_expr([_|_]=S, _) ->                        %Test if string literal
    case is_posint_list(S) of
        true -> S;                              %It an "atomic" type
        false ->                                %It is a bad application form
            bad_form_error(application)
    end;
eval_expr(Symb, Env) when is_atom(Symb) ->
    case get_vbinding(Symb, Env) of
        {yes,Val} -> Val;
        no -> unbound_symb_error(Symb)
    end;
eval_expr(E, _) -> E.                           %Atomic evaluate to themselves

%% get_fbinding(NAme, Arity, Env) ->
%%     {yes,Module,Fun} | {yes,Binding} | no.
%%  Get the function binding. Locally bound function takes precedence
%%  over auto-imported BIFs.

get_fbinding(Name, Ar, Env) ->
    case lfe_env:get_fbinding(Name, Ar, Env) of
        {yes,_,_}=Yes -> Yes;                   %Imported function
        {yes,_}=Yes -> Yes;                     %Bound function
        no ->
            case lfe_internal:is_lfe_bif(Name, Ar) of
                true -> {yes,lfe,Name};         %Auto-imported LFE BIF
                false ->
                    case lfe_internal:is_erl_bif(Name, Ar) of
                        true ->                 %Auto-imported Erlang BIF
                            {yes,erlang,Name};
                        false -> no
                    end
            end
    end.

eval_list(Es, Env) ->
    map(fun (E) -> eval_expr(E, Env) end, Es).

eval_body([E], Env) -> eval_expr(E, Env);
eval_body([E|Es], Env) ->
    eval_expr(E, Env),
    eval_body(Es, Env);
eval_body([], _) -> [].                         %Empty body

%% eval_binary(Bitsegs, Env) -> Binary.
%%  Construct a binary from Bitsegs. This code is taken from
%%  eval_bits.erl. Pass in an evaluator function to be used when
%%  evaluating vale and size expression.

eval_binary(Segs, Env) ->
    Vsps = get_bitsegs(Segs),
    Eval = fun (S) -> eval_expr(S, Env) end,
    eval_bitsegs(Vsps, Eval).

get_bitsegs(Segs) ->
    foldr(fun (S, Vs) -> get_bitseg(S, Vs) end, [], Segs).

%% get_bitseg(Bitseg, ValSpecs) -> ValSpecs.
%%  A bitseg is either an atomic value, a list of value and specs, or
%%  a string.

get_bitseg([Val|Specs]=Seg, Vsps) ->
    case is_posint_list(Seg) of                 %Is bitseg a string?
        true ->                                 %A string
            {Sz,Ty} = get_bitspecs([]),
            foldr(fun (V, Vs) -> [{V,Sz,Ty}|Vs] end, Vsps, Seg);
        false ->                                %A value and spec
            {Sz,Ty} = get_bitspecs(Specs),
            case is_posint_list(Val) of         %Is Val a string?
                true -> foldr(fun (V, Vs) -> [{V,Sz,Ty}|Vs] end, Vsps, Val);
                false -> [{Val,Sz,Ty}|Vsps]     %The default
            end
    end;
get_bitseg(Val, Vsps) ->
    {Sz,Ty} = get_bitspecs([]),
    [{Val,Sz,Ty}|Vsps].

%% get_bitspec(Specs) -> {Size,Type}.
%%  Get the error handling as we want it.

get_bitspecs(Ss) ->
    case lfe_bits:get_bitspecs(Ss) of
        {ok,Sz,Ty} -> {Sz,Ty};
        {error,Error} -> eval_error(Error)
    end.

is_posint_list([I|Is]) when is_integer(I), I >= 0 ->
    is_posint_list(Is);
is_posint_list([]) -> true;
is_posint_list(_) -> false.

%% eval_bitsegs(VSTys, Evaluator) -> Binary.
%%  The evaluator function is use to evaluate the value and size
%%  fields.

eval_bitsegs(Vsps, Eval) ->
    foldl(fun ({Val,Sz,Ty}, Acc) ->
                  Bin = eval_bitseg(Val, Sz, Ty, Eval),
                  <<Acc/bitstring,Bin/bitstring>>
          end, <<>>, Vsps).

eval_bitseg(Val, Sz, Ty, Eval) ->
    V = Eval(Val),
    eval_exp_bitseg(V, Sz, Eval, Ty).

%% eval_exp_bitseg(Value, Size, EvalSize, {Type,Unit,Sign,Endian}) -> Binary.

eval_exp_bitseg(Val, Size, Eval, Type) ->
    case Type of
        %% Integer types.
        {integer,Un,Si,En} ->
            Sz = Eval(Size),
            eval_int_bitseg(Val, Sz*Un, Si, En);
        %% Unicode types, ignore unused fields.
        {utf8,_,_,_} -> <<Val/utf8>>;
        {utf16,_,_,En} -> eval_utf16_bitseg(Val, En);
        {utf32,_,_,En} -> eval_utf32_bitseg(Val, En);
        %% Float types.
        {float,Un,_,En} ->
            Sz = Eval(Size),
            eval_float_bitseg(Val, Sz*Un, En);
        %% Binary types.
        {binary,Unit,_,_} ->
            if Size == all ->
                    case bit_size(Val) of
                        Sz when Sz rem Unit =:= 0 ->
                            <<Val:Sz/bitstring>>;
                        _ -> badarg_error()
                    end;
               true ->
                    Sz = Eval(Size),
                    <<Val:(Sz*Unit)/bitstring>>
            end
    end.

eval_int_bitseg(Val, Sz, signed, big) -> <<Val:Sz/signed>>;
eval_int_bitseg(Val, Sz, unsigned, big) -> <<Val:Sz>>;
eval_int_bitseg(Val, Sz, signed, little) -> <<Val:Sz/little-signed>>;
eval_int_bitseg(Val, Sz, unsigned, little) -> <<Val:Sz/little>>;
eval_int_bitseg(Val, Sz, signed, native) -> <<Val:Sz/native-signed>>;
eval_int_bitseg(Val, Sz, unsigned, native) -> <<Val:Sz/native>>.

eval_utf16_bitseg(Val, big) -> <<Val/utf16-big>>;
eval_utf16_bitseg(Val, little) -> <<Val/utf16-little>>;
eval_utf16_bitseg(Val, native) -> <<Val/utf16-native>>.

eval_utf32_bitseg(Val, big) -> <<Val/utf32-big>>;
eval_utf32_bitseg(Val, little) -> <<Val/utf32-little>>;
eval_utf32_bitseg(Val, native) -> <<Val/utf32-native>>.

eval_float_bitseg(Val, Sz, big) -> <<Val:Sz/float>>;
eval_float_bitseg(Val, Sz, little) -> <<Val:Sz/float-little>>;
eval_float_bitseg(Val, Sz, native) -> <<Val:Sz/float-native>>.

%% map_pairs(Args, Env) -> [{K,V}].

map_pairs([K,V|As], Env) ->
    P = {map_key(K, Env),eval_expr(V, Env)},
    [P|map_pairs(As, Env)];
map_pairs([], _) -> [];
map_pairs(_, _) -> badarg_error().

%% map_key(Key, Env) -> Value.
%%  A map key can only be a literal in 17 but can be anything in 18..

-ifdef(HAS_FULL_KEYS).
map_key(Key, Env) ->
    eval_expr(Key, Env).
-else.
map_key(?Q(E), _) -> E;
map_key([_|_]=L, _) ->
    case is_posint_list(L) of
        true -> L;                              %Literal strings only
        false -> illegal_mapkey_error(L)
    end;
map_key(E, _) when not is_atom(E) -> E;         %Everything else
map_key(E, _) -> illegal_mapkey_error(E).
-endif.

%% new_vars(N) -> Vars.

new_vars(N) when N > 0 ->
    V = list_to_atom(integer_to_list(N)),
    [V|new_vars(N-1)];
new_vars(0) -> [].

%% eval_lambda([lambda|LambdaBody], Env) -> Val.
%%  Evaluate (lambda args ...).
%% eval_match_lambda(['match-lambda'|MatchClauses], Env) -> Val.
%%  Evaluate (match-lambda cls ...).

eval_lambda([lambda,Args|Body], Env) ->
    Apply =  fun (Vals) -> apply_lambda(Args, Body, Vals, Env) end,
    make_lambda(length(Args), Apply);
eval_lambda(_, _) ->
    bad_form_error(lambda).

eval_match_lambda(['match-lambda'|Cls], Env) ->
    Apply = fun(Vals) -> apply_match_lambda(Cls, Vals, Env) end,
    make_lambda(match_lambda_arity(Cls), Apply).

make_lambda(Arity, Apply) ->
    %% This is a really ugly hack! But it's the same hack as in erl_eval.
    case Arity of
        0  -> fun () -> Apply([]) end;
        1  -> fun (A) -> Apply([A]) end;
        2  -> fun (A,B) -> Apply([A,B]) end;
        3  -> fun (A,B,C) -> Apply([A,B,C]) end;
        4  -> fun (A,B,C,D) -> Apply([A,B,C,D]) end;
        5  -> fun (A,B,C,D,E) -> Apply([A,B,C,D,E]) end;
        6  -> fun (A,B,C,D,E,F) -> Apply([A,B,C,D,E,F]) end;
        7  -> fun (A,B,C,D,E,F,G) -> Apply([A,B,C,D,E,F,G]) end;
        8  -> fun (A,B,C,D,E,F,G,H) -> Apply([A,B,C,D,E,F,G,H]) end;
        9  -> fun (A,B,C,D,E,F,G,H,I) -> Apply([A,B,C,D,E,F,G,H,I]) end;
        10 -> fun (A,B,C,D,E,F,G,H,I,J) -> Apply([A,B,C,D,E,F,G,H,I,J]) end;
        11 -> fun (A,B,C,D,E,F,G,H,I,J,K) -> Apply([A,B,C,D,E,F,G,H,I,J,K]) end;
        12 -> fun (A,B,C,D,E,F,G,H,I,J,K,L) ->
                      Apply([A,B,C,D,E,F,G,H,I,J,K,L]) end;
        13 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M) ->
                      Apply([A,B,C,D,E,F,G,H,I,J,K,L,M]) end;
        14 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N) ->
                      Apply([A,B,C,D,E,F,G,H,I,J,K,L,M,N]) end;
        15 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) ->
                      Apply([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O]) end;
        _ -> eval_error({argument_limit,Arity})
    end.

apply_lambda(Args, Body, Vals, Env0) ->
    Env1 = bind_args(Args, Vals, Env0),
    eval_body(Body, Env1).

bind_args(['_'|As], [_|Es], Env) ->             %Ignore don't care variables
    bind_args(As, Es, Env);
bind_args([A|As], [E|Es], Env) when is_atom(A) ->
    bind_args(As, Es, add_vbinding(A, E, Env));
bind_args([], [], Env) -> Env.

match_lambda_arity([[Pats|_]|_]) -> length(Pats).

apply_match_lambda([[Pats|B0]|Cls], Vals, Env) ->
    if length(Vals) == length(Pats) ->
            %% Sneaky! m-l args a list of patterns so wrap with list
            %% and pass in as one pattern. Have already checked a
            %% proper list.
            case match_when([list|Pats], Vals, B0, Env) of
                {yes,B1,Vbs} -> eval_body(B1, add_vbindings(Vbs, Env));
                no -> apply_match_lambda(Cls, Vals, Env)
            end;
       true -> eval_error(bad_arity)
    end;
apply_match_lambda([], Vals, _) -> eval_error({function_clause,Vals});
apply_match_lambda(_, _, _) -> bad_form_error('match-lambda').

%% eval_let([PatBindings|Body], Env) -> Value.

eval_let([Vbs|Body], Env0) ->
    %% Make sure we use the right environment.
    Env1 = foldl(fun ([Pat,E], Env) ->
                         Val = eval_expr(E, Env0),
                         case match(Pat, Val, Env0) of
                             {yes,Bs} -> add_vbindings(Bs, Env);
                             no -> eval_error({badmatch,Val})
                         end;
                     ([Pat,['when'|_]=G,E], Env) ->
                         Val = eval_expr(E, Env0),
                         case match_when(Pat, Val, [G], Env0) of
                             {yes,[],Bs} -> add_vbindings(Bs, Env);
                             no -> eval_error({badmatch,Val})
                         end;
                     (_, _) -> bad_form_error('let')
                 end, Env0, Vbs),
    eval_body(Body, Env1).

%% eval_let_function([FuncBindings|Body], Env) -> Value.

eval_let_function([Fbs|Body], Env0) ->
    Add = fun (F, Ar, Def, Lenv, Env) ->
                  add_lexical_func(F, Ar, Def, Lenv, Env)
          end,
    Env1 = foldl(fun ([V,[lambda,Args|_]=Lambda], E) when is_atom(V) ->
                         Add(V, length(Args), Lambda, Env0, E);
                     ([V,['match-lambda',[Pats|_]|_]=Match], E)
                       when is_atom(V) ->
                         Add(V, length(Pats), Match, Env0, E);
                     (_, _) -> bad_form_error('let-function')
                 end, Env0, Fbs),
    %% io:fwrite("elf: ~p\n", [{Body,Env1}]),
    eval_body(Body, Env1).

%% eval_letrec_function([FuncBindings|Body], Env) -> Value.
%%  This is a tricky one. But we dynamically update the environment
%%  each time we are called.

eval_letrec_function([Fbs0|Body], Env0) ->
    %% Check and abstract out function bindings.
    Fbs1 = map(fun ([V,[lambda,Args|_]=Lambda]) when is_atom(V) ->
                       {V,length(Args),Lambda};
                   ([V,['match-lambda',[Pats|_]|_]=Match]) when is_atom(V) ->
                       {V,length(Pats),Match};
                   (_) -> bad_form_error('letrec-function')
               end, Fbs0),
    Env1 = make_letrec_env(Fbs1, Env0),
    %% io:fwrite("elrf: ~p\n", [{Env0,Env1}]),
    eval_body(Body, Env1).

%% init_letrec_env(Env) -> {Lete,Env}.
%% make_letrec_env(Fbs, Env) -> Env.
%% make_letrec_env(Lete, Fbs, Env) -> {Lete,Env}.
%% extend_letrec_env(Lete, Fbs, Env) -> {Lete,Env}.
%%  Create local function bindings for a set of mutally recursive
%%  functions, for example from a module or a letrec-function. This is
%%  very similar to "Metacircular Semantics for Common Lisp Special
%%  Forms" by Henry Baker, except he uses macros whereas we directly
%%  fiddle with the environment and he keeps functions in a vector
%%  where we just push them into the environment. His version compiles
%%  much better (which we don't need) but is basically the same
%%  interpreted.

%% init_letrec_env(Env) -> {[],Env}.

make_letrec_env(Fbs0, Env) ->
    Fbs1 = map(fun ({V,Ar,Body}) -> {V,Ar,{letrec,Body,Fbs0,Env}} end, Fbs0),
    add_fbindings(Fbs1, Env).

%% extend_letrec_env(Lete0, Fbs0, Env0) ->
%%     {Lete0,Env0}.

%% add_lexical_func(Name, Arity, Def, FuncEnv, Env) -> Env.
%% add_lexical_func(Name, Arity, Def, Env) -> Env.
%% add_dynamic_func(Name, Arity, Def, Env) -> Env.
%%  Add a function definition in the correct format to the
%%  environment.

add_lexical_func(Name, Ar, Def, Fenv, Env) ->
    add_fbinding(Name, Ar, {lexical_expr,Def,Fenv}, Env).

add_lexical_func(Name, Ar, Def, Env) ->
    add_fbinding(Name, Ar, {lexical_expr,Def,Env}, Env).

add_dynamic_func(Name, Ar, Def, Env) ->
    add_fbinding(Name, Ar, {dynamic_expr,Def}, Env).

%% eval_apply(Function, Args, Env) -> Value.
%%  This is used to evaluate interpreted functions. Macros are
%%  expanded completely in the function definition before it is
%%  applied.

eval_apply({dynamic_expr,Func}, Es, Env) ->
    %% Don't clear variable bindings, even if this gives dynamic scoping.
    eval_apply_expr(Func, Es, Env);
eval_apply({lexical_expr,Func,Env}, Es, _) ->
    eval_apply_expr(Func, Es, Env);
eval_apply({letrec,Body,Fbs,Env}, Es, _) ->
    %% A function created by/for letrec-function.
    NewEnv = foldl(fun ({V,Ar,Lambda}, E) ->
                           add_fbinding(V, Ar, {letrec,Lambda,Fbs,Env}, E)
                   end, Env, Fbs),
    %% io:fwrite("la: ~p\n", [{Body,NewEnv}]),
    eval_apply_expr(Body, Es, NewEnv).

%% eval_apply_expr(Function, Args, Env) -> Value.
%%  Apply the Function definition to the (evaluated) Args in Env.
%%  Macros are expanded first.

eval_apply_expr(Func, Es, Env) ->
    case lfe_macro:expand_expr_all(Func, Env) of
        [lambda,Args|Body] -> apply_lambda(Args, Body, Es, Env);
        ['match-lambda'|Cls] -> apply_match_lambda(Cls, Es, Env);
        Fun when erlang:is_function(Fun) -> erlang:apply(Fun, Es)
    end.

%% eval_if(IfBody, Env) -> Value.

eval_if([Test,True], Env) ->                    %Add default false value
    eval_if(Test, True, ?Q(false), Env);
eval_if([Test,True,False], Env) ->
    eval_if(Test, True, False, Env).

eval_if(Test, True, False, Env) ->
    case eval_expr(Test, Env) of
        true -> eval_expr(True, Env);
        false -> eval_expr(False, Env);
        _ -> eval_error(if_expression)          %Explicit error here
    end.

%% eval_case(CaseBody, Env) -> Value.

eval_case([E|Cls], Env) ->
    eval_case_clauses(eval_expr(E, Env), Cls, Env).

eval_case_clauses(V, Cls, Env) ->
    case match_clause(V, Cls, Env) of
        {yes,B,Vbs} -> eval_body(B, add_vbindings(Vbs, Env));
        no -> eval_error({case_clause,V})
    end.

match_clause(V, [[Pat|B0]|Cls], Env) ->
    case match_when(Pat, V, B0, Env) of
        {yes,_,_}=Yes -> Yes;
        no -> match_clause(V, Cls, Env)
    end;
match_clause(_, [], _) -> no.

%% eval_receive(Body, Env) -> Value
%%  (receive (pat . body) ... [(after timeout . body)])

eval_receive(Body, Env) ->
    {Cls,Te,Tb} = split_receive(Body, []),
    case eval_expr(Te, Env) of            %Check timeout
        infinity -> receive_clauses(Cls, Env);
        T -> receive_clauses(T, Tb, Cls, Env)
    end.

split_receive([['after',T|B]], Rcls) ->
    {reverse(Rcls),T,B};
split_receive([Cl|Cls], Rcls) ->
    split_receive(Cls, [Cl|Rcls]);
split_receive([], Rcls) ->
    {reverse(Rcls),?Q(infinity),[]}.    %No timeout, return 'infinity.

%% receive_clauses(Clauses, Env) -> Value.
%%  Recurse down message queue. We are only called with timeout value
%%  of 'infinity'. Always pass over all messages in queue.

receive_clauses(Cls, Env) -> receive_clauses(Cls, Env, []).

receive_clauses(Cls, Env, Ms) ->
    receive
        Msg ->
            case match_clause(Msg, Cls, Env) of
                {yes,B,Vbs} ->
                    merge_queue(Ms),
                    eval_body(B, add_vbindings(Vbs, Env));
                no -> receive_clauses(Cls, Env, [Msg|Ms])
            end
    end.

%% receive_clauses(Timeout, TimeoutBody, Clauses, Env) -> Value.
%%  Recurse down message queue until timeout. We are never called with
%%  timeout value of 'infinity'. Always pass over all messages in
%%  queue.

receive_clauses(T, Tb, Cls, Env) ->
    statistics(runtime),            %Set runtime counter
    receive_clauses(T, Tb, Cls, Env, []).

receive_clauses(T, Tb, Cls, Env, Ms) ->
    receive
        Msg ->
            case match_clause(Msg, Cls, Env) of
                {yes,B,Vbs} ->
                    merge_queue(Ms),
                    eval_body(B, add_vbindings(Vbs, Env));
                no ->
                    %% Check how much time left and recurse correctly.
                    {_,T1} = statistics(runtime),
                    if  T-T1 < 0 ->
                            receive_clauses(0, Tb, Cls, Env, [Msg|Ms]);
                        true ->
                            receive_clauses(T-T1, Tb, Cls, Env, [Msg|Ms])
                    end
            end
    after T ->
            merge_queue(Ms),
            eval_body(Tb, Env)
    end.

merge_queue(Ms) ->
    send_all(recv_all(Ms), self()).

recv_all(Xs) ->
    receive
        X -> recv_all([X|Xs])
    after 0 ->
            reverse(Xs)
    end.

send_all([X|Xs], Self) ->
    Self ! X,
    send_all(Xs, Self);
send_all([], _) -> true.

%% eval_try(TryBody, Env) -> Value.
%%  Complicated by checking legal combinations of options.

eval_try([E,['case'|Cls]|Catch], Env) ->
    eval_try_catch(Catch, E, {yes,Cls}, Env);
eval_try([E|Catch], Env) ->
    eval_try_catch(Catch, E, no, Env);
eval_try(_, _) ->
    bad_form_error('try').

eval_try_catch([['catch'|Cls]], E, Case, Env) ->
    eval_try(E, Case, {yes,Cls}, no, Env);
eval_try_catch([['catch'|Cls],['after'|B]], E, Case, Env) ->
    eval_try(E, Case, {yes,Cls}, {yes,B}, Env);
eval_try_catch([['after'|B]], E, Case, Env) ->
    eval_try(E, Case, no, {yes,B}, Env);
eval_try_catch(_, _, _, _) ->
    bad_form_error('try').

%% We do it all in one, not so efficient but easier.
eval_try(E, Case, Catch, After, Env) ->
    try
        eval_expr(E, Env)
    of
        Ret ->
            case Case of
                {yes,Cls} -> eval_case_clauses(Ret, Cls, Env);
                no -> Ret
            end
    catch
        Class:Error ->
            %% Try does return the stacktrace here but we can't hit it
            %% so we have to explicitly get it.
            Stack = erlang:get_stacktrace(),
            case Catch of
                {yes,Cls} ->
                    eval_catch_clauses({Class,Error,Stack}, Cls, Env);
                no ->
                    erlang:raise(Class, Error, Stack)
            end
    after
        case After of
            {yes,B} -> eval_body(B, Env);
            no -> []
        end
    end.

eval_catch_clauses(V, [[Pat|B0]|Cls], Env) ->
    case match_when(Pat, V, B0, Env) of
        {yes,B1,Vbs} -> eval_body(B1, add_vbindings(Vbs, Env));
        no -> eval_catch_clauses(V, Cls, Env)
    end;
eval_catch_clauses({Class,Error,Stack}, [], _) ->
    erlang:raise(Class, Error, Stack).

eval_call([M0,F0|As0], Env) ->
    M1 = eval_expr(M0, Env),
    F1 = eval_expr(F0, Env),
    As1 = eval_list(As0, Env),
    %% io:fwrite("call: ~p\n    =>~p\n", [[call,M0,F0,As0],{M1,F1,As1}]),
    erlang:apply(M1, F1, As1).

%% match_when(Pattern, Value, Body, Env) -> {yes,RestBody,Bindings} | no.
%%  Try to match pattern and evaluate guard.

match_when(Pat, V, B0, Env) ->
    case match(Pat, V, Env) of
        {yes,Vbs} ->
            case B0 of
                [['when'|G]|B1] ->
                    case eval_guard(G, add_vbindings(Vbs, Env)) of
                        true -> {yes,B1,Vbs};
                        false -> no
                    end;
                B1 -> {yes,B1,Vbs}
            end;
        no -> no
    end.

%% eval_guard(GuardTests, Env) -> true | false.
%% Guards are fault safe, catch all errors in guards here and fail guard.

eval_guard(Gts, Env) ->
    try
        eval_gbody(Gts, Env)
    of
        true -> true;
        _Other -> false                         %Fail guard
    catch
	error:illegal_guard ->			%Handle illegal guard
	    St = erlang:get_stacktrace(),
	    erlang:raise(error, illegal_guard, St);
        _:_ -> false                            %Fail guard
    end.

%% eval_gbody(GuardTests, Env) -> true | false.
%% A body is a sequence of tests which must all succeed.

eval_gbody(Gts, Env) ->
    all(fun (Gt) -> eval_gexpr(Gt, Env) end, Gts).

%% eval_gexpr(Sexpr, Environment) -> Value.
%%  Evaluate a guard sexpr in the current environment.

%% Handle the Core data special forms.
eval_gexpr(?Q(E), _) -> E;
eval_gexpr([cons,H,T], Env) ->
    [eval_gexpr(H, Env)|eval_gexpr(T, Env)];
eval_gexpr([car,E], Env) -> hd(eval_gexpr(E, Env)); %Provide lisp names
eval_gexpr([cdr,E], Env) -> tl(eval_gexpr(E, Env));
eval_gexpr([list|Es], Env) -> eval_glist(Es, Env);
eval_gexpr([tuple|Es], Env) -> list_to_tuple(eval_glist(Es, Env));
eval_gexpr([tref,Tup,I], Env) ->
    element(eval_gexpr(I, Env), eval_gexpr(Tup, Env));
eval_gexpr([binary|Bs], Env) -> eval_gbinary(Bs, Env);
%% Map operations are not allowed in guards.
%% Handle the Core closure special forms.
%% Handle the control special forms.
eval_gexpr(['progn'|Body], Env) -> eval_gbody(Body, Env);
eval_gexpr(['if'|Body], Env) -> eval_gif(Body, Env);
eval_gexpr([call,?Q(erlang),?Q(Fun)|As], Env) ->
    Ar = length(As),
    case lfe_internal:is_guard_bif(Fun, Ar) of
        true -> erlang:apply(erlang, Fun, eval_glist(As, Env));
        false -> illegal_guard_error()
    end;
eval_gexpr([Fun|Es], Env) when is_atom(Fun), Fun =/= call ->
    Ar = length(Es),
    case get_gbinding(Fun, Ar, Env) of
        {yes,M,F} -> erlang:apply(M, F, eval_glist(Es, Env));
        no -> illegal_guard_error()
    end;
eval_gexpr([_|_], _) -> illegal_guard_error();
eval_gexpr(Symb, Env) when is_atom(Symb) ->
    case get_vbinding(Symb, Env) of
        {yes,Val} -> Val;
        no -> unbound_symb_error(Symb)
    end;
eval_gexpr(E, _) -> E.                          %Atoms evaluate to themselves.

%% get_gbinding(NAme, Arity, Env) -> {yes,Module,Fun} | no.
%%  Get the guard function binding. Locally bound function cannot be
%%  called in guard only guard BIF.

get_gbinding(Name, Ar, Env) ->
    case lfe_env:is_fbound(Name, Ar, Env) of
        true -> no;                             %Locally bound function
        false ->
            case lfe_internal:is_guard_bif(Name, Ar) of
                true -> {yes,erlang,Name};
                false -> no
            end
    end.

eval_glist(Es, Env) ->
    map(fun (E) -> eval_gexpr(E, Env) end, Es).

%% eval_gbinary(Bitsegs, Env) -> Binary.
%%  Construct a binary from Bitsegs. This code is taken from eval_bits.erl.

eval_gbinary(Segs, Env) ->
    Vsps = get_bitsegs(Segs),
    Eval = fun(S) -> eval_gexpr(S, Env) end,
    eval_bitsegs(Vsps, Eval).

%% eval_gif(IfBody, Env) -> Val.

eval_gif([Test,True], Env) ->
    eval_gif(Test, True, ?Q(false), Env);
eval_gif([Test,True,False], Env) ->
    eval_gif(Test, True, False, Env).

eval_gif(Test, True, False, Env) ->
    case eval_gexpr(Test, Env) of
        true -> eval_gexpr(True, Env);
        false -> eval_gexpr(False, Env)
    end.

%% match(Pattern, Value, Env) -> {yes,PatBindings} | no.
%%  Try to match Pattern against Value within the current environment
%%  returning bindings. Bindings is an orddict.

match(Pat, Val, Env) -> match(Pat, Val, [], Env).

match(?Q(P), Val, Pbs, _) ->
    if P =:= Val -> {yes,Pbs};
       true -> no
    end;
match(['=',P1,P2], Val, Pbs0, Env) ->           %Aliases
    case match(P1, Val, Pbs0, Env) of
        {yes,Pbs1} -> match(P2, Val, Pbs1, Env);
        no -> no
    end;
match([cons,H,T], Val, Pbs, Env) ->             %Explicit cons constructor
    match_cons(H, T, Val, Pbs, Env);
match([list|Ps], Val, Pbs, Env) ->              %Explicit list constructor
    match_list(Ps, Val, Pbs, Env);
match([tuple|Ps], Val, Pbs, Env) ->
    %% io:fwrite("~p ~p\n", [Ps,Val]),
    case is_tuple(Val) of
        true -> match_list(Ps, tuple_to_list(Val), Pbs, Env);
        false -> no
    end;
match([binary|Ss], Val, Pbs, Env) ->
    case is_bitstring(Val) of
        true -> match_binary(Ss, Val, Pbs, Env);
        false -> no
    end;
match([map|Ps], Val, Pbs, Env) ->
    case ?IS_MAP(Val) of
        true -> match_map(Ps, Val, Pbs, Env);
        false -> no
    end;
match([_|_]=List, Val, Pbs, _) ->               %No constructor
    case is_posint_list(List) of                %Accept strings
        true ->
            if List =:= Val -> {yes,Pbs};
               true -> no
            end;
        false -> eval_error({illegal_pattern,List})
    end;
match([], [], Pbs, _) -> {yes,Pbs};
match(Symb, Val, Pbs, Env) when is_atom(Symb) ->
    match_symb(Symb, Val, Pbs, Env);
match(Val, Val, Pbs, _) -> {yes,Pbs};
match(_, _, _, _) -> no.

match_cons(H, T, [V|Vs], Pbs0, Env) ->
    case match(H, V, Pbs0, Env) of
        {yes,Pbs1} -> match(T, Vs, Pbs1, Env);
        no -> no
    end;
match_cons(_, _, _, _, _) -> no.

match_list([P|Ps], [V|Vs], Pbs0, Env) ->
    case match(P, V, Pbs0, Env) of
        {yes,Pbs1} -> match_list(Ps, Vs, Pbs1, Env);
        no -> no
    end;
match_list([], [], Pbs, _) -> {yes,Pbs};
match_list(_, _, _, _) -> no.

match_symb('_', _, Pbs, _) -> {yes,Pbs};        %Don't care variable.
match_symb(S, Val, Pbs, _) ->
    %% Check if Symb already bound.
    case find(S, Pbs) of
        {ok,Val} -> {yes,Pbs};                  %Bound to the same value
        {ok,_} -> no;                           %Bound to a different value
        error -> {yes,store(S, Val, Pbs)}       %Not yet bound
    end.

%% match_binary(Bitsegs, Binary, PatBindings, Env) -> {yes,PatBindings} | no.
%%  Match Bitsegs against Binary. This code is taken from
%%  eval_bits.erl. Bitspec errors generate an error. Bad matches
%%  result in an error, we use catch to trap it.

match_binary(Segs, Bin, Pbs0, Env) ->
    Psps = get_bitsegs(Segs),
    match_bitsegs(Psps, Bin, [], Pbs0, Env).

match_bitsegs([{Pat,Sz,Ty}|Psps], Bin0, Bbs0, Pbs0, Env) ->
    case match_bitseg(Pat, Sz, Ty, Bin0, Bbs0, Pbs0, Env) of
        {yes,Bin1,Bbs1,Pbs1} ->
            match_bitsegs(Psps, Bin1, Bbs1, Pbs1, Env);
        no -> no
    end;
match_bitsegs([], <<>>, _, Pbs, _) -> {yes,Pbs}; %Reached the end of both
match_bitsegs([], _, _, _, _) -> no.            %More to go

match_bitseg(Pat, Size, Type, Bin0, Bbs0, Pbs0, Env) ->
    Sz = get_pat_bitsize(Size, Type, Bbs0, Pbs0, Env),
    case catch {ok,get_pat_bitseg(Bin0, Sz, Type)} of
        {ok,{Val,Bin1}} ->
            case match_bitexpr(Pat, Val, Bbs0, Pbs0, Env) of
                {yes,Bbs1,Pbs1} -> {yes,Bin1,Bbs1,Pbs1};
                no -> no
            end;
        _ -> no
    end.

get_pat_bitsize(all, {Ty,_,_,_}, _, _, _) ->
    if Ty =:= binary -> all;
       true -> eval_error(illegal_bitsize)
    end;
get_pat_bitsize(undefined, {Ty,_,_,_}, _, _, _) ->
    if Ty =:= utf8; Ty =:= utf16; Ty =:= utf32 -> undefined;
       true -> eval_error(illegal_bitsize)
    end;
get_pat_bitsize(S, _, _, _, _) when is_integer(S) -> S;
get_pat_bitsize(S, _, Bbs, _, Env) when is_atom(S) ->
    %% Variable either in environment or bound in binary.
    case get_vbinding(S, Env) of
        {yes,V} -> V;
        no ->
            case find(S, Bbs) of
                {ok,V} -> V;
                error -> unbound_symb_error(S)
            end
    end.

match_bitexpr(N, Val, Bbs, Pbs, _) when is_number(N) ->
    if N =:= Val -> {yes,Bbs,Pbs};
       true -> no
    end;
match_bitexpr('_', _, Bbs, Pbs, _) -> {yes,Bbs,Pbs};
match_bitexpr(S, Val, Bbs, Pbs, _) when is_atom(S) ->
    %% We know that if variable is in Pbs it will also be in Bbs!
    case find(S, Pbs) of
        {ok,Val} -> {yes,Bbs,Pbs};              %Bound to the same value
        {ok,_} -> no;                           %Bound to a different value
        error ->                                %Not yet bound
            {yes,store(S, Val, Bbs),store(S, Val, Pbs)}
    end;
match_bitexpr(_, _, _, _, _) -> eval_error(illegal_bitseg).

%% get_pat_bitseg(Binary, Size, {Type,Unit,Sign,Endian}) -> {Value,RestBinary}.
%%  This function can signal error if impossible to get specified bit
%%  segment.

get_pat_bitseg(Bin, Size, Type) ->
    case Type of
        %% Integer types.
        {integer,Un,Si,En} ->
            get_int_bitseg(Bin, Size*Un, Si, En);
        %% Unicode types, ignore unused bitsegs.
        {utf8,_,_,_} -> get_utf8_bitseg(Bin);
        {utf16,_,_,En} -> get_utf16_bitseg(Bin, En);
        {utf32,_,_,En} -> get_utf32_bitseg(Bin, En);
        %% Float types.
        {float,Un,_,En} -> get_float_bitseg(Bin, Size*Un, En);
        %% Binary types.
        {binary,Un,_,_} ->
            if Size == all ->
                    0 = (bit_size(Bin) rem Un),
                    {Bin,<<>>};
               true ->
                    TotSize = Size * Un,
                    <<Val:TotSize/bitstring,Rest/bitstring>> = Bin,
                    {Val,Rest}
            end
    end.

get_int_bitseg(Bin, Sz, signed, big) ->
    <<Val:Sz/big-signed,Rest/bitstring>> = Bin,
    {Val,Rest};
get_int_bitseg(Bin, Sz, unsigned, big) ->
    <<Val:Sz/big-unsigned,Rest/bitstring>> = Bin,
    {Val,Rest};
get_int_bitseg(Bin, Sz, signed, little) ->
    <<Val:Sz/little-signed,Rest/bitstring>> = Bin,
    {Val,Rest};
get_int_bitseg(Bin, Sz, unsigned, little) ->
    <<Val:Sz/little-unsigned,Rest/bitstring>> = Bin,
    {Val,Rest};
get_int_bitseg(Bin, Sz, signed, native) ->
    <<Val:Sz/native-signed,Rest/bitstring>> = Bin,
    {Val,Rest};
get_int_bitseg(Bin, Sz, unsigned, native) ->
    <<Val:Sz/native-unsigned,Rest/bitstring>> = Bin,
    {Val,Rest}.

get_utf8_bitseg(Bin) ->
    <<Val/utf8,Rest/bitstring>> = Bin,
    {Val,Rest}.

get_utf16_bitseg(Bin, big) ->
    <<Val/utf16-big,Rest/bitstring>> = Bin,
    {Val,Rest};
get_utf16_bitseg(Bin, little) ->
    <<Val/utf16-little,Rest/bitstring>> = Bin,
    {Val,Rest};
get_utf16_bitseg(Bin, native) ->
    <<Val/utf16-native,Rest/bitstring>> = Bin,
    {Val,Rest}.

get_utf32_bitseg(Bin, big) ->
    <<Val/utf32-big,Rest/bitstring>> = Bin,
    {Val,Rest};
get_utf32_bitseg(Bin, little) ->
    <<Val/utf32-little,Rest/bitstring>> = Bin,
    {Val,Rest};
get_utf32_bitseg(Bin, native) ->
    <<Val/utf32-native,Rest/bitstring>> = Bin,
    {Val,Rest}.

get_float_bitseg(Bin, Sz, big) ->
    <<Val:Sz/float,Rest/bitstring>> = Bin,
    {Val,Rest};
get_float_bitseg(Bin, Sz, little) ->
    <<Val:Sz/float-little,Rest/bitstring>> = Bin,
    {Val,Rest};
get_float_bitseg(Bin, Sz, native) ->
    <<Val:Sz/float-native,Rest/bitstring>> = Bin,
    {Val,Rest}.

%% match_map(Pairs, Map, PatBindings, Env) -> {yes,PatBindings} | no.

match_map([K,V|Ps], Map, Pbs0, Env) ->
    Pat = pat_map_key(K),                       %Evaluate the key
    case maps:is_key(Pat, Map) of
        true ->
            case match(V, maps:get(Pat, Map), Pbs0, Env) of
                {yes,Pbs1} -> match_map(Ps, Map, Pbs1, Env);
                no -> no
            end;
        false -> no
    end;
match_map([], _, Pbs, _) -> {yes,Pbs};
match_map(Ps, _, _, _) -> eval_error({illegal_pattern,Ps}).

pat_map_key(?Q(E)) -> E;
pat_map_key([_|_]=L) ->
    case is_posint_list(L) of
        true -> L;                              %Literal strings only
        false -> illegal_mapkey_error(L)
    end;
pat_map_key(E) when not is_atom(E) -> E;        %Everything else
pat_map_key(K) -> illegal_mapkey_error(K).

%% eval_lit(Literal, Env) -> Value.
%%  Evaluate a literal expression. Error if invalid.

eval_lit(?Q(K), _) -> K;
eval_lit([cons,H,T], Env) ->
    [eval_lit(H, Env)|eval_lit(T, Env)];
eval_lit([list|Es], Env) ->
    eval_lit_list(Es, Env);
eval_lit([tuple|Es], Env) ->
    list_to_tuple(eval_lit_list(Es, Env));
eval_lit([binary|Bs], Env) ->
    eval_lit_binary(Bs, Env);
eval_lit([map|As], Env) ->
    KVs = eval_lit_map(As, Env),
    maps:from_list(KVs);
eval_lit([_|_]=Lit, _) ->                       %All other lists illegal
    eval_error({illegal_literal,Lit});
eval_lit(Symb, Env) when is_atom(Symb) ->
    case get_vbinding(Symb, Env) of
        {yes,Val} -> Val;
        no -> unbound_symb_error(Symb)
    end;
eval_lit(Key, _) -> Key.                        %Literal values

eval_lit_list(Es, Env) ->
    [ eval_lit(E, Env) || E <- Es ].

eval_lit_binary(Segs, Env) ->
    Vsps = get_bitsegs(Segs),
    Eval = fun (S) -> eval_lit(S, Env) end,
    eval_bitsegs(Vsps, Eval).

eval_lit_map([K,V|As], Env) ->
    [{eval_lit(K, Env),eval_lit(V, Env)}|eval_lit_map(As, Env)];
eval_lit_map([], _) -> [].

%% Error functions. {?MODULE,eval_expr,2} is the stacktrace.

badarg_error() -> eval_error(badarg).

unbound_symb_error(Sym) ->
    eval_error({unbound_symb,Sym}).

undefined_func_error(Func, Ar) ->
    eval_error({undefined_func,{Func,Ar}}).

bad_form_error(Form) ->
    eval_error({bad_form,Form}).

illegal_guard_error() ->
    eval_error(illegal_guard).

illegal_mapkey_error(Key) ->
    eval_error({illegal_mapkey,Key}).

eval_error(Error) ->
    erlang:raise(error, Error, stacktrace()).

stacktrace() -> [{?MODULE,eval_expr,2}].

%%% Helper functions

maps_put({K,V}, M) -> maps:put(K, V, M).

maps_update({K,V}, M) -> maps:update(K, V, M).
