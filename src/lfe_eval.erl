%% Copyright (c) 2008 Robert Virding. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%% File    : lfe_eval.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang interpreter.

%%% This is a real hack!

-module(lfe_eval).

-export([expr/1,expr/2,gexpr/1,gexpr/2,apply/2,apply/3,
	 make_letrec_env/2,add_expr_func/4,match/3]).

-export([eval/1,eval/2,eval_list/2]).

-import(lfe_lib, [new_env/0,add_vbinding/3,add_vbindings/2,get_vbinding/2,
		  add_fbinding/4,add_fbindings/2,get_fbinding/3,
		  add_ibinding/5,get_gbinding/3]).

-import(lists, [reverse/1,map/2,foldl/3]).
-import(orddict, [find/2,store/3]).

-deprecated([eval/1,eval/2]).

%% -compile([export_all]).

%% eval(Sexpr) -> Value.
%% eval(Sexpr, Env) -> Value.

eval(E) -> eval(E, new_env()).

eval(E, Env) -> eval_expr(E, Env).

%% expr(Sexpr) -> Value.
%% expr(Sexpr, Env) -> Value.

expr(E) -> expr(E, new_env()).

expr(E, Env) -> eval_expr(E, Env).

%% expr(Sexpr) -> Value.
%% expr(Sexpr, Env) -> Value.

gexpr(E) -> gexpr(E, new_env()).

gexpr(E, Env) -> eval_gexpr(E, Env).

%% apply(Function, Args) -> Expr.
%% apply(Function, Args, Env) -> Expr.
%%  This is applying interpreted Erlang functions, for applying funs
%%  use normal apply. Name scoping stops us from using apply/s
%%  internally. Args should already be evaluated.

apply(F, Args) ->
    Env = new_env(),
    eval_apply({expr,F,Env}, Args, Env).

apply(F, Args, Env) ->
    eval_apply({expr,F,Env}, Args, Env).		%Env at function def

%% eval_expr(Sexpr, Environment) -> Value.
%%  Evaluate a sexpr in the current environment. Try to catch core
%%  forms by just name and check arguments arguments later. Otherwise
%%  users can redefine core forms with different number of arguments.

%% Handle the Core data special forms.
eval_expr([quote|E], _) -> hd(E);
eval_expr([cons,H,T], Env) ->
    [eval_expr(H, Env)|eval_expr(T, Env)];
eval_expr([car,E], Env) -> hd(eval_expr(E, Env)); %Provide lisp names
eval_expr([cdr,E], Env) -> tl(eval_expr(E, Env));
eval_expr([list|Es], Env) -> eval_list(Es, Env);
eval_expr([tuple|Es], Env) -> list_to_tuple(eval_list(Es, Env));
eval_expr([binary|Bs], Env) ->
    eval_binary(Bs, Env);
%% Handle the Core closure special forms.
eval_expr([lambda|Body], Env) ->
    eval_lambda(Body, Env);
eval_expr(['match-lambda'|Cls], Env) ->
    eval_match_lambda(Cls, Env);
eval_expr(['let'|Body], Env) ->
    eval_let(Body, Env);
eval_expr(['let-function'|Body], Env) ->
    eval_let_function(Body, Env);
eval_expr(['letrec-function'|Body], Env) ->
    eval_letrec_function(Body, Env);
%% Handle the control special forms.
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
    erlang:apply(eval_expr(F, Env), eval_list(As, Env));
eval_expr([call|Body], Env) ->
    eval_call(Body, Env);
eval_expr([Fun|Es]=Call, Env) when is_atom(Fun) ->
    %% If macro then expand and try again, else try to find function.
    %% We only expand the top level here.
    case lfe_macro:expand_macro(Call, Env) of
	{yes,Exp} -> eval_expr(Exp, Env);	%This was macro, try again
	no ->
	    Ar = length(Es),			%Arity
	    case get_fbinding(Fun, Ar, Env) of
		{yes,M,F} -> erlang:apply(M, F, eval_list(Es, Env));
		{yes,F} -> eval_apply(F, eval_list(Es, Env), Env);
		no -> erlang:error({unbound_func,{Fun,Ar}})
	    end
    end;
eval_expr([_|_], _) ->
    erlang:error({bad_form,application});
eval_expr(Symb, Env) when is_atom(Symb) ->
    case get_vbinding(Symb, Env) of
	{yes,Val} -> Val;
	no -> erlang:error({unbound_symb,Symb})
    end;
eval_expr(E, _) -> E.				%Atoms evaluate to themselves.

eval_list(Es, Env) ->
    map(fun (E) -> eval_expr(E, Env) end, Es).

eval_body([E], Env) -> eval_expr(E, Env);
eval_body([E|Es], Env) ->
    eval_expr(E, Env),
    eval_body(Es, Env);
eval_body([], _) -> [].				%Empty body

%% eval_binary(Fields, Env) -> Binary.
%%  Construct a binary from Fields. This code is taken from eval_bits.erl.

eval_binary(Fs, Env) ->
    Vsps = map(fun (F) -> parse_field(F, Env) end, Fs),
    eval_fields(Vsps, Env).

-record(spec, {type=integer,size=default,unit=default,
	       sign=default,endian=default}).

parse_field([Val|Specs], Env) ->
    {Val,parse_bitspecs(Specs, #spec{}, Env)};
parse_field(Val, Env) ->
    {Val,parse_bitspecs([], #spec{}, Env)}.

eval_fields(Vsps, Env) ->
    foldl(fun ({Val,Spec}, Acc) ->
		  Bin = eval_field(Val, Spec, Env),
		  <<Acc/binary-unit:1,Bin/binary-unit:1>>
	  end, <<>>, Vsps).

eval_field(Val, Spec, Env) ->
    V = eval_expr(Val, Env),
    eval_exp_field(V, Spec).

%% parse_bitspecs(Specs, Spec, Env) -> {Type,Size,Unit,Sign,End}.

parse_bitspecs(Ss, Sp0, Env) ->
    Sp1 = foldl(fun (S, Sp) -> parse_bitspec(S, Sp, Env) end, Sp0, Ss),
    %% Adjust the values depending on type and given value.
    #spec{type=Type,size=Csize,unit=Cunit,sign=Csign,endian=Cend} = Sp1,
    case Type of
	integer ->
	    {integer,
	     val_or_def(Csize, 8),val_or_def(Cunit, 1),
	     val_or_def(Csign, unsigned),val_or_def(Cend, big)};
	utf8 ->					%Ignore unused fields!
	    {utf8,undefined,undefined,undefined,undefined};
	utf16 ->				%Ignore unused fields!
	    {utf16,undefined,undefined,undefined,val_or_def(Cend, big)};
	utf32 ->				%Ignore unused fields!
	    {utf32,undefined,undefined,undefined,val_or_def(Cend, big)};
	float ->
	    {float,
	     val_or_def(Csize, 64),val_or_def(Cunit, 1),
	     val_or_def(Csign, unsigned),val_or_def(Cend, big)};
	binary ->
	    {binary,
	     val_or_def(Csize, all),val_or_def(Cunit, 8),
	     val_or_def(Csign, unsigned),val_or_def(Cend, big)};
	bitstring ->
	    {binary,
	     val_or_def(Csize, all),val_or_def(Cunit, 1),
	     val_or_def(Csign, unsigned),val_or_def(Cend, big)}
    end.

%% Types.
parse_bitspec(integer, Sp, _) -> Sp#spec{type=integer};
parse_bitspec(float, Sp, _) -> Sp#spec{type=float};
parse_bitspec(binary, Sp, _) -> Sp#spec{type=binary};
parse_bitspec(bytes, Sp, _) -> Sp#spec{type=binary};
parse_bitspec(bitstring, Sp, _) -> Sp#spec{type=bitstring};
parse_bitspec(bits, Sp, _) -> Sp#spec{type=bitstring};
%% Unicode types.
parse_bitspec('utf-8', Sp, _) -> Sp#spec{type=utf8};
parse_bitspec('utf-16', Sp, _) -> Sp#spec{type=utf16};
parse_bitspec('utf-32', Sp, _) -> Sp#spec{type=utf32};
%% Endianness.
parse_bitspec('big-endian', Sp, _) -> Sp#spec{endian=big};
parse_bitspec('little-endian', Sp, _) -> Sp#spec{endian=little};
parse_bitspec('native-endian', Sp, _) -> Sp#spec{endian=native};
%% Sign.
parse_bitspec(signed, Sp, _) -> Sp#spec{sign=signed};
parse_bitspec(unsigned, Sp, _) -> Sp#spec{sign=unsigned};
%% Size.
parse_bitspec([size,N], Sp, Env) ->
    Size = eval_expr(N, Env),
    Sp#spec{size=Size};
parse_bitspec([unit,N], Sp, _) when is_integer(N), N > 0 -> Sp#spec{unit=N};
%% Illegal spec.
parse_bitspec(Spec, _, _) -> erlang:error({illegal_bitspec,Spec}).

val_or_def(default, Def) -> Def;
val_or_def(V, _) -> V.

%% eval_exp_field(Value, {Type,Size,Unit,Sign,Endian}) -> Binary.

eval_exp_field(Val, Spec) ->
    case Spec of
	%% Integer types.
	{integer,Sz,Un,Si,En} -> eval_int_field(Val, Sz*Un, Si, En);
	%% Unicode types, ignore unused fields.
	{utf8,_,_,_,_} -> <<Val/utf8>>;
	{utf16,_,_,_,En} -> eval_utf16_field(Val, En);
	{utf32,_,_,_,En} -> eval_utf32_field(Val, En);
	%% Float types.
	{float,Sz,Un,_,En} -> eval_float_field(Val, Sz*Un, En);
	%% Binary types.
	{binary,all,Unit,_,_} ->
	    case erlang:bit_size(Val) of
		Size when Size rem Unit =:= 0 ->
		    <<Val:Size/binary-unit:1>>;
		_ -> erlang:error(badarg)
	    end;
	{binary,Size,Unit,_,_} ->
	    <<Val:(Size*Unit)/binary-unit:1>>
    end.

eval_int_field(Val, Sz, signed, little) -> <<Val:Sz/little-signed>>;
eval_int_field(Val, Sz, unsigned, little) -> <<Val:Sz/little>>;
eval_int_field(Val, Sz, signed, native) -> <<Val:Sz/native-signed>>;
eval_int_field(Val, Sz, unsigned, native) -> <<Val:Sz/native>>;
eval_int_field(Val, Sz, signed, big) -> <<Val:Sz/signed>>;
eval_int_field(Val, Sz, unsigned, big) -> <<Val:Sz>>.

eval_utf16_field(Val, little) -> <<Val/utf16-little>>;
eval_utf16_field(Val, native) -> <<Val/utf16-native>>;
eval_utf16_field(Val, big) -> <<Val/utf16-big>>.

eval_utf32_field(Val, little) -> <<Val/utf32-little>>;
eval_utf32_field(Val, native) -> <<Val/utf32-native>>;
eval_utf32_field(Val, big) -> <<Val/utf32-big>>.

eval_float_field(Val, Sz, little) -> <<Val:Sz/float-little>>;
eval_float_field(Val, Sz, native) -> <<Val:Sz/float-native>>;
eval_float_field(Val, Sz, big) -> <<Val:Sz/float>>.

%% eval_lambda(LambdaBody, Env) -> Val.
%%  Evaluate (lambda args ...).

eval_lambda([Args|Body], Env) ->
    %% This is a really ugly hack!
    case length(Args) of
	0 -> fun () -> eval_lambda([], [], Body, Env) end;
	1 -> fun (A) -> eval_lambda([A], Args, Body, Env) end;
	2 -> fun (A,B) -> eval_lambda([A,B], Args, Body, Env) end;
	3 -> fun (A,B,C) -> eval_lambda([A,B,C], Args, Body, Env) end;
	4 -> fun (A,B,C,D) -> eval_lambda([A,B,C,D], Args, Body, Env) end;
	5 -> fun (A,B,C,D,E) -> eval_lambda([A,B,C,D,E], Args, Body, Env) end;
	6 -> fun (A,B,C,D,E,F) ->
	    eval_lambda([A,B,C,D,E,F], Args, Body, Env) end;
	7 -> fun (A,B,C,D,E,F,G) ->
	    eval_lambda([A,B,C,D,E,F,G], Args, Body, Env) end;
	8 -> fun (A,B,C,D,E,F,G,H) ->
	    eval_lambda([A,B,C,D,E,F,G,H], Args, Body, Env) end;
	9 -> fun (A,B,C,D,E,F,G,H,I) ->
	    eval_lambda([A,B,C,D,E,F,G,H,I], Args, Body, Env) end;
	10 -> fun (A,B,C,D,E,F,G,H,I,J) ->
	    eval_lambda([A,B,C,D,E,F,G,H,I,J], Args, Body, Env) end;
	11 -> fun (A,B,C,D,E,F,G,H,I,J,K) ->
	    eval_lambda([A,B,C,D,E,F,G,H,I,J,K], Args, Body, Env) end;
	12 -> fun (A,B,C,D,E,F,G,H,I,J,K,L) ->
	    eval_lambda([A,B,C,D,E,F,G,H,I,J,K,L], Args, Body, Env) end;
	13 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M) ->
	    eval_lambda([A,B,C,D,E,F,G,H,I,J,K,L,M], Args, Body, Env) end;
	14 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N) ->
	    eval_lambda([A,B,C,D,E,F,G,H,I,J,K,L,M,N], Args, Body, Env) end;
	15 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) ->
	    eval_lambda([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], Args, Body, Env) end
    end.

eval_lambda(Vals, Args, Body, Env0) ->
    Env1 = bind_args(Args, Vals, Env0),
    eval_body(Body, Env1).

bind_args(['_'|As], [_|Es], Env) ->		%Ignore don't care variables
    bind_args(As, Es, Env);
bind_args([A|As], [E|Es], Env) when is_atom(A) ->
    bind_args(As, Es, add_vbinding(A, E, Env));
bind_args([], [], Env) -> Env.

%% eval_match_lambda(MatchClauses, Env) -> Val.
%%  Evaluate (match-lambda cls ...).

eval_match_lambda(Cls, Env) ->
    %% This is a really ugly hack!
    case match_lambda_arity(Cls) of
	0 -> fun () -> eval_match_clauses([], Cls, Env) end;
	1 -> fun (A) -> eval_match_clauses([A], Cls, Env) end;
	2 -> fun (A,B) -> eval_match_clauses([A,B], Cls, Env) end;
	3 -> fun (A,B,C) -> eval_match_clauses([A,B,C], Cls, Env) end;
	4 -> fun (A,B,C,D) -> eval_match_clauses([A,B,C,D], Cls, Env) end;
	5 -> fun (A,B,C,D,E) -> eval_match_clauses([A,B,C,D,E], Cls, Env) end;
	6 -> fun (A,B,C,D,E,F) ->
	    eval_match_clauses([A,B,C,D,E,F], Cls, Env) end;
	7 -> fun (A,B,C,D,E,F,G) ->
	    eval_match_clauses([A,B,C,D,E,F,G], Cls, Env) end;
	8 -> fun (A,B,C,D,E,F,G,H) ->
	    eval_match_clauses([A,B,C,D,E,F,G,H], Cls, Env) end;
	9 -> fun (A,B,C,D,E,F,G,H,I) ->
	    eval_match_clauses([A,B,C,D,E,F,G,H,I], Cls, Env) end;
	10 -> fun (A,B,C,D,E,F,G,H,I,J) ->
	    eval_match_clauses([A,B,C,D,E,F,G,H,I,J], Cls, Env) end;
	11 -> fun (A,B,C,D,E,F,G,H,I,J,K) ->
	    eval_match_clauses([A,B,C,D,E,F,G,H,I,J,K], Cls, Env) end;
	12 -> fun (A,B,C,D,E,F,G,H,I,J,K,L) ->
	    eval_match_clauses([A,B,C,D,E,F,G,H,I,J,K,L], Cls, Env) end;
	13 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M) ->
	    eval_match_clauses([A,B,C,D,E,F,G,H,I,J,K,L,M], Cls, Env) end;
	14 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N) ->
	    eval_match_clauses([A,B,C,D,E,F,G,H,I,J,K,L,M,N], Cls, Env) end;
	15 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) ->
	    eval_match_clauses([A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], Cls, Env) end
    end.

match_lambda_arity([[Pats|_]|_]) -> length(Pats).

eval_match_clauses(Vals, [[Pats|B0]|Cls], Env) ->
    if length(Vals) == length(Pats) ->
	    case match_when(Pats, Vals, B0, Env) of
		{yes,B1,Vbs} -> eval_body(B1, add_vbindings(Vbs, Env));
		no -> eval_match_clauses(Vals, Cls, Env)
	    end;
       true -> erlang:error(badarity)
    end;
eval_match_clauses(_, _, _) -> erlang:error(function_clause).

eval_let([Vbs|Body], Env0) ->
    Env1 = foldl(fun ([Pat,E], Env) ->
			 Val = eval_expr(E, Env0),
			 {yes,Bs} = match(Pat, Val, Env0),
			 add_vbindings(Bs, Env);
		     ([Pat,G,E], Env) ->
			 Val = eval_expr(E, Env0),
			 {yes,[],Bs} = match_when(Pat, Val, [G], Env0),
			 add_vbindings(Bs, Env);
		     (_, _) -> erlang:error({bad_form,'let'})
		 end, Env0, Vbs),
    eval_body(Body, Env1).

%% eval_let_function([FuncBindings|Body], Env) -> Value.

eval_let_function([Fbs|Body], Env0) ->
    Env1 = foldl(fun ([V,[lambda,Args|_]=Lambda], E) when is_atom(V) ->
			 add_fbinding(V, length(Args), {expr,Lambda,Env0}, E);
		     ([V,['match-lambda',[Pats|_]|_]=Match], E)
		     when is_atom(V) ->
			 add_fbinding(V, length(Pats), {expr,Match,Env0}, E);
		     (_, _) -> erlang:error({bad_form,'let-function'})
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
		   (_) -> erlang:error({bad_form,'letrec-function'})
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

init_letrec_env(Env) -> {[],Env}.

make_letrec_env(Fbs0, Env) ->
    Fbs1 = map(fun ({V,Ar,Body}) -> {V,Ar,{letrec,Body,Fbs0,Env}} end, Fbs0),
    add_fbindings(Fbs1, Env).

extend_letrec_env(Lete0, Fbs0, Env0) ->
    {Lete0,Env0}.

%% add_expr_func(Name, Arity, Def, Env) -> Env.
%%  Add a function definition in the correct format to the
%%  environment.

add_expr_func(Name, Ar, Def, Env) ->
    add_fbinding(Name, Ar, {expr,Def,Env}, Env).

%% eval_apply(Function, Vals, Env) -> Value.
%%  This is used to evaluate interpreted functions.

eval_apply({expr,[lambda,Args|Body],Env}, Es, _) ->
    eval_lambda(Es, Args, Body, Env);
eval_apply({expr,['match-lambda'|Cls],Env}, Es, _) ->
    eval_match_clauses(Es, Cls, Env);
eval_apply({letrec,Body,Fbs,Env}, Es, Ee) ->
    %% A function created by/for letrec-function.
    NewEnv = foldl(fun ({V,Ar,Lambda}, E) ->
			   add_fbinding(V, Ar, {letrec,Lambda,Fbs,Env}, E)
		   end, Env, Fbs),
    %% io:fwrite("la: ~p\n", [{Body,NewEnv}]),
    eval_apply({expr,Body,NewEnv}, Es, Ee).

%% eval_if(IfBody, Env) -> Value.

eval_if([Test,True], Env) ->
    eval_if(Test, True, [quote,false], Env);
eval_if([Test,True,False], Env) ->
    eval_if(Test, True, False, Env).

eval_if(Test, True, False, Env) ->
    case eval_expr(Test, Env) of
	true -> eval_expr(True, Env);
	false -> eval_expr(False, Env);
	_ -> erlang:error(if_clause)		%Explicit error here
    end.

eval_case([E|Cls], Env) ->
    eval_case_clauses(eval_expr(E, Env), Cls, Env).

eval_case_clauses(V, Cls, Env) ->
    case match_clause(V, Cls, Env) of
	{yes,B,Vbs} -> eval_body(B, add_vbindings(Vbs, Env));
	no -> erlang:error({case_clause,V})
    end.

match_clause(V, [[Pat|B0]|Cls], Env) ->
    case match_when(Pat, V, B0, Env) of
	{yes,_,_}=Yes -> Yes;
	no -> match_clause(V, Cls, Env)
    end;
match_clause(_, [], _) -> no.

%% eval_recieve(Body, Env) -> Value
%%  (receive (pat . body) ... [(after timeout . body)])

eval_receive(Body, Env) ->
    {Cls,Te,Tb} = split_receive(Body, []),
    case eval_expr(Te, Env) of			%Check timeout
	infinity -> receive_clauses(Cls, Env, []);
	T -> receive_clauses(T, Tb, Cls, Env)
    end.

split_receive([['after',T|B]], Rcls) ->
    {reverse(Rcls),T,B};
split_receive([Cl|Cls], Rcls) ->
    split_receive(Cls, [Cl|Rcls]);
split_receive([], Rcls) ->
    {reverse(Rcls),[quote,infinity],[]}.	%No timeout, return 'infinity.

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

%% receive_clauses(Timeout, TimeoutBody, Clauses) -> Value.
%%  Recurse down message queue until timeout. We are never called with
%%  timeout value of 'infinity'. Always pass over all messages in
%%  queue.

receive_clauses(T, Tb, Cls, Env) ->
    statistics(runtime),			%Set runtime counter
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
    eval_try_catch(Catch, E, no, Env).

eval_try_catch([['catch'|Cls]], E, Case, Env) ->
    eval_try(E, Case, {yes,Cls}, no, Env);
eval_try_catch([['catch'|Cls],['after'|B]], E, Case, Env) ->
    eval_try(E, Case, {yes,Cls}, {yes,B}, Env);
eval_try_catch([['after'|B]], E, Case, Env) ->
    eval_try(E, Case, no, {yes,B}, Env).

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
	no -> eval_case_clauses(V, Cls, Env)
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
		[['when',G]|B1] ->
		    %% Guards are fault safe.
		    try
			eval_gexpr(G, add_vbindings(Vbs, Env))
			of
			true -> {yes,B1,Vbs};
			_Other -> no			%Fail guard
		    catch
			_:_ -> no
		    end;
		B1 -> {yes,B1,Vbs}
	    end;
	no -> no
    end.

%% eval_gexpr(Sexpr, Environment) -> Value.
%%  Evaluate a guard sexpr in the current environment.

%% Handle the Core data special forms.
eval_gexpr([quote,E], _) -> E;
eval_gexpr([cons,H,T], Env) ->
    [eval_gexpr(H, Env)|eval_gexpr(T, Env)];
eval_gexpr([car,E], Env) -> hd(eval_gexpr(E, Env));	%Provide lisp names
eval_gexpr([cdr,E], Env) -> tl(eval_gexpr(E, Env));
eval_gexpr([list|Es], Env) -> eval_glist(Es, Env);
eval_gexpr([tuple|Es], Env) -> list_to_tuple(eval_glist(Es, Env));
%% Handle the Core closure special forms.
eval_gexpr(['let',Vbs|Body], Env) ->
    eval_glet(Vbs, Body, Env);
%% Handle the control special forms.
eval_gexpr(['progn'|Body], Env) ->
    eval_gbody(Body, Env);
eval_gexpr(['if',Test,True], Env) ->
    eval_gif(Test, True, [quote,false], Env);
eval_gexpr(['if',Test,True,False], Env) ->
    eval_gif(Test, True, False, Env);
eval_gexpr(['case',E|Cls], Env) ->
    eval_gcase(E, Cls, Env);
eval_gexpr([call,[quote,erlang],F0|As], Env) ->
    Ar = length(As),
    F1 = eval_gexpr(F0, Env),
    case get_gbinding(F1, Ar, Env) of
	{yes,M,F} -> erlang:apply(M, F, eval_glist(As, Env));
	_ -> erlang:error({unbound_func,{F1,Ar}})
    end;
eval_gexpr([Fun|Es], Env) when is_atom(Fun) ->
    Ar = length(Es),
    case get_gbinding(Fun, Ar, Env) of
	{yes,M,F} -> erlang:apply(M, F, eval_glist(Es, Env));
	_ -> erlang:error({unbound_func,Fun})
    end;
eval_gexpr([_|_], _) ->
    erlang:error(illegal_guard);
eval_gexpr(Symb, Env) when is_atom(Symb) ->
    case get_vbinding(Symb, Env) of
	{yes,Val} -> Val;
	no -> erlang:error({unbound_symb,Symb})
    end;
eval_gexpr(E, _) -> E.				%Atoms evaluate to themselves.

eval_glist(Es, Env) ->
    map(fun (E) -> eval_gexpr(E, Env) end, Es).

eval_gbody([E], Env) -> eval_gexpr(E, Env);
eval_gbody([E|Es], Env) ->
    eval_gexpr(E, Env),
    eval_gbody(Es, Env);
eval_gbody([], _) -> [].

eval_glet(Vbs, Body, Env0) ->
    Env1 = foldl(fun ([V,E], Env) when is_atom(V) ->
			 add_vbinding(V, eval_gexpr(E, Env0), Env)
		 end, Env0, Vbs),
    eval_gbody(Body, Env1).

eval_gif(Test, True, False, Env) ->
    case eval_gexpr(Test, Env) of
	true -> eval_gexpr(True, Env);
	false -> eval_gexpr(False, Env)
    end.

eval_gcase(E, Cls, Env) ->
    eval_gcase_clauses(eval_gexpr(E, Env), Cls, Env).

eval_gcase_clauses(V, [[Pat|B0]|Cls], Env) ->
    case match_when(Pat, V, B0, Env) of
	{yes,B1,Vbs} -> eval_gbody(B1, add_vbindings(Vbs, Env));
	no -> eval_gcase_clauses(V, Cls, Env)
    end.

%% match(Pattern, Value, Env) -> {yes,Bindings} | no.
%%  Try to match Pattern against Value within the current environment
%%  returning bindings. Bindings is an orddict.

match(Pat, Val, Env) -> match(Pat, Val, Env, []).

match([quote,P], Val, _, Bs) ->
    if P == Val -> {yes,Bs};
       true -> no
    end;
match([tuple|Ps], Val, Env, Bs) ->
    %% io:fwrite("~p ~p\n", [Ps,Val]),
    case is_tuple(Val) of
	true -> match(Ps, tuple_to_list(Val), Env, Bs);
	false -> no
    end;
match([binary|Fs], Val, Env, Bs) ->
    case is_bitstring(Val) of
	true -> match_binary(Fs, Val, Env, Bs);
	false -> no
    end;
match(['=',P1,P2], Val, Env, Bs0) ->		%Aliases
    case match(P1, Val, Env, Bs0) of
	{yes,Bs1} -> match(P2, Val, Env, Bs1);
	no -> no
    end;
match([P|Ps], [V|Vs], Env, Bs0) ->
    case match(P, V, Env, Bs0) of
	{yes,Bs1} -> match(Ps, Vs, Env, Bs1);
	no -> no
    end;
match([], [], _, Bs) -> {yes,Bs};
match(Symb, Val, Env, Bs) when is_atom(Symb) ->
    match_symb(Symb, Val, Env, Bs);
match(Val, Val, _, Bs) -> {yes,Bs};
match(_, _, _, _) -> no.

match_symb('_', _, _, Bs) -> {yes,Bs};		%Don't care variable.
match_symb(Symb, Val, _, Bs) ->
    %% Check if Symb already bound.
    case find(Symb, Bs) of
	{ok,_} -> no;				%Already bound, multiple var
	error -> {yes,store(Symb, Val, Bs)}	%Not yet bound
    end.

%% match_binary(Fields, Binary, Env, Bindings) -> {yes,Bindings} | no.
%%  Match Fields against Binary. This code is taken from
%%  eval_bits.erl.  Use catch to trap bad matches when getting value,
%%  errors become no match. Split into two passes so as to throw error
%%  on bitspec error and return no on all no matches.

match_binary(Fs, Bin, Env, Bs0) ->
    Psps = map(fun (F) -> parse_field(F, Env) end, Fs),
    %% io:format("~p\n", [Psps]),
    case catch match_fields(Psps, Bin, Env, Bs0) of
	{yes,<<>>,Bs1} -> {yes,Bs1};		%Matched whole binary
	{yes,_,_} -> no;			%Matched part of binary
	no -> no
    end.

match_fields([{Pat,Specs}|Psps], Bin0, Env, Bs0) ->
    case match_field(Pat, Specs, Bin0, Env, Bs0) of
	{yes,Bin1,Bs1} -> match_fields(Psps, Bin1, Env, Bs1);
	no -> no
    end;
match_fields([], Bin, _, Bs) -> {yes,Bin,Bs}.

match_field(Pat, Spec, Bin0, Env, Bs0) ->
    {Val,Bin1} = get_pat_field(Bin0, Spec),
    case match(Pat, Val, Env, Bs0) of
	{yes,Bs1} -> {yes,Bin1,Bs1};
	no -> no
    end.

%% get_pat_field(Binary, {Type,Size,Unit,Sign,Endian}) -> {Value,RestBinary}.

get_pat_field(Bin, Spec) ->
    case Spec of
	%% Integer types.
	{integer,Sz,Un,Si,En} ->
	    get_int_field(Bin, Sz*Un, Si, En);
	%% Unicode types, ignore unused fields.
	{utf8,_,_,_,_} -> get_utf8_field(Bin);
	{utf16,_,_,_,En} -> get_utf16_field(Bin, En);
	{utf32,_,_,_,En} -> get_utf32_field(Bin, En);
	%% Float types.
	{float,Sz,Un,_,En} -> get_float_field(Bin, Sz*Un, En);
	%% Binary types.
	{binary,all,Un,_,_} ->
	    0 = (erlang:bit_size(Bin) rem Un),
	    {Bin,<<>>};
	{binary,Sz,Un,_,_} ->
	    TotSize = Sz * Un,
	    <<Val:TotSize/bitstring,Rest/bitstring>> = Bin,
	    {Val,Rest}
    end.

get_int_field(Bin, Sz, signed, little) ->
    <<Val:Sz/little-signed,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_int_field(Bin, Sz, unsigned, little) ->
    <<Val:Sz/little-unsigned,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_int_field(Bin, Sz, signed, native) ->
    <<Val:Sz/native-signed,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_int_field(Bin, Sz, unsigned, native) ->
    <<Val:Sz/native-unsigned,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_int_field(Bin, Sz, signed, big) ->
    <<Val:Sz/big-signed,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_int_field(Bin, Sz, unsigned, big) ->
    <<Val:Sz/big-unsigned,Rest/binary-unit:1>> = Bin,
    {Val,Rest}.

get_utf8_field(Bin) ->
    <<Val/utf8,Rest/bitstring>> = Bin,
    {Val,Rest}.

get_utf16_field(Bin, little) ->
    <<Val/utf16-little,Rest/bitstring>> = Bin,
    {Val,Rest};
get_utf16_field(Bin, native) ->
    <<Val/utf16-native,Rest/bitstring>> = Bin,
    {Val,Rest};
get_utf16_field(Bin, big) ->
    <<Val/utf16-big,Rest/bitstring>> = Bin,
    {Val,Rest}.

get_utf32_field(Bin, little) ->
    <<Val/utf32-little,Rest/bitstring>> = Bin,
    {Val,Rest};
get_utf32_field(Bin, native) ->
    <<Val/utf32-native,Rest/bitstring>> = Bin,
    {Val,Rest};
get_utf32_field(Bin, big) ->
    <<Val/utf32-big,Rest/bitstring>> = Bin,
    {Val,Rest}.

get_float_field(Bin, Sz, little) ->
    <<Val:Sz/float-little,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_float_field(Bin, Sz, native) ->
    <<Val:Sz/float-native,Rest/binary-unit:1>> = Bin,
    {Val,Rest};
get_float_field(Bin, Sz, big) ->
    <<Val:Sz/float,Rest/binary-unit:1>> = Bin,
    {Val,Rest}.
