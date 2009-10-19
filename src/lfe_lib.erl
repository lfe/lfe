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

%%% File    : lfe_lib.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang library of miscellaneous functions.

-module(lfe_lib).

%% Environment functions.
-export([new_env/0,add_env/2,
	 add_vbinding/3,add_vbindings/2,is_vbound/2,get_vbinding/2,
	 fetch_vbinding/2,update_vbinding/3,
	 add_fbinding/4,add_fbindings/2,update_fbinding/4,
	 is_fbound/3,get_fbinding/3,add_ibinding/5,
	 is_gbound/3,get_gbinding/3,
	 add_mbinding/3,is_mbound/2,get_mbinding/2]).

%% General library functions.
-export([is_bif/2,is_erl_bif/2,is_guard_bif/2]).

-export([is_symb/1,is_symb_list/1,is_proper_list/1,is_core_form/1]).

-export([proc_forms/3]).

%% Standard lisp library.
-export([is_lfe_bif/2,acons/3,assoc/2,rassoc/2,
	 subst/3,'subst-if'/3,'subst-if-not'/3,
	 eval/1]).

%% Miscellaneous useful LFE functions.
-export([format_exception/6,format_stacktrace/3]).

-import(lists, [reverse/1,reverse/2,map/2,foldl/3,dropwhile/2]).
-import(orddict, [find/2,store/3]).

%% -compile([export_all]).

%% Environment functions.
%% new_env() -> Env.
%% add_env(Env1, Env2) -> Env.
%% add_vbinding(Name, Val, Env) -> Env.
%% add_vbindings([{Name,Val}], Env) -> Env.
%% update_vbinding(Name, Val, Env) -> Env.
%% is_vbound(Symb, Env) -> bool().
%% get_vbinding(Name, Env) -> {yes,Val} | no.
%% fetch_vbinding(Name, Env) -> Val.
%% add_fbinding(Name, Arity, Val, Env) -> Env.
%% add_fbindings([{Name,Arity,Val}], Env) -> Env.
%% update_fbinding(Name, Arity, Val, Env) -> Env.
%% add_ibinding(Mod, Name, Arity, LocalName, Env) -> Env.
%% is_fbound(Symb, Arity, Env) -> bool().
%% is_gbound(Symb, Arity, Env) -> bool().
%% get_fbinding(Name, Arity, Env) -> {yes,Val} | {yes,Mod,Name} | no.
%% add_mbinding(Name, Macro, Env) -> Env.
%% is_mbound(Symb, Env) -> bool().
%% get_mbinding(Name, Env) -> {yes,Macro} | no.
%%
%%  Unfortunately the searching property means we can not use a simple
%%  dictionary but need an ordered sequence.
%%
%%  The dictionary has structures:
%%  Variables - {Name,Value}
%%  Functions - {function,Name,Arity,Value}
%%  Imports -   {function,Local,Arity,Module,Remote}
%%  Macros -    {macro,Name,Value}.
%%
%%  Macros and functions occupy the same environment so they shadow
%%  each other.
%%
%%  add_Xbinding adds a completely *new* binding to the head of Env.
%%  update_Xbinding finds the closest existing binding and updates it.

new_env() -> [].

add_env(E1, E2) -> E1 ++ E2.

add_vbinding(N, V, Env) -> [{N,V}|Env].

add_vbindings(Vbs, Env) ->
    %% Vbs ++ Env because we KNOW.
    foldl(fun ({N,V}, E) -> add_vbinding(N, V, E) end, Env, Vbs).

update_vbinding(N, V, [{N,_}|Env]) -> [{N,V}|Env];
update_vbinding(N, V, [Vb|Env]) ->
    [Vb|update_vbinding(N, V, Env)].

is_vbound(N, [{N,_}|_]) -> true;
is_vbound(N, [_|Env]) -> is_vbound(N, Env);
is_vbound(_, []) -> false.

get_vbinding(N, [{N,V}|_]) -> {yes,V};
get_vbinding(N, [_|Env]) -> get_vbinding(N, Env);
get_vbinding(_, []) -> no.

fetch_vbinding(N, [{N,V}|_]) -> V;
fetch_vbinding(N, [_|Env]) -> fetch_vbinding(N, Env).

add_fbinding(N, A, V, Env) -> [{function,N,A,V}|Env].

add_fbindings(Fbs, Env) ->
    foldl(fun ({N,Ar,V}, E) -> add_fbinding(N, Ar, V, E) end, Env, Fbs).

update_fbinding(N, A, V, [{function,N,A,_}|Env]) ->
    [{function,N,A,V}|Env];
update_fbinding(N, A, V, [Fb|Env]) ->
    [Fb|update_fbinding(N, A, V, Env)].

add_ibinding(M, R, A, L, Env) -> [{function,L,A,M,R}|Env].

is_fbound(N, A, [{function,N,A,_}|_]) -> true;
is_fbound(N, A, [{function,N,A,_,_}|_]) -> true;
is_fbound(N, _, [{macro,N,_}|_]) -> false;	%Macros shadow
is_fbound(N, A, [_|Env]) -> is_fbound(N, A, Env);
is_fbound(N, A, []) -> is_bif(N, A).    	%Known BIF, LFE or erlang

get_fbinding(N, A, [{function,N,A,V}|_]) -> {yes,V};
get_fbinding(N, A, [{function,N,A,M,F}|_]) -> {yes,M,F};	%Import
get_fbinding(N, _, [{macro,N,_}|_]) -> no;			%Macros shadow
get_fbinding(N, A, [_|Env]) -> get_fbinding(N, A, Env);
get_fbinding(N, A, []) ->
    %% First check if is an LFE BIF.
    case is_lfe_bif(N, A) of
	true -> {yes,lfe_lib,N};
	false ->
	    %% Now check if it is a known BIF.
	    case is_erl_bif(N, A) of
		true -> {yes,erlang,N};
		false -> no
	    end
    end.

is_gbound(N, A, [{function,N,A,_}|_]) -> false;
is_gbound(N, A, [{function,N,A,_,_}|_]) -> false;
is_gbound(N, _, [{macro,N,_}|_]) -> false;	%Macros shadow
is_gbound(N, A, [_|Env]) -> is_gbound(N, A, Env);
is_gbound(N, A, []) -> is_guard_bif(N, A).    	%Known guard BIF

get_gbinding(N, A, [{function,N,A,_}|_]) -> no;
get_gbinding(N, A, [{function,N,A,_,_}|_]) -> no;	%Import
get_gbinding(N, _, [{macro,N,_}|_]) -> no;		%Macros shadow
get_gbinding(N, A, [_|Env]) -> get_gbinding(N, A, Env);
get_gbinding(N, A, _) ->
    case is_guard_bif(N, A) of
	true -> {yes,erlang,N};
	false -> no
    end.

add_mbinding(M, V, Env) -> [{macro,M,V}|Env].

is_mbound(N, [{function,N,_,_}|_]) -> false;	%Functions shadow
is_mbound(N, [{function,N,_,_,_}|_]) -> false;	%Functions shadow
is_mbound(N, [{macro,N,_}|_]) -> true;
is_mbound(N, [_|Env]) -> is_mbound(N, Env);
is_mbound(_, []) -> false.

get_mbinding(N, [{function,N,_,_}|_]) -> no;	%Functions shadow
get_mbinding(N, [{function,N,_,_,_}|_]) -> no;	%Functions shadow
get_mbinding(N, [{macro,N,V}|_]) -> {yes,V};
get_mbinding(N, [_|Env]) -> get_mbinding(N, Env);
get_mbinding(_, []) -> no.

%% is_bif(Name, Arity) -> bool().
%% is_erl_bif(Name, Arity) -> bool().
%% is_guard_bif(Name, Arity) -> bool().
%% Collected tests for valid BIFs in guards and expressions.

is_bif(Name, Ar) ->
    is_lfe_bif(Name, Ar) orelse is_erl_bif(Name, Ar).

is_erl_bif(Op, Ar) ->
    erl_internal:bif(Op, Ar)
	orelse erl_internal:arith_op(Op, Ar)
	orelse erl_internal:bool_op(Op, Ar)
	orelse erl_internal:comp_op(Op, Ar)
	orelse erl_internal:list_op(Op, Ar)
	orelse erl_internal:send_op(Op, Ar).

is_guard_bif(Op ,Ar) ->
    erl_internal:guard_bif(Op, Ar)
	orelse erl_internal:arith_op(Op, Ar)
	orelse erl_internal:bool_op(Op, Ar)
	orelse erl_internal:comp_op(Op, Ar).

%% is_symb(Sexpr) -> bool().
%% is_symb_list(Sexprs) -> bool().
%% is_proper_list(Sexprs) -> bool().

is_symb(S) -> is_atom(S).

is_symb_list([S|Ss]) when is_atom(S) ->
    is_symb_list(Ss);
is_symb_list([]) -> true;
is_symb_list(_) -> false.			%Might not be a proper list

is_proper_list([_|Ss]) -> is_proper_list(Ss);
is_proper_list([]) -> true;
is_proper_list(_) -> false.

%% is_core_form(Form) -> bool().
%%  Return true if Form (name) is one of the LFE core forms, else false.

%% Core data special forms.
is_core_form(quote) -> true;
is_core_form(cons) -> true;
is_core_form(car) -> true;
is_core_form(cdr) -> true;
is_core_form(list) -> true;
is_core_form(tuple) -> true;
is_core_form(binary) -> true;
%% Core closure special forms.
is_core_form(lambda) -> true;
is_core_form('match-lambda') -> true;
is_core_form('let') -> true;
is_core_form('let-function') -> true;
is_core_form('letrec-function') -> true;
is_core_form('let-macro') -> true;
is_core_form('eval-when-compile') -> true;
is_core_form('define-function') -> true;
is_core_form('define-macro') -> true;
%% Core control special forms.
is_core_form('progn') -> true;
is_core_form('if') -> true;
is_core_form('case') -> true;
is_core_form('receive') -> true;
is_core_form('catch') -> true;
is_core_form('try') -> true;
is_core_form('funcall') -> true;
is_core_form(call) -> true;
%% Everything else is not special.
is_core_form(_) -> false.

%% proc_forms(FormFun, Forms, State) -> {Forms,State}.
%%  Process a (progn ... ) nested list of forms where top level list
%%  has elements {Form,LineNumber}. Return a flat list of results and
%%  passes through State. All the elements are processed left to
%%  right.

proc_forms(Fun, Fs, St) -> proc_forms(Fun, Fs, [], St).

proc_forms(Fun, [{['progn'|Bs],L}|Fs], Rs0, St0) ->
    {Rs1,St1} = proc_forms_progn(Fun, Bs, L, Rs0, St0),
    proc_forms(Fun, Fs, Rs1, St1);
proc_forms(Fun, [{F,L}|Fs], Rs, St0) ->
    {Frs,St1} = Fun(F, L, St0),
    proc_forms(Fun, Fs, reverse(Frs, Rs), St1);
proc_forms(_, [], Rs, St) -> {reverse(Rs),St}.

proc_forms_progn(Fun, [['progn'|Bbs]|Bs], L, Rs0, St0) ->
    {Rs1,St1} = proc_forms_progn(Fun, Bbs, L, Rs0, St0),
    proc_forms_progn(Fun, Bs, L, Rs1, St1);
proc_forms_progn(Fun, [B|Bs], L, Rs, St0) ->
    {Frs,St1} = Fun(B, L, St0),
    proc_forms_progn(Fun, Bs, L, reverse(Frs, Rs), St1);
proc_forms_progn(_, [], _, Rs, St) ->
    {Rs,St}.

%% proc_forms(Fun, [{['progn'|Bs],L}|Fs], Rs, St) ->
%%     proc_forms_progn(Fun, Bs, L, [], Fs, Rs, St);
%% proc_forms(Fun, [{F,L}|Fs], Rs, St0) ->
%%     {Frs,St1} = Fun(F, L, St0),
%%     proc_forms(Fun, Fs, reverse(Frs, Rs), St1);
%% proc_forms(_, [], Rs, St) -> {reverse(Rs),St}.

%% proc_forms_progn(Fun, [['progn'|Bs1]|Bs], L, Bss, Fs, Rs, St) ->
%%     proc_forms_progn(Fun, Bs1, L, [Bs|Bss], Fs, Rs, St);
%% proc_forms_progn(Fun, [B|Bs], L, Bss, Fs, Rs, St0) ->
%%     {Frs,St1} = Fun(B, L, St0),
%%     proc_forms_progn(Fun, Bs, L, Bss, Fs, reverse(Frs, Rs), St1);
%% proc_forms_progn(Fun, [], L, [Bs|Bss], Fs, Rs, St) ->
%%     proc_forms_progn(Fun, Bs, L, Bss, Fs, Rs, St);
%% proc_forms_progn(Fun, [], _, [], Fs, Rs, St) ->
%%     proc_forms(Fun, Fs, Rs, St).

%% Standard lisp library functions.
%% is_lfe_bif(Name, Arity) -> bool().
%% acons(Key, Value, Alist) -> Alist.
%% assoc(Key, Alist) -> [Key|Value] | [].
%% rassoc(Value, Alist) -> [Key|Value] | [].
%% subst(New, Old, Tree) -> Tree.
%% subst-if(New, Test, Tree) -> Tree.
%% subst-if-not(New, Test, Tree) -> Tree.
%% eval(Sexpr) -> Value.

is_lfe_bif(acons, 3) -> true;
is_lfe_bif(assoc, 2) -> true;
is_lfe_bif(rassoc, 2) -> true;
is_lfe_bif(subst, 3) -> true;
is_lfe_bif('subst-if', 3) -> true;
is_lfe_bif('subst-if-not', 3) -> true;
is_lfe_bif(eval, 1) -> true;
is_lfe_bif(_, _) -> false.

acons(K, V, Alist) -> [[K|V]|Alist].

assoc(K, [[K|_]=Pair|_]) -> Pair;
assoc(K, [_|L]) -> assoc(K, L);
assoc(_, []) -> [].

rassoc(V, [[_|V]=Pair|_]) -> Pair;
rassoc(V, [_|L]) -> rassoc(V, L);
rassoc(_, []) -> [].

%% subst(New, Old, Tree) -> Tree.

subst(New, Old, Old) -> New;
subst(New, Old, [H|T]) ->
    [subst(New, Old,H)|subst(New, Old, T)];
subst(_, _, Tree) -> Tree.

%% subst-if(New, Test, Tree) -> Tree.

'subst-if'(New, Test, Tree) ->
    case Test(Tree) of
	true -> New;
	false ->
	    case Tree of
		[H|T] ->
		    ['subst-if'(New, Test, H)|'subst-if'(New, Test, T)];
		_ -> Tree
	    end
    end.

%% subst-if-not(New, Test, Tree) -> Tree.

'subst-if-not'(New, Test, Tree) ->
    case Test(Tree) of
	false -> New;
	true ->
	    case Tree of
		[H|T] ->
		    ['subst-if-not'(New, Test, H)|'subst-if-not'(New, Test, T)];
		_ -> Tree
	    end
    end.

eval(Sexpr) -> lfe_eval:expr(Sexpr, new_env()).	%Empty environment.

%% Miscellaneous useful LFE functions.

%% format_exception(Class, Error, Stacktrace, SkipFun, FormatFun, Indentation)
%%      -> DeepCharList.
%%  Format an exception. Class, Error and Stacktrace describe the
%%  exception; SkipFun is used to trim the end of stack; FormatFun is
%%  used to format terms; and Indentation is the current column.

format_exception(throw, Error, St, Sf, Ff, I) ->
    P = "exception throw: ",
    [P,lfe_io:prettyprint1(Error, 10, length(P)+I),"\n",
     format_stacktrace(St, Sf, Ff)];
format_exception(exit, Error, St, Sf, Ff, I) ->
    P = "exception exit: ",
    [P,lfe_io:prettyprint1(Error, 10, length(P)+I),"\n",
     format_stacktrace(St, Sf, Ff)];
format_exception(error, Error, St, Sf, Ff, I) ->
    P = "exception error: ",
    [P,lfe_io:prettyprint1(Error, 10, length(P)+I),"\n",
     format_stacktrace(St, Sf, Ff)].

%% format_stacktrace(Stacktrace, SkipFun, FormatFun) -> DeepCharList.
%%  Format a stacktrace. SkipFun is used to trim the end of stack;
%%  FormatFun is used to format terms; and Indentation is the current
%%  column.

format_stacktrace(St0, Sf, Ff) ->
    St1 = reverse(dropwhile(fun ({M,F,A}) -> Sf(M, F, A) end, reverse(St0))),
    map(fun ({M,F,A}) when is_integer(A) ->
		lfe_io:format1("  in (~w ~w ~w)\n", [M,F,A]);
	    ({M,F,A}) -> ["  in ",Ff([':',M,F|A], 5),"\n"]
	end, St1).
