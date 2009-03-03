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

%%% File    : lfe_pmod.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang paramterised module transformer.

-module(lfe_pmod).

-export([module/2]).

%% -compile(export_all).

-import(lists, [member/2,keysearch/3,
		all/2,map/2,foldl/3,foldr/3,mapfoldl/3,mapfoldr/3,
		concat/1]).
-import(ordsets, [add_element/2,is_element/2,from_list/1,union/2]).
-import(orddict, [store/3,find/2]).

-import(lfe_lib, [new_env/0,add_vbinding/3,add_vbindings/2,get_vbinding/2,
		  add_fbinding/4,add_fbindings/2,get_fbinding/3,
		  add_ibinding/5,get_gbinding/3]).

-record(param, {mod=[],				%Module name
		pars=[],			%Module parameters
		env=[]}).			%Environment

%% module(Forms, Options) -> Forms.
%%  Expand the forms to handle parameterised modules if necessary,
%%  otherwise just pass forms straight through.

module([{['define-module',[_|_]|_],_}|_]=Fs, Opts) ->
    expand_module(Fs, Opts);
module(Fs, _) -> Fs.				%Normal module, do nothing

expand_module(Fs0, Opts) ->
    St0 = #param{env=new_env()},
    {Fs1,St1} = lfe_lib:proc_forms(fun exp_form/3, Fs0, St0),
    debug_print("#param: ~p\n", [{Fs1,St1}], Opts),
    %% {ok,_} = lfe_lint:module(Fs1, Opts),
    Fs1.

exp_form(['define-module',[Mod|Ps]|Mdef0], L, St0) ->
    %% Save the good bits and define new/N and instance/N.
    St1 = St0#param{mod=Mod,pars=Ps},
    New = ['define-function',new,[lambda,Ps,[instance|Ps]]],
    Inst = ['define-function',instance,[lambda,Ps,[tuple,[quote,Mod]|Ps]]],
    {Mdef1,St2} = exp_mdef(Mdef0, St1),
    {[{['define-module',Mod|Mdef1],L},
      {New,L},{Inst,L}],St2};
exp_form(['define-function',F,Def0], L, St) ->
    Def1 = exp_function(Def0, St),
    {[{['define-function',F,Def1],L}],St};
exp_form(F, L, St) ->
    {[{F,L}],St}.

debug_print(Format, Args, Opts) ->
    case member(debug_print, Opts) of
	true -> io:fwrite(Format, Args);
	false -> ok
    end.

exp_mdef([[export,all]|Mdef0], St0) ->		%Leave unchanged
    {Mdef1,St1} = exp_mdef(Mdef0, St0),
    {[[export,all]|Mdef1],St1};
exp_mdef([[export|Es0]|Mdef0], St0) ->
    Ar = length(St0#param.pars),
    Es1 = map(fun ([F,A]) -> [F,A+1] end, Es0),
    {Mdef1,St1} = exp_mdef(Mdef0, St0),
    {[[export,[new,Ar],[instance,Ar]|Es1]|Mdef1],St1};
exp_mdef([[import|Is]|Mdef0], St0) ->
    St1 = collect_imps(Is, St0),
    {Mdef1,St2} = exp_mdef(Mdef0, St1),
    {[[import|Is]|Mdef1],St2};
exp_mdef([Md|Mdef0], St0) ->
    {Mdef1,St1} = exp_mdef(Mdef0, St0),
    {[Md|Mdef1],St1};
exp_mdef([], St) -> {[[abstract,true]],St}.

collect_imps([['from',M|Fs]|Is], St) ->
    Env = foldl(fun ([F,Ar], E) -> add_ibinding(M, F, Ar, F, E) end,
		St#param.env, Fs),
    collect_imps(Is, St#param{env=Env});
collect_imps([['rename',M|Fs]|Is], St) ->
    Env = foldl(fun ([[F,Ar],R], E) -> add_ibinding(M, F, Ar, R, E) end,
		St#param.env, Fs),
    collect_imps(Is, St#param{env=Env});
collect_imps([], St) -> St.
    
exp_function(['match-lambda'|Cls0], #param{pars=Ps,env=Env}) ->
    Cls1 = map(fun ([As|Body]) ->
		       exp_clause([lambda_args(As, Ps)|Body], Env)
	       end, Cls0),
    ['match-lambda'|Cls1];
exp_function([lambda,As|Body0], #param{pars=Ps,env=Env}) ->
    Body1 = exp_list(Body0, Env),
    ['match-lambda',[lambda_args(As, Ps)|Body1]].

lambda_args(As, Pars) ->
    As ++ [['=',this,[tuple,'_'|Pars]]].

%% exp_expr(Sexpr, Environment) -> Expr.
%%  Expand Sexpr.

%% Handle the Core data special forms.
exp_expr([quote|_]=E, _) -> E;
exp_expr([cons,H,T], Env) ->
    [cons,exp_expr(H, Env),exp_expr(T, Env)];
exp_expr([car,E], Env) -> [car,exp_expr(E, Env)]; %Provide lisp names
exp_expr([cdr,E], Env) -> [cdr,exp_expr(E, Env)];
exp_expr([list|Es], Env) -> [list|exp_list(Es, Env)];
exp_expr([tuple|Es], Env) -> [tuple|exp_list(Es, Env)];
exp_expr([binary|Bs], Env) ->
    [binary|exp_binary(Bs, Env)];
%% Handle the Core closure special forms.
exp_expr([lambda|Body], Env) ->
    [lambda|exp_lambda(Body, Env)];
exp_expr(['match-lambda'|Cls], Env) ->
    ['match-lambda'|exp_match_lambda(Cls, Env)];
exp_expr(['let'|Body], Env) ->
    ['let'|exp_let(Body, Env)];
exp_expr(['let-function'|Body], Env) ->
    ['let-function'|exp_let_function(Body, Env)];
exp_expr(['letrec-function'|Body], Env) ->
    ['letrec-function'|exp_letrec_function(Body, Env)];
%% Handle the control special forms.
exp_expr(['progn'|Body], Env) ->
    [progn|exp_body(Body, Env)];
exp_expr(['if'|Body], Env) ->
    ['if'|exp_if(Body, Env)];
exp_expr(['case'|Body], Env) ->
    ['case'|exp_case(Body, Env)];
exp_expr(['receive'|Body], Env) ->
    ['receive'|exp_receive(Body, Env)];
exp_expr(['catch'|Body], Env) ->
    ['catch'|exp_body(Body, Env)];
exp_expr(['try'|Body], Env) ->
    ['try'|exp_try(Body, Env)];
exp_expr([funcall,F|As], Env) ->
    [funcall,exp_expr(F, Env)|exp_list(As, Env)];
exp_expr([call|Body], Env) ->
    [call|exp_call(Body, Env)];
exp_expr([Fun|Es], Env) when is_atom(Fun) ->
    Ar = length(Es),
    case get_fbinding(Fun, Ar, Env) of
	{yes,_,_} -> [Fun|exp_list(Es, Env)];	%Imported or Bif
	{yes,local} -> [Fun|exp_list(Es, Env)];	%Local function
	_ -> [Fun|exp_list(Es, Env) ++ [this]]
    end;
exp_expr(E, _) when is_atom(E) -> E;
exp_expr(E, _) -> E.				%Atoms expand to themselves.

exp_list(Es, Env) ->
    map(fun (E) -> exp_expr(E, Env) end, Es).

exp_body(Es, Env) ->
    map(fun (E) -> exp_expr(E, Env) end, Es).

exp_binary(Segs, Env) ->
    map(fun (S) -> exp_bitseg(S, Env) end, Segs).

exp_bitseg([N|Specs0], Env) ->
    %% The only bitspec that needs expanding is size.
    Specs1 = map(fun ([size,S]) -> [size,exp_expr(S, Env)];
		     (S) -> S
		 end, Specs0),
    [exp_expr(N, Env)|Specs1];
exp_bitseg(N, Env) -> exp_expr(N, Env).

exp_lambda([As|Body], Env) ->
    [As|exp_list(Body, Env)].

exp_match_lambda(Cls, Env) ->
    exp_clauses(Cls, Env).

exp_clauses(Cls, Env) ->
    map(fun (Cl) -> exp_clause(Cl, Env) end, Cls).

exp_clause([P,['when'|_]=G|Body], Env) -> [P,G|exp_body(Body, Env)];
exp_clause([P|Body], Env) -> [P|exp_body(Body, Env)].

exp_let([Vbs|Body], Env) ->
    Evbs = map(fun ([P,E]) -> [P,exp_expr(E, Env)];
		   ([P,G,E]) -> [P,G,exp_expr(E, Env)]
	       end, Vbs),
    [Evbs|exp_body(Body, Env)].

%% exp_let_function(FletBody, Env) -> FletBody.
%% exp_letrec_function(FletrecBody, Env) -> FletrecBody.
%%  The only difference is the order in which the environment is updated.

exp_let_function([Fbs|Body], Env0) ->
    Efbs = map(fun ([F,Def]) -> [F,exp_expr(Def, Env0)] end, Fbs),
    Env1 = foldl(fun ([F,Def], E) ->
			 add_fbinding(F,lambda_arity(Def),local,E)
		 end, Env0, Fbs),
    [Efbs|exp_body(Body, Env1)].

exp_letrec_function([Fbs|Body], Env0) ->
    Env1 = foldl(fun ([F,Def], E) ->
			 add_fbinding(F,lambda_arity(Def),local,E)
		 end, Env0, Fbs),
    Efbs = map(fun ([F,Def]) -> [F,exp_expr(Def, Env1)] end, Fbs),
    [Efbs|exp_body(Body, Env1)].

exp_if([Test,True], Env) ->
    [exp_expr(Test, Env),exp_expr(True, Env)];
exp_if([Test,True,False], Env) ->
    [exp_expr(Test, Env),exp_expr(True, Env),exp_expr(False, Env)].

exp_case([E|Cls], Env) ->
    [exp_expr(E, Env)|exp_clauses(Cls, Env)].

exp_receive(Cls, Env) ->
    map(fun (Cl) -> exp_rec_clause(Cl, Env) end, Cls).

exp_rec_clause(['after',T|Body], Env) ->
    ['after',exp_expr(T, Env)|exp_body(Body, Env)];
exp_rec_clause(Cl, Env) -> exp_clause(Cl, Env).

exp_try([E|Body], Env) ->
    [exp_expr(E, Env)|
     map(fun (['case'|Cls]) -> ['case'|exp_clauses(Cls, Env)];
	     (['catch'|Cls]) -> ['catch'|exp_clauses(Cls, Env)];
	     (['after'|Body]) -> ['after'|exp_body(Body, Env)]
	 end, Body)].

exp_call([M,F|As], Env) ->
    [exp_expr(M, Env),exp_expr(F, Env)|exp_list(As, Env)].

lambda_arity([lambda,As|_]) -> length(As);
lambda_arity(['match-lambda',[As|_]|_]) -> length(As).
