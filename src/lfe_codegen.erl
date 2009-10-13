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

%%% File    : lfe_codegen.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang code generator (to core Erlang).

-module(lfe_codegen).

-export([forms/2]).

%% -compile(export_all).

-import(lists, [member/2,keysearch/3,reverse/1,
		all/2,map/2,foldl/3,foldr/3,mapfoldl/3,mapfoldr/3,
		concat/1,zipwith/3]).
-import(ordsets, [add_element/2,is_element/2,from_list/1,union/2]).
-import(orddict, [store/3,find/2]).

-import(lfe_lib, [new_env/0,add_env/2,
		  add_vbinding/3,get_vbinding/2,add_fbinding/4,get_fbinding/3,
		  add_ibinding/5,get_gbinding/3]).

-include_lib("compiler/src/core_parse.hrl").

-record(cg, {opts=[],				%Options
	     vc=0,				%Variable counter
	     fc=0,				%Function counter
	     mod=[],				%Module name
	     exps=[],				%Exports (ordsets)
	     imps=[],				%Imports (orddict)
	     pref=[],				%Prefixes
	     atts=[],				%Attrubutes
	     defs=[],				%Function definitions.
	     env=[],
	     func=[]}).

%% forms(Forms, Options) -> {ModuleName,CoreModule}

forms(Forms, Opts) ->
    St0 = #cg{opts=Opts},
    Core0 = #c_module{defs=[],exports=[],attrs=[]},
    {Core1,St1} = forms(Forms, St0, Core0),
    {St1#cg.mod,Core1}.

%% forms(Forms, State, CoreModule) -> {CoreModule,State}.
%% Compile the forms from the file as stored in the state record.

forms(Forms, St0, Core0) ->
    %% Collect the module definition and functions definitions.
    {Fbs0,St1} = lfe_lib:proc_forms(fun collect_form/3, Forms, St0),
    %% Add predefined functions and definitions.
    Predefs = [{module_info,0},{module_info,1}],
    Fbs1 = [{module_info,
	     [lambda,[],
	      [call,[quote,erlang],[quote,get_module_info],
	       [quote,St1#cg.mod]]],1},
	    {module_info,
	     [lambda,[x],
	      [call,[quote,erlang],[quote,get_module_info],
	       [quote,St1#cg.mod],x]],1}|
	    Fbs0],
    %% Make initial environment and set state
    Env0 = foldl(fun ({M,Fs}, Env) ->
			 foldl(fun ({{F,A},R}, E) ->
				       add_ibinding(M, F, A, R, E)
			       end, Env, Fs)
		 end, new_env(), St1#cg.imps),
    Env1 = foldl(fun ({Name,Def,_}, E) ->
			 add_fbinding(Name, func_arity(Def), Name, E)
		 end, Env0, Fbs1),
    St2 = St1#cg{exps=add_exports(St1#cg.exps, Predefs),
		 defs=Fbs1,env=Env1},
    Exps = make_exports(St2#cg.exps, Fbs1),
    Atts = map(fun ({N,V}) ->
		       {comp_lit(N),comp_lit(V)}
	       end, St2#cg.atts),
    %% Compile the functions.
    {Cdefs,St3} = mapfoldl(fun (D, St) -> comp_define(D, Env1, St) end,
			  St2, St2#cg.defs),
    %% Build the final core module structure.
    Core1 = Core0#c_module{name=c_atom(St3#cg.mod),
			   exports=Exps,
			   attrs=Atts,
			   defs=Cdefs},
    %% Maybe print lots of debug info.
    debug_print("#cg: ~p\n", [St3], St3),
    when_opt(fun () -> io:fwrite("core_lint: ~p\n",
				 [(catch core_lint:module(Core1))])
	     end, debug_print, St3),
    when_opt(fun () ->
		     Pp = (catch io:put_chars([core_pp:format(Core1),$\n])),
		     io:fwrite("core_pp: ~p\n", [Pp])
	     end, debug_print, St3),
    %% debug_print("#core: ~p\n", [Core1], St3),
    {Core1,St3}.

debug_print(Format, Args, St) ->
    when_opt(fun () -> io:fwrite(Format, Args) end, debug_print, St).

when_opt(Fun, Opt, St) ->
    case member(Opt, St#cg.opts) of
	true -> Fun();
	false -> ok
    end.

add_exports(all, _) -> all;
add_exports(_, all) -> all;
add_exports(Old, More) -> union(Old, More).

make_exports(all, Fbs) ->
    map(fun ({F,Def,_}) -> c_fname(F, func_arity(Def)) end, Fbs);
make_exports(Exps, _) ->
    map(fun ({F,A}) -> c_fname(F, A) end, Exps).

%% collect_form(Form, Line, State} -> {[Ret],State}.
%%  Collect valid forms and module data. Returns forms and put module
%%  data into state.

collect_form(['define-module',Mod|Mdef], _, St) ->
    %% Everything into State
    {[],collect_mdef(Mdef, St#cg{mod=Mod})};
collect_form(['define-function',Name,[lambda|_]=Lambda], L, St) ->
    {[{Name,Lambda,L}],St};
collect_form(['define-function',Name,['match-lambda'|_]=Match], L, St) ->
    {[{Name,Match,L}],St}.

%% collect_props(ModDef, State) -> State.
%% Collect module definition and fill in the #cg state record.

collect_mdef([[export,all]|Mdef], St) ->
    collect_mdef(Mdef, St#cg{exps=all});
collect_mdef([[export|Es]|Mdef], St) ->
    case St#cg.exps of
	all -> collect_mdef(Mdef, St);		%Propagate all.
	Exps0 ->
	    %% Add exports to export set.
	    Exps1 = foldl(fun ([F,A], E) -> add_element({F,A}, E) end,
			  Exps0, Es),
	    collect_mdef(Mdef, St#cg{exps=Exps1})
    end;
collect_mdef([[import|Is]|Mdef], St) ->
    collect_mdef(Mdef, collect_imps(Is, St));
collect_mdef([[N|Vs]|Mdef], St) ->
    As = St#cg.atts ++ [{N,Vs}],		%Probably not many
    collect_mdef(Mdef, St#cg{atts=As});
collect_mdef([], St) -> St.

collect_imps([['from',Mod|Fs]|Is], St0) ->
    St1 = collect_imp(fun ([F,A], Imps) -> store({F,A}, F, Imps) end,
		      Mod, St0, Fs),
    collect_imps(Is, St1);
collect_imps([['rename',Mod|Rs]|Is], St0) ->
    St1 = collect_imp(fun ([[F,A],R], Imps) -> store({F,A}, R, Imps) end,
		      Mod, St0, Rs),
    collect_imps(Is, St1);
collect_imps([['prefix',Mod,Pre]|Is], St) ->
    Pstr = atom_to_list(Pre),
    Pref = store(Pstr, Mod, St#cg.pref),
    collect_imps(Is, St#cg{pref=Pref});
collect_imps([], St) -> St.

collect_imp(Fun, Mod, St, Fs) ->
    Imps0 = safe_fetch(Mod, St#cg.imps, []),
    Imps1 = foldl(Fun, Imps0, Fs),
    St#cg{imps=store(Mod, Imps1, St#cg.imps)}.

%% comp_define(DefForm, Env, State) -> {Corefunc,State}.
%%  Compile a top-level define. Sets current function name.

comp_define({Name,Def,L}, Env, St) ->
    Cf = c_fname(Name, func_arity(Def)),	%Could be useful
    comp_func(Name, Def, Env, L, St#cg{func=Cf,vc=0,fc=0}).

%% comp_body(BodyList, Env, Line, State) -> {CoreBody,State}.
%% Compile a body list of expressions.

comp_body([E], Env, L, St) -> comp_expr(E, Env, L, St);
comp_body([E|Es], Env, L, St0) ->
    {Ce,St1} = comp_expr(E, Env, L, St0),
    {Ces,St2} = comp_body(Es, Env, L, St1),
    {#c_seq{anno=[L],arg=Ce,body=Ces},St2};
comp_body([], _, _, St) -> {c_nil(),St}.	%Empty body

%% comp_expr(Expr, Env, Line, State) -> {CoreExpr,State}.
%% Compile an expression.

comp_expr([_|_]=Call, Env, L, St) ->
    comp_call(Call, Env, L, St);
comp_expr([], _, _, St) -> {c_nil(),St};	%Self evaluating
comp_expr(Tup, _, _, St) when is_tuple(Tup) ->
    %% This just builds a tuple constant.
    {comp_lit(Tup),St};
comp_expr(Symb, _, _, St) when is_atom(Symb) ->
    {c_var(Symb),St};
comp_expr(Numb, _, _, St) when is_number(Numb) ->
    {comp_lit(Numb),St};
comp_expr(Bin, _, _, St) when is_binary(Bin) ->
    {comp_lit(Bin),St}.

%% comp_call(Call, Env, Line, State) -> {CoreCall,State}.
%% Handle the Core data special forms.

comp_call([quote,E], _, _, St) -> {comp_lit(E),St};
comp_call([cons,H,T], Env, L, St) ->
    Call = fun ([Ch,Ct], _, _, St) -> {c_cons(Ch, Ct),St} end,
    comp_args([H,T], Call, Env, L, St);
%%     {Ch,St1} = comp_expr(H, Env, L, St0),
%%     {Ct,St2} = comp_expr(T, Env, L, St1),
%%     {c_cons(Ch, Ct),St2};
comp_call([car,E], Env, L, St) ->		%Provide lisp names
    comp_call([hd,E], Env, L, St);
comp_call([cdr,E], Env, L, St) ->
    comp_call([tl,E], Env, L, St);
comp_call([list|Es], Env, L, St) ->
    Call = fun (Ces, _, _, St) ->
		   {foldr(fun (E, T) -> c_cons(E, T) end, c_nil(), Ces),St}
	   end,
    comp_args(Es, Call, Env, L, St);
%%     foldr(fun (E, {T,St0}) ->
%% 		  {Ce,St1} = comp_expr(E, Env, L, St0),
%% 		  {c_cons(Ce, T),St1}
%% 	  end, {c_nil(),St}, Es);
comp_call([tuple|As], Env, L, St) ->
    comp_args(As, fun (Args, _, _, St) -> {c_tuple(Args),St} end, Env, L, St);
%%     {Cas,St1} = comp_args(As, Env, L, St0),
%%     {c_tuple(Cas),St1};
comp_call([binary|Segs], Env, L, St) ->
    comp_binary(Segs, Env, L, St);		%And bitstring as well
%% Handle the Core closure special forms.
comp_call([lambda,Args|Body], Env, L, St) ->
    comp_lambda(Args, Body, Env, L, St);
comp_call(['match-lambda'|Cls], Env, L, St) ->
    comp_match_lambda(Cls, Env, L, St);
comp_call(['let',Vbs|Body], Env, L, St) ->
    comp_let(Vbs, Body, Env, L, St);
comp_call(['let-function',Fbs|Body], Env, L, St) ->
    comp_let_function(Fbs, Body, Env, L, St);
comp_call(['letrec-function',Fbs|Body], Env, L, St) ->
    comp_letrec_function(Fbs, Body, Env, L, St);
%% (let-syntax ...) should never be seen here!
%% Handle the Core control special forms.
comp_call(['progn'|Body], Env, L, St) ->
    comp_body(Body, Env, L, St);
comp_call(['if',Test,True], Env, L, St) ->
    comp_if(Test, True, [quote,false], Env, L, St);
comp_call(['if',Test,True,False], Env, L, St) ->
    comp_if(Test, True, False, Env, L, St);
comp_call(['case',Expr|Cls], Env, L, St) ->
    comp_case(Expr, Cls, Env, L, St);
comp_call(['receive'|Cls], Env, L, St0) ->
    {Ccs,Ct,Ca,St1} = rec_clauses(Cls, Env, L, St0),
    {#c_receive{anno=[L],clauses=Ccs,timeout=Ct,action=Ca},St1};
comp_call(['catch'|Body], Env, L, St0) ->
    {Cb,St1} = comp_body(Body, Env, L, St0),
    {#c_catch{anno=[L],body=Cb},St1};
comp_call(['try'|B], Env, L, St) ->
    comp_try(B, Env, L, St);
comp_call(['funcall',F|As], Env, L, St) ->
    comp_funcall(F, As, Env, L, St);
%%comp_call([call,[quote,erlang],[quote,primop]|As], Env, L, St) ->
%% An interesting thought to open up system.
comp_call([call,M,N|As], Env, L, St) ->
    %% Call a function in another module.
    Call = fun ([Cm,Cn|Cas], _, L, St) ->
		   {#c_call{anno=[L],module=Cm,name=Cn,args=Cas},St}
	   end,
    comp_args([M,N|As], Call, Env, L, St);
%%     {[Cm,Cn|Cas],St1} = comp_args([M,N|As], Env, L, St0),
%%     {#c_call{anno=[L],module=Cm,name=Cn,args=Cas},St1};
%% General function calls.
comp_call([Fun|As], Env, L, St) when is_atom(Fun) ->
    %% Fun is a symbol which is either a known BIF or function.
    Call = fun (Cas, Env, L, St) ->
		   Ar = length(Cas),
		   case get_fbinding(Fun, Ar, Env) of
		       {yes,M,F} ->				%Import
			   {#c_call{anno=[L],module=c_atom(M),
				    name=c_atom(F),args=Cas},St};
		       {yes,Name} ->
			   %% Might have been renamed, use real function name.
			   {#c_apply{anno=[L],op=c_fname(Name, Ar),
				     args=Cas},St}
		   end
	   end,
    comp_args(As, Call, Env, L, St).

%%     {Cas,St1} = comp_args(As, Env, L, St),
%%     Ar = length(As),
%%     case get_fbinding(Fun, Ar, Env) of
%% 	{yes,M,F} ->				%Import
%% 	    {#c_call{anno=[L],module=c_atom(M),name=c_atom(F),args=Cas},
%% 	     St1};
%% 	{yes,Name} ->
%% 	    %% Might have been renamed, use real function name.
%% 	    {#c_apply{anno=[L],op=c_fname(Name, Ar),args=Cas},St1}
%%     end.

%% comp_args(Args, Env, Line, State) -> {ArgList,State}.

comp_args(As, Env, L, St) ->
    mapfoldl(fun (A, Sta) -> comp_expr(A, Env, L, Sta) end, St, As).

%% comp_args(Args, CallFun, Env, Line, State) -> {Call,State}.
%%  Sequentialise the evaluation of Args building the Call at the
%%  bottom. For non-simple arguments use let to break the arg
%%  evaluation out from the main call. Cannot use foldr as we pass
%%  data both in an out.

comp_args(As, Call, Env, L, St) ->
    comp_args(As, Call, [], Env, L, St).

comp_args([A|As], Call, Cas, Env, L, St0) ->
    {Ca,St1} = comp_expr(A, Env, L, St0),
    %% Use erlang core compiler lib which does what we want.
    case core_lib:is_simple(Ca) of
	true -> comp_args(As, Call, [Ca|Cas], Env, L, St1);
	false ->
	    {Cv,St2} = new_c_var(L, St1),
	    {Rest,St3} = comp_args(As, Call, [Cv|Cas], Env, L, St2),
	    {#c_let{anno=[L],
		    vars=[Cv],
		    arg=Ca,
		    body=Rest},St3}
    end;
comp_args([], Call, Cas, Env, L, St) ->
    Call(reverse(Cas), Env, L, St).

%% comp_lambda(Args, Body, Env, Line, State) -> {#c_fun{},State}.
%% Compile a (lambda (...) ...).

comp_lambda(Args, Body, Env, L, St0) ->
    {Cvs,Pvs,St1} = comp_lambda_args(Args, L, St0),
    {Cb,St2} = comp_body(Body, add_vbindings(Pvs, Env), L, St1),
    {c_fun(Cvs, Cb, L),St2}.

comp_lambda_args(Args, L, St) ->
    foldr(fun (A, {Cvs,Pvs0,St0}) ->
		  {Cv,Pvs1,St1} = pat_symb(A, L, Pvs0, St0),
		  {[Cv|Cvs],Pvs1,St1}
	  end, {[],[],St}, Args).

lambda_arity([Args|_]) -> length(Args).

%% comp_match_lambda(Clauses, Env, Line, State) -> {#c_fun{},State}.
%% (match-lambda (Pat ...) ...).

comp_match_lambda(Cls, Env, L, St0) ->
    Ar = match_lambda_arity(Cls),
    {Cvs,St1} = new_c_vars(Ar, L, St0),
    {Ccs,St2} = comp_match_clauses(Cls, Env, L, St1),
    {Fvs,St3} = new_c_vars(Ar, L, St2),
    Cf = fail_clause(Fvs,c_tuple([c_atom(function_clause)|Fvs]), L, St3),
    Cb = #c_case{anno=[L],
		 arg=c_values(Cvs),
		 clauses=Ccs ++ [Cf]},
    {c_fun(Cvs, Cb, L),St3}.

%% match_lambda_arity(MatchClauses) -> int().

match_lambda_arity([[Pats|_]|_]) -> length(Pats).

comp_match_clauses(Cls, Env, L, St) ->
    mapfoldl(fun (Cl, Sta) -> comp_match_clause(Cl, Env, L, Sta) end,
	     St, Cls).

%% comp_match_clause(Clause, Env, L, State) -> {#c_clause{},State}.
%% (Pats [(when Guard)] . Body)
%% Pats is here a list of patterns which are the function clause
%% arguments. This must be compiled to a list of patterns not a
%% pattern with a list!

comp_match_clause([Pats|Body], Env0, L, St0) ->
    {Cps,{Pvs,St1}} = mapfoldl(fun (P, {Psvs,Sta}) ->
				       {Cp,Pvs,Stb} = comp_pat(P, L, Sta),
				       {Cp,{union(Pvs, Psvs),Stb}}
			       end, {[],St0}, Pats),
    Env1 = add_vbindings(Pvs, Env0),
    {Cg,Cb,St2} = comp_clause_body(Body, Env1, L, St1),
    {#c_clause{anno=[L],pats=Cps,guard=Cg,body=Cb},St2}.

%% comp_let(VarBindings, Body, Env, L, State) -> {#c_let{}|#c_case{},State}.
%% Compile a let expr. We are a little cunning in that we specialise
%% the the case where all the patterns are variables and there are no
%% guards, the simple case.

comp_let(Vbs, B, Env, L, St0) ->
    %% Test if this is a simple let, i.e. no matching.
    Simple = all(fun ([Pat,_]) -> is_atom(Pat);
		     (_) -> false		%Has guard
		 end, Vbs),
    case Simple of
	true ->
	    %% This is not really necessary, but fun.
	    {Cvs,Pvs,St1} = comp_lambda_args([ V || [V|_] <- Vbs ], L, St0),
	    {Ces,St2} = mapfoldl(fun ([_,E], St) -> comp_expr(E, Env, L, St) end,
				 St1, Vbs),
	    {Cb,St3} = comp_body(B, add_vbindings(Pvs, Env), L, St2),
	    {#c_let{anno=[L],
		    vars=Cvs,
		    arg=c_values(Ces),
		    body=Cb},St3};
	false ->
	    %% This would be much easier to do by building a clause
	    %% and compiling it directly. but then we would have to
	    %% build a tuple to hold all values.
	    {Cps,{Pvs,St1}} = mapfoldl(fun ([P|_], {Psvs,Sta}) ->
					       {Cp,Pvs,Stb} = comp_pat(P, L, Sta),
					       {Cp,{union(Pvs, Psvs),Stb}}
				       end, {[],St0}, Vbs),
	    Gs = foldr(fun ([_,['when',G]|_], Cgs) ->
			       %% andalso would be more efficient, but
			       %% doesn't exist here
			       ['and',G,Cgs];
			    (_, Cgs) -> Cgs end,
		       [quote,true], Vbs),
	    {Ces,St2} = mapfoldl(fun ([_,_,E], St) -> comp_expr(E, Env, L, St);
				     ([_,E], St) -> comp_expr(E, Env, L, St)
				 end, St1, Vbs),
	    Env1 = add_vbindings(Pvs, Env),
	    {Cg,St3} = comp_guard(Gs, Env1, L, St2),
	    {Cb,St4} = comp_body(B, Env1, L, St3),
	    {Cvs,St5} = new_c_vars(length(Ces), L, St4),
	    Cf = fail_clause(Cvs,
			     c_tuple([c_atom(badmatch),c_tuple(Cvs)]),
			     L, St5),
	    {#c_case{anno=[L],
		     arg=c_values(Ces),
		     clauses=[#c_clause{anno=[L],pats=Cps,guard=Cg,body=Cb},Cf]},
	     St5}
    end.

%% comp_let_function(FuncBindngs, Body, Env, Line, State) ->
%%      {#c_letrec{},State}.
%%  Compile an flet. This is complicated by the fact that Core only
%%  has letrec so we have to some name munging of the functions to
%%  avoid recursive definitions.

comp_let_function(Fbs0, B, Env0, L, St0) ->
    %% Munge names of functions. Don't use new_symb as we want to link
    %% new names to original.
    {Nfbs,St1} = mapfoldl(fun ([Old,Def], S0) ->
			      {New,S1} = new_fun_name(atom_to_list(Old), S0),
			      {{Old,New,Def},S1}
			  end, St0, Fbs0),
    %% Now compile functions in old environment.
    {Cfs,St2} = mapfoldl(fun ({_,New,Def}, St) ->
				 comp_func(New, Def, Env0, L, St)
			 end, St1, Nfbs),
    %% Add local functions Env mapping old name to new.
    Env1 = foldl(fun ({Old,New,Def}, E) ->
			 add_fbinding(Old, func_arity(Def), New, E)
		 end, Env0, Nfbs),
    {Cb,St3} = comp_body(B, Env1, L, St2),
    {#c_letrec{anno=[L],
	       defs=Cfs,
	       body=Cb},St3}.

%% comp_letrec_function(FuncBindngs, Body, Env, Line, State) ->
%%      {#c_letrec{},State}.

comp_letrec_function(Fbs, B, Env0, L, St0) ->
    %% Add local functions Env.
    Env1 = foldl(fun ([Name,Def], E) ->
			 add_fbinding(Name, func_arity(Def), Name, E)
		 end, Env0, Fbs),
    %% Now compile functions in new environment.
    {Cfs,St1} = mapfoldl(fun ([Name,Def], St) ->
				 comp_func(Name, Def, Env1, L, St)
			 end, St0, Fbs),
    {Cb,St2} = comp_body(B, Env1, L, St1),
    {#c_letrec{anno=[L],
	       defs=Cfs,
	       body=Cb},St2}.

%% func_arity(FuncDef) -> Arity.
%%  Return the arity of a function definition.

func_arity([lambda,Args|_]) -> length(Args);
func_arity(['match-lambda'|Cls]) ->
    match_lambda_arity(Cls).

%% comp_func(FuncName, FuncDef, Env, L, State) -> {{Fname,Cfun},State}.

comp_func(Name, [lambda,Args|Body], Env, L, St0) ->
    Cf = c_fname(Name, length(Args)),
    {Cfun,St1} = comp_lambda(Args, Body, Env, L, St0),
    {{Cf,Cfun},St1};
comp_func(Name, ['match-lambda'|Cls], Env, L, St0) ->
    Cf = c_fname(Name, match_lambda_arity(Cls)),
    {Cfun,St1} = comp_match_lambda(Cls, Env, L, St0),
    {{Cf,Cfun},St1}.

%% comp_if(Test, True, False, Env, Line, State) -> {#c_case{},State}.
%%  Compile in if form to a case testing the Test expression.

comp_if(Te, Tr, Fa, Env, L, St0) ->
    {Cte,St1} = comp_expr(Te, Env, L, St0),	%Test expression
    {Ctr,St2} = comp_expr(Tr, Env, L, St1),	%True expression
    {Cfa,St3} = comp_expr(Fa, Env, L, St2),	%False expression
    True = c_atom(true),
    False = c_atom(false),
    Cf = if_fail(L, St3),
    {#c_case{anno=[L],
	     arg=Cte,
	     clauses=[#c_clause{anno=[L],pats=[True],guard=True,body=Ctr},
		      #c_clause{anno=[L],pats=[False],guard=True,body=Cfa},
		      Cf]},St3}.

if_fail(L, St) ->
    Cv = c_var(omega),
    fail_clause([Cv], c_atom(if_clause), L, St).

%% fail_clause(Pats, Arg, L, State) -> Clause.
%% Build a general failure clause.

fail_clause(Pats, Arg, L, _) ->
    #c_clause{anno=[L,compiler_generated],	%It is compiler generated!
	      pats=Pats,
	      guard=c_atom(true),
	      body=c_primop(c_atom(match_fail), [Arg], L)}.

%% comp_case(Expr, Clauses, Env, Line, State) -> {#c_case{},State}.
%% Compile a case.

comp_case(E, Cls, Env, L, St0) ->
    {Ce,St1} = comp_expr(E, Env, L, St0),
    {Ccs,St2} = case_clauses(Cls, Env, L, St1),
    Cf = case_fail(L, St2),
    {#c_case{anno=[L],arg=Ce,clauses=Ccs ++ [Cf]},St2}.

case_clauses(Cls, Env, L, St) ->
    mapfoldl(fun (Cl, Sta) -> comp_clause(Cl, Env, L, Sta) end,
	     St, Cls).

case_fail(L, St) ->
    Cv = c_var(omega),
    fail_clause([Cv], c_tuple([c_atom(case_clause),Cv]), L, St).

%% rec_clauses(RecClauses, Env, Line, State) -> {Clause,Timeout,After,State}.

rec_clauses([['after',T|B]], Env, L, St0) ->
    {Ct,St1} = comp_expr(T, Env, L, St0),
    {Ca,St2} = comp_body(B, Env, L, St1),
    {[],Ct,Ca,St2};
rec_clauses([Cl|Cls], Env, L, St0) ->
    {Cc,St1} = comp_clause(Cl, Env, L, St0),
    {Ccs,Ct,Ca,St2} = rec_clauses(Cls, Env, L, St1),
    {[Cc|Ccs],Ct,Ca,St2};
rec_clauses([], _, _, St) ->
    {[],c_atom(infinity),c_atom(true),St}.

%% comp_clause(Clause, Env, Line, State) -> {#c_clause{},State}.
%%  This is a case/receive clause where the is only one pattern.

comp_clause([Pat|Body], Env0, L, St0) ->
    {Cp,Pvs,St1} = comp_pat(Pat, L, St0),
    Env1 = add_vbindings(Pvs, Env0),
    {Cg,Cb,St2} = comp_clause_body(Body, Env1, L, St1),
    {#c_clause{anno=[L],pats=[Cp],guard=Cg,body=Cb},St2}.

comp_clause_body([['when',Guard]|Body], Env, L, St0) ->
    {Cg,St1} = comp_guard(Guard, Env, L, St0),
    {Cb,St2} = comp_body(Body, Env, L, St1),
    {Cg,Cb,St2};
comp_clause_body(Body, Env, L, St0) ->
    {Cb,St1} = comp_body(Body, Env, L, St0),
    {c_atom(true),Cb,St1}.

%% comp_try(Body, Env, Line, State) -> {#c_try{},State}.
%% Compile a try. We know that case is optional but must have at least
%% one of catch or after. Complicated by the behaviour of the after
%% which means we split try with all parts into two try's.

comp_try([E|Body], Env, L, St) ->
    %% Separate try body into separate bits, none if not there.
    Case = tag_tail(Body, 'case'),
    Catch = tag_tail(Body, 'catch'),
    After = tag_tail(Body, 'after'),
    comp_try(E, Case, Catch, After, Env, L, St). %Now build the bugger

%% comp_try(Exp, Case, Catch, After, Env, L, St) -> {#c_try{},State}.

comp_try(E, Case, [], [], Env, L, St0) ->
    %% No catch or after - (try E [(case ...)])
    %% This is compiler generated.
    {Ce,St1} = comp_expr(E, Env, L, St0),
    {Cv,Cc,St2} = try_case(Case, Env, L, St1),
    {[_,Val,Info]=Evs,St3} = new_c_vars(3, L, St2), %Tag, Value, Info
    After = raise_primop([Info,Val], L, St2),
    {#c_try{anno=[L],arg=Ce,vars=[Cv],
	    body=Cc,
	    evars=Evs,
	    handler=After},St3};
comp_try(E, Case, Catch, [], Env, L, St0) ->
    %% No after - (try E [(case ...)] (catch ...))
    {Ce,St1} = comp_expr(E, Env, L, St0),
    {Cv,Cc,St2} = try_case(Case, Env, L, St1),
    {Evs,Ecs,St3} = try_exception(Catch, Env, L, St2),
    {#c_try{anno=[L],arg=Ce,vars=[Cv],
	    body=Cc,
	    evars=Evs,
	    handler=Ecs},St3};
comp_try(E, [], [], After, Env, L, St0) ->
    %% Just after - (try E (after ...))
    {Ce,St1} = comp_expr(E, Env, L, St0),
    {Cv,St2} = new_c_var(L, St1),
    {Ca,St3} = comp_body(After, Env, L, St2),
    Cb = #c_seq{anno=[L],arg=Ca,body=Cv},
    {Evs,Ecs,St4} = try_after(After, Env, L, St3),
    {#c_try{anno=[L],arg=Ce,vars=[Cv],
	    body=Cb,
	    evars=Evs,
	    handler=Ecs},St4};
comp_try(E, Case, Catch, After, Env, L, St) ->
    %% Both catch and after - (try E [(case ...)] (catch ...) (after ...))
    %% The case where all options are given.
    Try = ['try',E,['case'|Case],['catch'|Catch]],
    comp_try(Try, [], [], After, Env, L, St).

%% try_case(CaseClauses, Env, Line, State) -> {Var,#c_case{}|#c_var{},State}.
%% Case is optional, no case just returns value.

try_case([], _, L, St0) ->			%No case, just return value
    {Cv,St1} = new_c_var(L, St0),
    {Cv,Cv,St1};
try_case(Cls, Env, L, St0) ->
    {Cv,St1} = new_c_var(L, St0),
    {Ccs,St2} = case_clauses(Cls, Env, L, St1),
    Cf = try_case_fail(L, St2),
    {Cv,#c_case{anno=[L],arg=Cv,clauses=Ccs ++ [Cf]},St2}.

try_case_fail(L, St) ->
    Cv = c_var(omega),
    fail_clause([Cv], c_tuple([c_atom(try_clause),Cv]), L, St).

%% try_exception(CatchClauses, Env, L, State) -> {Vars,#c_case{},State}.

try_exception(Cls, Env, L, St0) ->
    %% Note that Tag is not needed for rethrow - it is already in Info.
    {Cvs,St1} = new_c_vars(3, L, St0),		%Tag, Value, Info
    {Ccs,St2} = case_clauses(Cls, Env, L, St1),
    [_,Val,Info] = Cvs,
    Arg = c_tuple(Cvs),
    Fc = #c_clause{anno=[L,compiler_generated],
		   pats=[Arg],
		   guard=c_atom(true),
		   body=raise_primop([Info,Val], L, St2)},
    Excp = #c_case{anno=[L],
		   arg=Arg,
		   clauses=Ccs ++ [Fc]},
    {Cvs,Excp,St2}.

%% try_after(AfterBody, Env, L, State) -> {Vars,After,State}.

try_after(B, Env, L, St0) ->
    %% Note that Tag is not needed for rethrow - it is already in Info.
    {[_,Val,Info]=Cvs,St1} = new_c_vars(3, L, St0), %Tag, Value, Info
    {Cb,St2} = comp_body(B, Env, L, St1),
    After = #c_seq{anno=[L],
		   arg=Cb,
		   body=raise_primop([Info,Val], L, St2)},
    {Cvs,After,St2}.

raise_primop(Args, L, _) ->
    c_primop(c_atom(raise), Args, L).

tag_tail([[Tag|Tail]|_], Tag) -> Tail;
tag_tail([_|Try], Tag) -> tag_tail(Try, Tag);
tag_tail([], _) -> [].

%% comp_funcall(Call, Args, Env, Line, State) -> {Core,State}.
%%  Special case if Call is directly lambda or match-lambda, convert
%%  to a let. Might be useful in macros.

comp_funcall([lambda,Las|Body]=F, As, Env, L, St) ->
    if length(Las) == length(As) ->		%Check right number of args
	    %% Convert into a let. Would like to sequentialise eval of
	    %% args here but leave that to let.
	    Vbs = zipwith(fun (V, E) -> [V,E] end, Las, As),
	    comp_let(Vbs, Body, Env, L, St);
       true ->					%Catch arg mismatch at runtime
	    comp_funcall_1(F, As, Env, L, St)
    end;
comp_funcall(['match-lambda'|Cls]=F, As, Env, L, St0) ->
    case match_lambda_arity(Cls) == length(As) of
	true ->
	    %% Expand comp_let as we need to special case body.
	    {#c_fun{vars=Cvs,body=Cb},St1} = comp_match_lambda(Cls, Env, L, St0),
	    {Ces,St2} = mapfoldl(fun (E, St) -> comp_expr(E, Env, L, St) end,
				 St1, As),
	    {#c_let{anno=[L],
		    vars=Cvs,
		    arg=c_values(Ces),
		    body=Cb},St2};
	false ->				%Catch arg mismatch at runtime
	    comp_funcall_1(F, As, Env, L, St0)
    end;
comp_funcall(F, As, Env, L, St0) ->
    comp_funcall_1(F, As, Env, L, St0).		%Naively just do it.

comp_funcall_1(F, As, Env, L, St0) ->
    {[Cf|Cas],St1} = comp_args([F|As], Env, L, St0),
    {#c_apply{anno=[L],op=Cf,args=Cas},St1}.

%% comp_binary(Segs, Env, Line, State) -> {#c_binary{},State}.

comp_binary(Segs, Env, L, St0) ->
    {Csegs,St1} = comp_bitsegs(Segs, Env, L, St0),
    {#c_binary{anno=[L],segments=Csegs},St1}.

%% comp_bitsegs(BitSegs, Env, Line, State) -> {CBitsegs,State}.

comp_bitsegs(Segs, Env, L, St0) ->
    mapfoldl(fun (S, St) -> comp_bitseg(S, Env, L, St) end, St0, Segs).

%% comp_bitseg(Bitseg, Env, Line, State) -> {#c_bitstr{},State}.

-record(spec, {type=integer,size=default,unit=default,
	       sign=default,endian=default}).

comp_bitseg([Val|Specs], Env, L, St0) ->
    {Cv,St1} = comp_expr(Val, Env, L, St0),
    {{Ty,Sz,Un,Si,En},St2} = comp_bitspecs(Specs, #spec{}, Env, L, St1),
    {c_bitseg(Cv, Sz, c_int(Un), c_atom(Ty), c_lit([Si,En])),St2};
comp_bitseg(Val, Env, L, St0) ->
    {Cv,St1} = comp_expr(Val, Env, L, St0),
    %% Create default segment.
    {{Ty,Sz,Un,Si,En},St2} = comp_bitspecs([], #spec{}, Env, L, St1),
    {c_bitseg(Cv, Sz, c_int(Un), c_atom(Ty), c_lit([Si,En])),St2}.

%% comp_bitspecs(Specs, Spec, Env, Line, State) ->
%%      {{Type,Size,Unit,Sign,End},State}.
%%  Only Size is already in Core form as it can be an expression.

comp_bitspecs(Ss, Sp0, Env, L, St0) ->
    {Sp1,St1} = foldl(fun (S, {Sp,St}) -> comp_bitspec(S, Sp, Env, L, St) end,
		      {Sp0,St0}, Ss),
    %% Adjust the values depending on type and given value.
    #spec{type=Type,size=Csize,unit=Cunit,sign=Csign,endian=Cend} = Sp1,
    case Type of
	integer ->
	    {{integer,val_or_def(Csize, c_int(8)),val_or_def(Cunit, 1),
	      val_or_def(Csign, unsigned),val_or_def(Cend, big)},St1};
	float ->
	    {{float,val_or_def(Csize, c_int(64)),val_or_def(Cunit, 1),
	      val_or_def(Csign, unsigned),val_or_def(Cend, big)},St1};
	utf8 ->					%Ignore unused fields!
	    {{utf8,c_lit(undefined),undefined,
	      val_or_def(Csign, unsigned),val_or_def(Cend, big)},St1};
	utf16 ->				%Ignore unused fields!
	    {{utf16,c_lit(undefined),undefined,
	      val_or_def(Csign, unsigned),val_or_def(Cend, big)},St1};
	utf32 ->				%Ignore unused fields!
	    {{utf32,c_lit(undefined),undefined,
	      val_or_def(Csign, unsigned),val_or_def(Cend, big)},St1};
	binary ->
	    {{binary,val_or_def(Csize, c_atom(all)),val_or_def(Cunit, 8),
	      val_or_def(Csign, unsigned),val_or_def(Cend, big)},St1};
	bitstring ->
	    {{binary,val_or_def(Csize, c_atom(all)),val_or_def(Cunit, 1),
	      val_or_def(Csign, unsigned),val_or_def(Cend, big)},St1}
    end.

%% Types.
comp_bitspec(integer, Sp, _, _, St) -> {Sp#spec{type=integer},St};
comp_bitspec(float, Sp, _, _, St) -> {Sp#spec{type=float},St};
comp_bitspec(binary, Sp, _, _, St) -> {Sp#spec{type=binary},St};
comp_bitspec(bytes, Sp, _, _, St) -> {Sp#spec{type=binary},St};
comp_bitspec(bitstring, Sp, _, _, St) -> {Sp#spec{type=bitstring},St};
comp_bitspec(bits, Sp, _, _, St) -> {Sp#spec{type=bitstring},St};
%% Unicode types.
comp_bitspec('utf-8', Sp, _, _, St) -> {Sp#spec{type=utf8},St};
comp_bitspec('utf-16', Sp, _, _, St) -> {Sp#spec{type=utf16},St};
comp_bitspec('utf-32', Sp, _, _, St) -> {Sp#spec{type=utf32},St};
%% Endianness.
comp_bitspec('big-endian', Sp, _, _, St) ->
    {Sp#spec{endian=big},St};
comp_bitspec('little-endian', Sp, _, _, St) ->
    {Sp#spec{endian=little},St};
comp_bitspec('native-endian', Sp, _, _, St) ->
    {Sp#spec{endian=native},St};
%% Sign.
comp_bitspec(signed, Sp, _, _, St) -> {Sp#spec{sign=signed},St};
comp_bitspec(unsigned, Sp, _, _, St) -> {Sp#spec{sign=unsigned},St};
%% Size.
comp_bitspec([unit,N], Sp, _, _, St) -> {Sp#spec{unit=N},St};
comp_bitspec([size,N], Sp, Env, L, St0) ->
    {Csz,St1} = comp_expr(N, Env, L, St0),
    {Sp#spec{size=Csz},St1}.

val_or_def(default, Def) -> Def;
val_or_def(V, _) -> V.

%% comp_guard(Guard, Env, Line, State) -> {Guard,State}.
%% Should really do some special handling here.

comp_guard(G, Env, L, St) ->
    comp_expr(G, Env, L, St).

%% comp_pat(Pattern, Line, Status) -> {CorePat,PatVars,State}.
%% Compile a pattern into a Core term. Handle quoted sexprs here
%% especially for symbols which then become variables instead of
%% atoms.

comp_pat(Pat, L, St) -> comp_pat(Pat, L, [], St).

comp_pat([quote,E], _, Vs, St) -> {comp_lit(E),Vs,St};
comp_pat([binary|Segs], L, Vs, St) ->
    pat_binary(Segs, L, Vs, St);
comp_pat([tuple|Ps], L, Vs0, St0) ->
    {Cps,{Vs1,St1}} = mapfoldl(fun (P, {Vsa,Sta}) ->
				       {Cp,Vsb,Stb} = comp_pat(P, L, Vsa, Sta),
				       {Cp,{Vsb,Stb}}
			       end, {Vs0,St0}, Ps),
    {c_tuple(Cps),Vs1,St1};
comp_pat(['=',P1,P2], L, Vs0, St0) ->
    %% Core can only alias against a variable so there is wotk to do!
    {Cp1,Vs1,St1} = comp_pat(P1, L, Vs0, St0),
    {Cp2,Vs2,St2} = comp_pat(P2, L, Vs0, St1),
    Cp = pat_alias(Cp1, Cp2),
    {Cp,union(Vs1, Vs2),St2};
comp_pat([H|T], L, Vs0, St0) ->
    {Ch,Vs1,St1} = comp_pat(H, L, Vs0, St0),
    {Ct,Vs2,St2} = comp_pat(T, L, Vs1, St1),
    {c_cons(Ch, Ct),Vs2,St2};
comp_pat([], _, Vs, St) -> {c_nil(),Vs,St};
%% Literals.
comp_pat(Bin, _, Vs, St) when is_bitstring(Bin) ->
    {comp_lit(Bin),Vs,St};
comp_pat(Tup, _, Vs, St) when is_tuple(Tup) ->
    {comp_lit(Tup),Vs,St};
comp_pat(Symb, L, Vs, St) when is_atom(Symb) ->
    pat_symb(Symb, L, Vs, St);			%Variable
comp_pat(Numb, _, Vs, St) when is_number(Numb) -> {c_lit(Numb),Vs,St}.

pat_symb('_', L, Vs, St0) ->			%Don't care variable.
    {Cv,St1} = new_c_var(L, St0),
    {Cv,Vs,St1};				%Not added to variables
pat_symb(Symb, _, Vs, St) ->
    {c_var(Symb),add_element(Symb, Vs),St}.

%% pat_alias(CorePat, CorePat) -> AliasPat.

%%  Normalise aliases. This has been taken from v3_core.erl in the
%%  erlang compiler. Trap bad aliases by throwing 'nomatch'.

pat_alias(#c_var{name=V1}, P2) -> #c_alias{var=#c_var{name=V1},pat=P2};
pat_alias(P1, #c_var{name=V2}) -> #c_alias{var=#c_var{name=V2},pat=P1};
pat_alias(#c_cons{}=Cons, #c_literal{anno=A,val=[H|T]}=S) ->
    pat_alias(Cons, #c_cons{anno=A,hd=#c_literal{anno=A,val=H},
			    tl=S#c_literal{val=T}});
pat_alias(#c_literal{anno=A,val=[H|T]}=S, #c_cons{}=Cons) ->
    pat_alias(#c_cons{anno=A,hd=#c_literal{anno=A,val=H},
		      tl=S#c_literal{val=T}}, Cons);
pat_alias(#c_cons{anno=A,hd=H1,tl=T1}, #c_cons{hd=H2,tl=T2}) ->
    #c_cons{anno=A,hd=pat_alias(H1, H2),tl=pat_alias(T1, T2)};
pat_alias(#c_tuple{es=Es1}, #c_tuple{es=Es2}) ->
    #c_tuple{es=pat_alias_list(Es1, Es2)};
pat_alias(#c_binary{segments=Segs1}=Bin, #c_binary{segments=Segs2}) ->
    Bin#c_binary{segments=pat_alias_list(Segs1, Segs2)};
pat_alias(#c_bitstr{val=P1,size=Sz,unit=U,type=T,flags=F}=Bitstr,
	  #c_bitstr{val=P2,size=Sz,unit=U,type=T,flags=F}) ->
    Bitstr#c_bitstr{val=pat_alias(P1, P2)};
pat_alias(#c_alias{var=V1,pat=P1}, #c_alias{var=V2,pat=P2}) ->
    if V1 =:= V2 -> pat_alias(P1, P2);
       true -> #c_alias{var=V1,pat=#c_alias{var=V2,pat=pat_alias(P1, P2)}}
    end;
pat_alias(#c_alias{var=V1,pat=P1}, P2) ->
    #c_alias{var=V1,pat=pat_alias(P1, P2)};
pat_alias(P1, #c_alias{var=V2,pat=P2}) ->
    #c_alias{var=V2,pat=pat_alias(P1, P2)};
pat_alias(P1, P2) ->
    case {core_lib:set_anno(P1, []),core_lib:set_anno(P2, [])} of
	{P,P} -> P;				%Same pattern.
	_ -> throw(nomatch)
    end.

%% pat_alias_list([A1], [A2]) -> [A].

pat_alias_list([A1|A1s], [A2|A2s]) ->
    [pat_alias(A1, A2)|pat_alias_list(A1s, A2s)];
pat_alias_list([], []) -> [];
pat_alias_list(_, _) -> throw(nomatch).

%% pat_binary(Segs, Line, PatVars, State) -> {#c_binary{},PatVars,State}.

pat_binary(Segs, L, Vs0, St0) ->
    {Csegs,Vs1,St1} = pat_bitsegs(Segs, L, Vs0, St0),
    {#c_binary{anno=[L],segments=Csegs},Vs1,St1}.

%% pat_bitsegs(Segs, Line, PatVars, State) -> {CBitsegs,PatVars,State}.

pat_bitsegs(Segs, L, Vs0, St0) ->
    {Csegs,{Vs1,St1}} =
	mapfoldl(fun (S, {Vsa,Sta}) ->
			 {Cs,Vsb,Stb} = pat_bitseg(S, L, Vsa, Sta),
			 {Cs,{Vsb,Stb}}
		 end, {Vs0,St0}, Segs),
    {Csegs,Vs1,St1}.

%% pat_bitseg(Seg, Line, PatVars, State) -> {#c_bitstr{},PatVars,State}.
%%  ??? Should noenv be new_env() instead ???
%%  ??? We know its correct so why worry? ???

pat_bitseg([Pat|Specs], L, Vs0, St0) ->
    {Cp,Vs1,St1} = comp_pat(Pat, L, Vs0, St0),
    {{Ty,Sz,Un,Si,En},St2} = comp_bitspecs(Specs, #spec{}, noenv, L, St1),
    {c_bitseg(Cp, Sz, c_int(Un), c_atom(Ty), c_lit([Si,En])),Vs1,St2};
pat_bitseg(Pat, L, Vs0, St0) ->
    {Cp,Vs1,St1} = comp_pat(Pat, L, Vs0, St0),
    %% Create default segment.
    {{Ty,Sz,Un,Si,En},St2} = comp_bitspecs([], #spec{}, noenv, L, St1),
    {c_bitseg(Cp, Sz, c_int(Un), c_atom(Ty), c_lit([Si,En])),Vs1,St2}.

%% c_fun(Vars, Body, Line) -> #c_fun{}.
%% c_primop(Name, Args, Line) -> #c_primop{}.
%% c_fname(Name, Arity) -> #c_fname{}.
%% c_values(Values) -> #c_values{}.
%% c_cons(Head, Tail) -> #c_cons{}.
%% c_tuple(Elements) -> #c_tuple{}.
%% c_atom(Value) -> #c_literal{}.
%% c_int(Value) -> #c_literal{}.
%% c_float(Value) -> #c_literal{}.
%% c_nil() -> #c_literal{}.
%% c_lit(Value) -> #c_literal{}.
%% c_var(Name) -> #c_var{}.
%% c_bitseg(Value, Size, Unit, Type, Sign, Endian) -> #c_bitseg{}.

c_fun(Vs, B, L) -> #c_fun{anno=[L],vars=Vs,body=B}.
c_primop(N, As, L) ->
    #c_primop{anno=[L],name=N,args=As}.
%% R12B/R13B fix, choose one of following depending on version.
%%c_fname(N, A) -> #c_fname{anno=[],id=N,arity=A}.	%R12B
c_fname(N, A) -> #c_var{anno=[],name={N,A}}.		%R13B
c_values([V]) -> V;				%An optimisation
c_values(Vs) -> #c_values{anno=[],es=Vs}.
c_atom(A) -> #c_literal{anno=[],val=A}.
c_int(I) -> #c_literal{anno=[],val=I}.
c_float(F) -> #c_literal{anno=[],val=F}.
c_nil() -> #c_literal{anno=[],val=[]}.
c_lit(Val) -> #c_literal{anno=[],val=Val}.	%Generic literal
c_cons(Hd, Tl) -> #c_cons{anno=[],hd=Hd,tl=Tl}.
c_tuple(Es) -> #c_tuple{anno=[],es=Es}.
c_var(N) -> #c_var{anno=[],name=N}.
c_bitseg(Val, Sz, Un, Ty, Fs) ->
    #c_bitstr{anno=[],val=Val,size=Sz,unit=Un,type=Ty,flags=Fs}.

%% comp_lit(Value) -> LitExpr.
%%  Make a literal expression from an Erlang value. Try to make it as
%%  literal as possible. This function will fail if the value is not
%%  expressable as a literal (for instance, a pid).

comp_lit([H0|T0]) ->
    case {comp_lit(H0),comp_lit(T0)} of
	{#c_literal{val=H},#c_literal{val=T}} ->
	    c_lit([H|T]);
	{H,T} -> c_cons(H, T)
    end;
comp_lit([]) -> c_nil();
comp_lit(T) when is_tuple(T) ->
    Es = comp_lit_list(tuple_to_list(T)),
    case is_lit_list(Es) of
	true -> c_lit(list_to_tuple(concrete_list(Es)));
	false -> c_tuple(Es)
    end;
comp_lit(A) when is_atom(A) -> c_atom(A);
comp_lit(I) when is_integer(I) -> c_int(I);
comp_lit(F) when is_float(F) -> c_float(F);
comp_lit(Bin) when is_bitstring(Bin) ->
    Bits = comp_lit_bitsegs(Bin),
    #c_binary{anno=[],segments=Bits}.

comp_lit_list(Vals) -> [ comp_lit(V) || V <- Vals ].

is_lit_list(Es) -> all(fun (E) -> is_record(E, c_literal) end, Es).

concrete_list([#c_literal{val=V}|T]) -> [V|concrete_list(T)];
concrete_list([]) -> [].

comp_lit_bitsegs(<<B:8,Bits/bitstring>>) ->	%Next byte
    [c_byte_bitseg(B, 8)|comp_lit_bitsegs(Bits)];
comp_lit_bitsegs(<<>>) -> [];			%Even bytes
comp_lit_bitsegs(Bits) ->			%Size < 8
    N = bit_size(Bits),
    <<B:N>> = Bits,
    [c_byte_bitseg(B, N)].

c_byte_bitseg(B, Sz) ->
    c_bitseg(c_lit(B), c_int(Sz), c_int(1), c_atom(integer),
	     c_lit([unsigned,big])).

%% new_symb(State) -> {Symbol,State}.
%% Create a hopefully new unused symbol.

%% new_symb(St) ->
%%     C = St#cg.vc,
%%     {list_to_atom("|=" ++ integer_to_list(C) ++ "=|"),St#cg{vc=C+1}}.

new_fun_name(Pre, St) ->
    C = St#cg.fc,
    {list_to_atom("'" ++ Pre ++ "~" ++ integer_to_list(C)),St#cg{fc=C+1}}.

%% new_c_var(Line, State) -> {#c_var{},State}.
%% Create a hopefully new core variable.

new_c_var(_, St) ->
    C = St#cg.vc,
    Name = list_to_atom(integer_to_list(C)),
    {c_var(Name),St#cg{vc=C+1}}.

new_c_vars(N, L, St) -> new_c_vars(N, L, St, []).

new_c_vars(N, L, St0, Vs) when N > 0 ->
    {V,St1} = new_c_var(L, St0),
    new_c_vars(N-1, L, St1, [V|Vs]);
new_c_vars(0, _, St, Vs) -> {Vs,St}.

add_vbindings(Vs, Env) ->
    foldl(fun (V, E) -> add_vbinding(V, dummy, E) end, Env, Vs).

%% safe_fetch(Key, Dict, Default) -> Value.

safe_fetch(Key, D, Def) ->
    case find(Key, D) of
	{ok,Val} -> Val;
	error -> Def
    end.
