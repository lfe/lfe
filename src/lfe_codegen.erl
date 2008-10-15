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
		concat/1]).
-import(ordsets, [add_element/2,is_element/2,from_list/1,union/2]).
-import(orddict, [store/3,find/2]).

-import(lfe_lib, [new_env/0,add_env/2,
		  add_vbinding/3,vbinding/2,fbinding/3,add_fbinding/4,
		  add_ibinding/5,gbinding/3]).

-include_lib("compiler/src/core_parse.hrl").

-record(cg, {opts=[],				%Options
	     vc=0,				%Variable counter
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
	       [quote,St1#cg.mod]]],0},
	    {module_info,
	     [lambda,[x],
	      [call,[quote,erlang],[quote,get_module_info],
	       [quote,St1#cg.mod],x]],0}|
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
		       {core_lib:make_literal(N),core_lib:make_literal(V)}
	       end, St2#cg.atts),
    %% Compile the functions.
    {Cdefs,St3} = mapfoldl(fun (D, St) -> comp_define(D, Env1, St) end,
			  St2, St2#cg.defs),
    %% Build the final core module structure.
    Core1 = Core0#c_module{name=c_lit(St3#cg.mod, 1),
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
    debug_print("#core: ~p\n", [Core1], St3),
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
    map(fun ({F,Def,_}) -> c_fname(F, func_arity(Def), 1) end, Fbs);
make_exports(Exps, _) ->
    map(fun ({F,A}) -> c_fname(F, A, 1) end, Exps).

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
collect_mdef([[N,V]|Mdef], St) ->
    As = St#cg.atts ++ [{N,V}],		%Probably not many
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
    Cf = c_fname(Name, func_arity(Def), L),	%Could be useful
    comp_func(Name, Def, Env, L, St#cg{func=Cf}).

%% comp_body(BodyList, Env, Line, State) -> {CoreBody,State}.
%% Compile a body list of expressions.

comp_body([E], Env, L, St) -> comp_expr(E, Env, L, St);
comp_body([E|Es], Env, L, St0) ->
    {Ce,St1} = comp_expr(E, Env, L, St0),
    {Ces,St2} = comp_body(Es, Env, L, St1),
    {#c_seq{anno=[L],arg=Ce,body=Ces},St2};
comp_body([], _, L, St) -> {c_nil(L),St}.	%Empty body
    
%% comp_expr(Expr, Env, Line, State) -> {CoreExpr,State}.
%% Compile an expression.

comp_expr([_|_]=Call, Env, L, St) ->
    comp_call(Call, Env, L, St);
comp_expr([], _, L, St) -> {c_nil(L),St};	%Self evaluating
comp_expr(Tup, _, _, St) when is_tuple(Tup) ->
    %% This just builds a tuple constant.
    {core_lib:make_literal(Tup),St};
comp_expr(Symb, _, L, St) when is_atom(Symb) ->
    {c_var(Symb, L),St};
comp_expr(Numb, _, _, St) when is_number(Numb) ->
    {core_lib:make_literal(Numb),St};
comp_expr(Bin, _, _, St) when is_binary(Bin) ->
    {core_lib:make_literal(Bin),St}.

%% comp_call(Call, Env, Line, State) -> {CoreCall,State}.
%% Handle the Core data special forms.

comp_call([quote,E], _, _, St) -> {core_lib:make_literal(E),St};
comp_call([cons,H,T], Env, L, St0) ->
    {Ch,St1} = comp_expr(H, Env, L, St0),
    {Ct,St2} = comp_expr(T, Env, L, St1),
    {c_cons(Ch, Ct, L),St2};
comp_call([car,E], Env, L, St) ->		%Provide lisp names
    comp_call([hd,E], Env, L, St);
comp_call([cdr,E], Env, L, St) ->
    comp_call([tl,E], Env, L, St);
comp_call([list|Es], Env, L, St) ->
    foldr(fun (E, {T,St0}) ->
		  {Ce,St1} = comp_expr(E, Env, L, St0),
		  {c_cons(Ce, T, L),St1}
	  end, {c_nil(L),St}, Es);
comp_call([tuple|As], Env, L, St0) ->
    {Cas,St1} = comp_args(As, Env, L, St0),
    sequentialise_args(Cas, fun (Args, _, L, St) ->
				    {c_tuple(Args, L),St}
			    end, [], Env, L, St1);
%%     {Cas,St1} = comp_args(As, Env, L, St0),
%%     {c_tuple(Cas, L),St1};
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
comp_call(['funcall',F|As], Env, L, St0) ->
    {Cf,St1} = comp_expr(F, Env, L, St0),
    {Cas,St2} = comp_args(As, Env, L, St1),
    {#c_apply{anno=[L],op=Cf,args=Cas},St2};
%%comp_call([call,[quote,erlang],[quote,primop]|As], Env, L, St) ->
%% An interesting thought to open up system.
comp_call([call,M,N|As], Env, L, St0) ->
    %% Call a function in another module.
    {Cm,St1} = comp_expr(M, Env, L, St0),
    {Cn,St2} = comp_expr(N, Env, L, St1),
    {Cas,St3} = comp_args(As, Env, L, St2),
    {#c_call{anno=[L],module=Cm,name=Cn,args=Cas},St3};
%% General function calls.
comp_call([Fun|As], Env, L, St0) when is_atom(Fun) ->
    %% Fun is a symbol which is either a known BIF or function.
    {Cas,St1} = comp_args(As, Env, L, St0),
    Call = fun (Args, Env, L, St) ->
		   Ar = length(Args),
		   case fbinding(Fun, Ar, Env) of
		       {yes,M,F} ->				%Import
			   {#c_call{anno=[L],module=c_lit(M, L),
				    name=c_lit(F, L),args=Args},
			    St};
		       {yes,Name} ->
			   %% Might have been renamed, use real function name.
			   {#c_apply{anno=[L],op=c_fname(Name, Ar, L),
				     args=Args},St}
		   end
	   end,
    sequentialise_args(Cas, Call, [], Env, L, St1).
    
%%     Ar = length(As),
%%     case fbinding(Fun, Ar, Env) of
%% 	{yes,M,F} ->				%Import
%% 	    {#c_call{anno=[L],module=c_lit(M, L),name=c_lit(F, L),args=Cas},
%% 	     St1};
%% 	{yes,Name} ->
%% 	    %% Might have been renamed, use real function name.
%% 	    {#c_apply{anno=[L],op=c_fname(Name, Ar, L),args=Cas},St1}
%%     end.

%% sequentialise_args(CompiledArgs, Call, Env, Line, State) ->
%%      {CoreCall,State}.

sequentialise_args([Ca|Cas], Call, Sas, Env, L, St0) ->
    %% Use erlang core compiler lib which does what we want.
    case core_lib:is_simple(Ca) of
	true -> sequentialise_args(Cas, Call, [Ca|Sas], Env, L, St0);
	false ->
	    {Var,St1} = new_c_var(L, St0),
	    {Rest,St2} = sequentialise_args(Cas, Call, [Var|Sas], Env, L, St1),
	    {#c_let{anno=[L],
		    vars=[Var],
		    arg=Ca,
		    body=Rest},St2}
    end;
sequentialise_args([], Call, Sas, Env, L, St) ->
    Call(reverse(Sas), Env, L, St).

%% comp_args(Args, Env, Line, State) -> {ArgList,State}.

comp_args(As, Env, L, St) ->
    mapfoldl(fun (A, Sta) -> comp_expr(A, Env, L, Sta) end, St, As).

%% comp_lambda(Args, Body, Env, Line, State) -> {#c_fun{},State}.
%% Compile a (lambda (...) ...).

comp_lambda(Args, Body, Env, L, St0) ->
    Pvs = foldl(fun (A, Pvs) -> add_element(A, Pvs) end, [], Args),
    Cvs = map(fun (A) -> c_var(A, L) end, Args),
    {Cb,St1} = comp_body(Body, add_vbindings(Pvs, Env), L, St0),
    {c_fun(Cvs, Cb, L),St1}.

lambda_arity([Args|_]) -> length(Args).

%% comp_match_lambda(Clauses, Env, Line, State) -> {#c_fun{},State}.
%% (match-lambda (Pat ...) ...).

comp_match_lambda(Cls, Env, L, St0) ->
    {Cvs,St1} = new_c_vars(match_lambda_arity(Cls), L, St0),
    {Ccs,St2} = comp_match_clauses(Cls, Env, L, St1),
    {Fvs,St3} = new_c_vars(length(hd(hd(Cls))), L, St2),
    Cf = fail_clause(Fvs,c_tuple([c_lit(function_clause, L)|Fvs],L), L, St3),
    Cb = #c_case{anno=[L],
		 arg=c_values(Cvs, L),
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

%% comp_let(VarBindings, Bory, Env, L, State) -> {#c_let{}|#c_case{},State}.
%% Compile a let expr. We are a little cunning in that we specialise
%% the the case where all the patterns are variables and there a re no
%% guards, the simple case.

comp_let(Vbs, B, Env, L, St0) ->
    %% Test if this is a simple let, i.e. no matching.
    Simple = all(fun ([Pat,_]) -> is_atom(Pat);
		     (_) -> false		%Has guard
		 end, Vbs),
    case Simple of
	true ->
	    %% This is not really necessary, but fun.
	    Pvs = foldl(fun ([V|_], Pvs) -> add_element(V, Pvs) end, [], Vbs),
	    Cvs = map(fun ([V|_]) -> c_var(V, L) end, Vbs),
	    {Ces,St1} = mapfoldl(fun ([_,E], St) -> comp_expr(E, Env, L, St) end,
				 St0, Vbs),
	    {Cb,St2} = comp_body(B, add_vbindings(Pvs, Env), L, St1),
	    {#c_let{anno=[L],
		    vars=Cvs,
		    arg=c_values(Ces, L),
		    body=Cb},St2};
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
			     c_tuple([c_lit(badmatch, L),c_tuple(Cvs, L)],L),
			     L, St5),
	    {#c_case{anno=[L],
		     arg=c_values(Ces, L),
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
    Cf = c_fname(Name, length(Args), L),
    {Cfun,St1} = comp_lambda(Args, Body, Env, L, St0),
    {{Cf,Cfun},St1};
comp_func(Name, ['match-lambda'|Cls], Env, L, St0) ->
    Cf = c_fname(Name, match_lambda_arity(Cls), L),
    {Cfun,St1} = comp_match_lambda(Cls, Env, L, St0),
    {{Cf,Cfun},St1}.

%% comp_if(Test, True, False, Env, Line, State) -> {#c_case{},State}.
%%  Compile in if form to a case testing the Test expression.

comp_if(Te, Tr, Fa, Env, L, St0) ->
    {Cte,St1} = comp_expr(Te, Env, L, St0),	%Test expression
    {Ctr,St2} = comp_expr(Tr, Env, L, St1),	%True expression
    {Cfa,St3} = comp_expr(Fa, Env, L, St2),	%False expression
    True = c_lit(true, L),
    False = c_lit(false, L),
    Cf = if_fail(L, St3),
    {#c_case{anno=[L],
	     arg=Cte,
	     clauses=[#c_clause{anno=[L],pats=[True],guard=True,body=Ctr},
		      #c_clause{anno=[L],pats=[False],guard=True,body=Cfa},
		      Cf]},St3}.

if_fail(L, St) ->
    Cv = c_var(omega, L),
    fail_clause([Cv], c_lit(if_clause, L), L, St).

%% fail_clause(Pats, Arg, L, State) -> Clause.
%% Build a general failure clause.

fail_clause(Pats, Arg, L, _) ->
    #c_clause{anno=[L,compiler_generated],	%It is compiler generated!
	      pats=Pats,
	      guard=c_lit(true, L),
	      body=c_primop(c_lit(match_fail, L), [Arg], L)}.

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
    Cv = c_var(omega, L),
    fail_clause([Cv], c_tuple([c_lit(case_clause, L),Cv],L), L, St).

%% rec_clauses(RecClauses, Env, Line, State) -> {Clause,Timeout,After,State}.

rec_clauses([['after',T|B]], Env, L, St0) ->
    {Ct,St1} = comp_expr(T, Env, L, St0),
    {Ca,St2} = comp_body(B, Env, L, St1),
    {[],Ct,Ca,St2};
rec_clauses([Cl|Cls], Env, L, St0) ->
    {Cc,St1} = comp_clause(Cl, Env, L, St0),
    {Ccs,Ct,Ca,St2} = rec_clauses(Cls, Env, L, St1),
    {[Cc|Ccs],Ct,Ca,St2};
rec_clauses([], _, L, St) ->
    {[],c_lit(infinity, L),c_lit(true, L),St}.

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
    {c_lit(true, L),Cb,St1}.

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
    {Fv,St3} = new_c_var(L, St2),
    Cf = fail_clause([Fv], c_tuple([c_lit(try_clause, L),Fv], L), L, St3),
    {Cv,#c_case{anno=[L],arg=Cv,clauses=Ccs ++ [Cf]},St3}.

%% try_exception(CatchClauses, Env, L, State) -> {Vars,#c_case{},State}.

try_exception(Cls, Env, L, St0) ->
    %% Note that Tag is not needed for rethrow - it is already in Info.
    {Cvs,St1} = new_c_vars(3, L, St0),		%Tag, Value, Info
    {Ccs,St2} = case_clauses(Cls, Env, L, St1),
    [_,Val,Info] = Cvs,
    Arg = c_tuple(Cvs, L),
    Fc = #c_clause{anno=[L,compiler_generated],
		   pats=[Arg],
		   guard=c_lit(true, L),
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
    c_primop(c_lit(raise, L), Args, L).

tag_tail([[Tag|Tail]|_], Tag) -> Tail;
tag_tail([_|Try], Tag) -> tag_tail(Try, Tag);
tag_tail([], _) -> [].

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
    {c_bitseg(Cv, Sz, Un, Ty, Si, En, L),St2};
comp_bitseg(Val, Env, L, St0) ->
    {Cv,St1} = comp_expr(Val, Env, L, St0),
    %% Create default segment.
    {{Ty,Sz,Un,Si,En},St2} = comp_bitspecs([], #spec{}, Env, L, St1),
    {c_bitseg(Cv, Sz, Un, Ty, Si, En, L),St2}.

c_bitseg(Val, Sz, Un, Ty, Si, En, L) ->
    #c_bitstr{anno=[L],val=Val,size=Sz,unit=Un,type=Ty,
		      flags=c_cons(Si, c_cons(En, c_nil(L), L), L)}.

comp_bitspecs([[size,N]|Ss], Sp, Env, L, St0) ->
    {Csz,St1} = comp_expr(N, Env, L, St0),
    comp_bitspecs(Ss, Sp#spec{size=Csz}, Env, L, St1);
comp_bitspecs([[unit,N]|Ss], Sp, Env, L, St) ->
    comp_bitspecs(Ss, Sp#spec{unit=c_lit(N, L)}, Env, L, St);
comp_bitspecs([integer|Ss], Sp, Env, L, St) ->
    comp_bitspecs(Ss, Sp#spec{type=integer}, Env, L, St);
comp_bitspecs([float|Ss], Sp, Env, L, St) ->
    comp_bitspecs(Ss, Sp#spec{type=float}, Env, L, St);
comp_bitspecs([binary|Ss], Sp, Env, L, St) ->
    comp_bitspecs(Ss, Sp#spec{type=binary}, Env, L, St);
comp_bitspecs([bitstring|Ss], Sp, Env, L, St) ->
    comp_bitspecs(Ss, Sp#spec{type=bitstring}, Env, L, St);
comp_bitspecs([signed|Ss], Sp, Env, L, St) ->
    comp_bitspecs(Ss, Sp#spec{sign=c_lit(signed, L)}, Env, L, St);
comp_bitspecs([unsigned|Ss], Sp, Env, L, St) ->
    comp_bitspecs(Ss, Sp#spec{sign=c_lit(unsigned, L)}, Env, L, St);
comp_bitspecs(['big-endian'|Ss], Sp, Env, L, St) ->
    comp_bitspecs(Ss, Sp#spec{endian=c_lit(big, L)}, Env, L, St);
comp_bitspecs(['little-endian'|Ss], Sp, Env, L, St) ->
    comp_bitspecs(Ss, Sp#spec{endian=c_lit(little, L)}, Env, L, St);
comp_bitspecs(['native-endian'|Ss], Sp, Env, L, St) ->
    comp_bitspecs(Ss, Sp#spec{endian=c_lit(native, L)}, Env, L, St);
comp_bitspecs([],
	      #spec{type=Type,size=Csize,unit=Cunit,sign=Csign,endian=Cend},
	      _, L, St) ->
    %% Adjust the values depending on type and given value.
    case Type of
	integer ->
	    {{c_lit(integer, L),
	      val_or_def(Csize, 8, L),val_or_def(Cunit, 1, L),
	      val_or_def(Csign, unsigned, L),val_or_def(Cend, big, L)},St};
	float ->
	    {{c_lit(float, L),
	      val_or_def(Csize, 64, L),val_or_def(Cunit, 1, L),
	      val_or_def(Csign, unsigned, L),val_or_def(Cend, big, L)},St};
	binary ->
	    {{c_lit(binary, L),
	      val_or_def(Csize, all, L),val_or_def(Cunit, 8, L),
	      val_or_def(Csign, unsigned, L),val_or_def(Cend, big, L)},St};
	bitstring ->
	    {{c_lit(binary, L),
	      val_or_def(Csize, all, L),val_or_def(Cunit, 1, L),
	      val_or_def(Csign, unsigned, L),val_or_def(Cend, big, L)},St}
    end.

val_or_def(default, Def, L) -> c_lit(Def, L);
val_or_def(V, _, _) -> V.

%% comp_guard(Guard, Env, Line, State) -> {Guard,State}.
%% Should really do some special handling here.

comp_guard(G, Env, L, St) ->
    comp_expr(G, Env, L, St).

%% comp_pat(Pattern, Line, Status) -> {CorePat,PatVars,State}.
%% Compile a pattern into a Core term. Handle quoted sexprs here
%% especially for symbols which then become variables instead of
%% atoms.

comp_pat(Pat, L, St) -> comp_pat(Pat, L, [], St).

comp_pat([quote,E], _, Vs, St) -> {core_lib:make_literal(E),Vs,St};
comp_pat([binary|Segs], L, Vs, St) ->
    pat_binary(Segs, L, Vs, St);
comp_pat([tuple|Ps], L, Vs0, St0) ->
    {Cps,{Vs1,St1}} = mapfoldl(fun (P, {Vsa,Sta}) ->
				       {Cp,Vsb,Stb} = comp_pat(P, L, Vsa, Sta),
				       {Cp,{Vsb,Stb}}
			       end, {Vs0,St0}, Ps),
    {c_tuple(Cps, L),Vs1,St1};
comp_pat(['=',P1,P2], L, Vs0, St0) ->
    %% Core can only alias against a variable so there is wotk to do!
    {Cp1,Vs1,St1} = comp_pat(P1, L, Vs0, St0),
    {Cp2,Vs2,St2} = comp_pat(P2, L, Vs0, St1),
    Cp = pat_alias(Cp1, Cp2),
    {Cp,union(Vs1, Vs2),St2};
comp_pat([H|T], L, Vs0, St0) ->
    {Ch,Vs1,St1} = comp_pat(H, L, Vs0, St0),
    {Ct,Vs2,St2} = comp_pat(T, L, Vs1, St1),
    {c_cons(Ch, Ct, L),Vs2,St2};
comp_pat([], L, Vs, St) -> {c_nil(L),Vs,St};
comp_pat(Tup, _, Vs, St) when is_tuple(Tup) ->
    {core_lib:make_literal(Tup),Vs,St};
comp_pat(Symb, L, Vs, St) when is_atom(Symb) ->
    pat_symb(Symb, L, Vs, St);			%Variable
comp_pat(Numb, L, Vs, St) when is_number(Numb) -> {c_lit(Numb, L),Vs,St}.

pat_symb('_', L, Vs, St0) ->			%Don't care variable.
    {Cv,St1} = new_c_var(L, St0),
    {Cv,Vs,St1};				%Not added to variables
pat_symb(Symb, L, Vs, St) ->
    {c_var(Symb, L),add_element(Symb, Vs),St}.

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
    {Csegs,{Vs1,St1}} = mapfoldl(fun (S, {Vsa,Sta}) ->
					 {Cs,Vsb,Stb} =
					     pat_bitseg(S, L, Vsa, Sta),
					 {Cs,{Vsb,Stb}}
				 end, {Vs0,St0}, Segs),
    {Csegs,Vs1,St1}.

%% pat_bitseg(Seg, Line, PatVars, State) -> {#c_bitstr{},PatVars,State}.
%%  ??? Should noenv be new_env() instead ???
%%  ??? We know its correct so why worry? ???

pat_bitseg([Pat|Specs], L, Vs0, St0) ->
    {Cv,Vs1,St1} = comp_pat(Pat, L, Vs0, St0),
    {{Ty,Sz,Un,Si,En},St2} = comp_bitspecs(Specs, #spec{}, noenv, L, St1),
    {c_bitseg(Cv, Sz, Un, Ty, Si, En, L),Vs1,St2};
pat_bitseg(Pat, L, Vs0, St0) ->
    {Cv,Vs1,St1} = comp_pat(Pat, L, Vs0, St0),
    %% Create default segment.
    {{Ty,Sz,Un,Si,En},St2} = comp_bitspecs([], #spec{}, noenv, L, St1),
    {c_bitseg(Cv, Sz, Un, Ty, Si, En, L),Vs1,St2}.

%% c_fun(Vars, Body, Line) -> #c_fun{}.
%% c_fname(Name, Arity, Line) -> #c_fname{}.
%% c_values(Values, Line) -> #c_values{}.
%% c_cons(Head, Tail, Line) -> #c_cons{}.
%% c_nil(Line) -> #c_literal{}.
%% c_tuple(Elements, Line) -> #c_tuple{}.
%% c_lit(Value, Line) -> #c_literal{}.
%% c_var(Name, Line) -> #c_var{}.
%% c_primop(Name, Args, Line) -> #c_primop{}.

c_fun(Vs, B, L) -> #c_fun{anno=[L],vars=Vs,body=B}.
c_fname(N, A, L) -> #c_fname{anno=[L],id=N,arity=A}.
c_values([V], _) -> V;				%An optimisation
c_values(Vs, L) -> #c_values{anno=[L],es=Vs}.
c_cons(Hd, Tl, L) -> #c_cons{anno=[L],hd=Hd,tl=Tl}.
c_nil(_) -> #c_literal{anno=[],val=[]}.
c_tuple(Es, L) -> #c_tuple{anno=[L],es=Es}.
c_lit(Val, _) -> #c_literal{anno=[],val=Val}.	%Enough with line numbers
c_var(N, _) -> #c_var{anno=[],name=N}.
c_primop(N, As, L) ->
    #c_primop{anno=[L],name=N,args=As}.

%% new_symb(State) -> {Symbol,State}.
%% Create a hopefully new unused symbol.

%% new_symb(St) ->
%%     C = St#cg.vc,
%%     {list_to_atom("|=" ++ integer_to_list(C) ++ "=|"),St#cg{vc=C+1}}.

new_fun_name(Pre, St) ->
    C = St#cg.vc,
    {list_to_atom("'" ++ Pre ++ "~" ++ integer_to_list(C)),St#cg{vc=C+1}}.

%% new_c_var(Line, State) -> {#c_var{},State}.
%% Create a hopefully new core variable.

new_c_var(L, St) ->
    C = St#cg.vc,
    Name = list_to_atom(integer_to_list(C)),
    {c_var(Name, L),St#cg{vc=C+1}}.

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
