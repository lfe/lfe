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

%% File    : lfe_lint.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang syntax checker.

-module(lfe_lint).

-export([form/1,module/1,module/2,format_error/1]).

-import(lfe_lib, [new_env/0,is_vbound/2,is_fbound/3,is_gbound/3,
		  vbinding/2,fbinding/3,gbinding/3,
		  add_vbinding/3,add_fbinding/4,add_ibinding/5,
		  is_erl_bif/2,is_guard_bif/2,
		  is_symb_list/1,is_proper_list/1]).

%% -compile(export_all).

-import(lists, [member/2,sort/1,all/2,foldl/3,foldr/3,foreach/2,mapfoldl/3]).
-import(ordsets, [add_element/2,from_list/1,is_element/2,
		  union/1,union/2,intersection/2,subtract/2]).
-import(orddict, [store/3,find/2]).

-record(lint, {module=[],			%Module name
	       pars=none,			%Module parameters
	       extd=[],				%Extends
	       exps=[],				%Exports
	       imps=[],				%Imports
	       pref=[],				%Prefixes
	       funcs=[],			%Defined functions
	       env=[],				%Top-level environment
	       errors=[],			%Errors
	       warnings=[],			%Warnings
	       line=[],				%Current line
	       func=[]}).			%Current function

form(F) ->
    module([{['define-module',dummy],1},
	    {F,2}]).

%% Errors.
format_error(bad_mod_def) -> "bad module definition";
format_error(bad_extends) -> "bad extends";
format_error(missing_module) -> "missing module";
format_error(bad_funcs) -> "bad function list";
format_error(bad_body) -> "bad body";
format_error(bad_clause) -> "bad clause";
format_error(bad_args) -> "bad arguments";
format_error({bad_attribute,A}) ->
    lfe_io:format1("bad attribute: ~w", [A]);
format_error({bad_form,Type}) ->
    lfe_io:format1("bad form: ~w", [Type]);
format_error({unbound_symb,S}) ->
    lfe_io:format1("unbound symbol: ~w", [S]);
format_error({unbound_func,F}) ->
    lfe_io:format1("unbound function: ~w", [F]);
format_error({multi_var,S}) ->
    lfe_io:format1("multiple variable: ~w", [S]);
format_error({redef_fun,F}) ->
    lfe_io:format1("redefining function: ~w", [F]);
format_error(illegal_pat) -> "illegal pattern";
format_error(illegal_guard) -> "illegal guard";
format_error(illegal_bitseg) -> "illegal bit segment";
format_error({illegal_bitspec,S}) ->
    lfe_io:format1("illegal bit specification: ~w", [S]);
format_error(unknown_form) -> "unknown form".

%% module(Forms) -> {ok,[Warning]} | {error,[Error],[Warning]}.
%% module(Forms, Options) -> {ok,[Warning]} | {error,[Error],[Warning]}.

module(Fs) -> module(Fs, []).

module(Fs0, Opts) ->
    %% Predefined functions
    St0 = #lint{},
    %% Collect forms and fill in module infor in state.
    {Fs1,St1} = lfe_lib:proc_forms(fun collect_form/3, Fs0, St0),
    St2 = check_module(Fs1, St1),
    debug_print("#lint: ~p\n", [St2], Opts),
    return_status(St2).

debug_print(Format, Args, Opts) ->
    case member(debug_print, Opts) of
	true -> io:fwrite(Format, Args);
	false -> ok
    end.

return_status(#lint{errors=[]}=St) ->
    {ok,St#lint.warnings};
return_status(St) ->
    {error,St#lint.errors,St#lint.warnings}.

%% collect_form(Form, Line, State) -> {[Ret],State}.
%%  Collect valid forms and module data. Returns forms and put module
%%  data into state. Flag unknown forms and define-module not first.

collect_form(['define-module',Mod|Mdef], L, St0) ->
    %% Check normal module or parameterised module.
    case is_symb_list(Mod) of			%Parameterised module
	true ->
	    {Vs,St1} = check_lambda_args(tl(Mod), L, St0),
	    %% Everything into State.
	    {[],check_mdef(Mdef, L, St1#lint{module=hd(Mod),pars=Vs})};
	false when is_atom(Mod) ->		%Normal module
	    %% Everything into State.
	    {[],check_mdef(Mdef, L, St0#lint{module=Mod,pars=none})};
	false ->				%Bad module name
	    {[],add_error(L, bad_mod_def, St0)}
    end;
collect_form(_, L, #lint{module=[]}=St) ->
    %% Set module name so this only triggers once.
    {[],add_error(L, bad_mod_def, St#lint{module='-no-module-'})};
collect_form(['define-function',Func,[lambda|_]=Lambda], L, St)
  when is_atom(Func) ->
    {[{Func,Lambda,L}],St};
collect_form(['define-function',Func,['match-lambda'|_]=Match], L, St)
  when is_atom(Func) ->
    {[{Func,Match,L}],St};
collect_form(_, L, St) ->
    {[],add_error(L, unknown_form, St)}.
    
check_mdef([[export,all]|Mdef], L, St) ->	%Pass 'all' along
    check_mdef(Mdef, L, St#lint{exps=all});
check_mdef([[export|Es]|Mdef], L, St) ->
    case is_flist(Es) of
	{yes,Fs} ->
	    Exps = add_exports(St#lint.exps, Fs),
	    check_mdef(Mdef, L, St#lint{exps=Exps});
	no ->
	    check_mdef(Mdef, L, bad_form_error(L, export, St))
    end;
check_mdef([[import|Is]|Mdef], L, St0) ->
    St1 = check_imports(Is, L, St0),
    check_mdef(Mdef, L, St1);
check_mdef([[extends,M]|Mdef], L, St) ->
    if is_atom(M) -> 
	    check_mdef(Mdef, L, St#lint{extd=M});
       true ->
	    check_mdef(Mdef, L, add_error(L, bad_extends, St))
    end;
check_mdef([[Name|Vals]|Mdef], L, St) ->
    %% Other attributes, must be list and have symbol name.
    case is_atom(Name) and is_proper_list(Vals) of
	true -> check_mdef(Mdef, L, St);
	false -> check_mdef(Mdef, L, add_error(L, {bad_attribute,Name}, St))
    end;
check_mdef([], _, St) -> St;
check_mdef(_, L, St) -> add_error(L, bad_mod_def, St).

check_imports(Is, L, St0) ->
    check_foreach(fun (I, St) -> check_import(I, L, St) end,
		  import, L, St0, Is).

check_import([from,Mod|Fs], L, St) when is_atom(Mod) ->
    check_import(fun ([F,A], Imps, S) when is_atom(F), is_integer(A) ->
			 {store({F,A}, F, Imps),S}
		 end, Mod, L, St, Fs);
check_import([rename,Mod|Rs], L, St) when is_atom(Mod) ->
    check_import(fun ([[F,A],R], Imps, S)
		     when is_atom(F), is_integer(A), is_atom(R) ->
			 {store({F,A}, R, Imps),S}
		 end, Mod, L, St, Rs);
check_import([prefix,Mod,Pre], L, St) when is_atom(Mod), is_atom(Pre) ->
    Pstr = atom_to_list(Pre),
    case find(Pstr, St#lint.pref) of
	{ok,_} -> bad_form_error(L, prefix, St);
	error ->
	    Pref = store(Pstr, Mod, St#lint.pref),
	    St#lint{pref=Pref}
    end;
check_import(_, L, St) -> bad_form_error(L, import, St).

check_import(Fun, Mod, L, St0, Fs) ->
    Imps0 = safe_fetch(Mod, St0#lint.imps, []),
    {Imps1,St1} = check_foldl(Fun, import, L, St0, Imps0, Fs),
    St1#lint{imps=store(Mod, Imps1, St1#lint.imps)}.

is_flist(Fs) -> is_flist(Fs, []).

is_flist([[F,Ar]|Fs], Funcs) when is_atom(F), is_integer(Ar), Ar >= 0 ->
    is_flist(Fs, add_element({F,Ar}, Funcs));
is_flist([], Funcs) -> {yes,Funcs};
is_flist(_, _) -> no.

%% check_module(FuncBindings, State) -> State.
%%  Do all the actual work checking a module.

check_module([], St) -> add_error(0, missing_module, St);
check_module(Fbs0, St0) ->
    %% Make an initial environment and set up state.
    {Predefs,Env0,St1} = init_state(St0),
    Fbs1 = Predefs ++ Fbs0,
    %% Now check definitions.
    {Fs,Env1,St2} = check_letrec_bindings(Fbs1, Env0, St1),
    %% Save functions and environment and test exports.
    St3 = St2#lint{funcs=Fs,env=Env1},
    check_exports(St3#lint.exps, Fs, St3).

%% init_state(State) -> {Predefs,Env,State}.
%%  Setup the initial predefines and state. Build dummies for
%%  predefined module_info and parameteried module functions, which
%%  makes it easier to later check redefines.

init_state(St) ->
    %% Add the imports.
    Env0 = foldl(fun ({M,Fs}, Env) ->
			 foldl(fun ({{F,A},R}, E) ->
				       add_ibinding(M, F, A, R, E)
			       end, Env, Fs)
		 end, new_env(), St#lint.imps),
    %% Basic predefines
    Predefs0 = [{module_info,[lambda,[],[quote,dummy]],1},
		{module_info,[lambda,[x],[quote,dummy]],1}],
    Exps0 = [{module_info,0},{module_info,1}],
    %% Now handle parameterised module.
    case St#lint.pars of
	none ->					%Normal module
	    {Predefs0,Env0,
	     St#lint{exps=add_exports(St#lint.exps, Exps0)}};
	Ps0 ->					%Parameterised module
	    {Ps1,Predefs1,Exps1} = para_defs(Ps0, Predefs0, Exps0, St),
	    {Predefs1,
	     add_vbindings([this|Ps1], Env0),
	     St#lint{exps=add_exports(St#lint.exps, Exps1)}}
    end.

para_defs(Ps, Predefs0, Exps0, St) ->
    Ar = length(Ps),
    Predefs1 = [{new,[lambda,Ps,[quote,dummy]],1}|Predefs0],
    Exps1 = add_element({new,Ar}, Exps0),
    case St#lint.extd of
	[] ->
	    {Ps,[{instance,[lambda,Ps,[quote,dummy]],1}|Predefs1],
	     add_element({instance,Ar},Exps1)};
	_ ->
	    {[base|Ps],[{instance,[lambda,[base|Ps],[quote,dummy]],1}|Predefs1],
	     add_element({instance,Ar+1},Exps1)}
    end.	    
	    
check_exports(all, _, St) -> St;		%All is all
check_exports(Exps, Fs, St) ->
    foldl(fun (E, S) ->
		  case is_element(E, Fs) of
		      true -> S;
		      false -> add_error(9999, {unbound_func,E}, S)
		  end
	  end, St, Exps).

%% add_exports(Old, More) -> New.

add_exports(all, _) -> all;
add_exports(_, all) -> all;
add_exports(Old, More) -> union(Old, More).

%% check_expr(Expr, Env, Line, State) -> State.
%% Check an expression.

%% Check the Core data special forms.
check_expr([quote,_], _, _, St) -> St;
check_expr([cons|[_,_]=As], Env, L, St) ->
    check_args(As, Env, L, St);
check_expr([car,E], Env, L, St) ->
    check_expr(E, Env, L, St);
check_expr([cdr,E], Env, L, St) ->
    check_expr(E, Env, L, St);
check_expr([list|As], Env, L, St) ->
    check_args(As, Env, L, St);
check_expr([tuple|As], Env, L, St) ->
    check_args(As, Env, L, St);
check_expr([binary|Segs], Env, L, St) ->
    expr_bitsegs(Segs, Env, L, St);
%% Check the Core closure special forms.
check_expr(['lambda'|Lambda], Env, L, St) ->
    check_lambda(Lambda, Env, L, St);
check_expr(['match-lambda'|Match], Env, L, St) ->
    check_match_lambda(Match, Env, L, St);
check_expr(['let'|Let], Env, L, St) ->
    check_let(Let, Env, L, St);
check_expr(['let-function'|Flet], Env, L, St) ->
    check_let_function(Flet, Env, L, St);
check_expr(['letrec-function'|Fletrec], Env, L, St) ->
    check_letrec_function(Fletrec, Env, L, St);
check_expr(['let-macro'|_], _, L, St) ->
    %% This should never occur! Removed by macro expander.
    bad_form_error(L, 'let-macro', St);
%% Check the Core control special forms.
check_expr(['progn'|Body], Env, L, St) ->
    check_body(Body, Env, L, St);
check_expr(['if',Test,True,False], Env, L, St) ->
    check_args([Test,True,False], Env, L, St);
check_expr(['if',Test,True], Env, L, St) ->
    check_args([Test,True], Env, L, St);
check_expr(['case'|B], Env, L, St) ->
    check_case(B, Env, L, St);
check_expr(['receive'|Cls], Env, L, St) ->
    check_rec_clauses(Cls, Env, L, St);
check_expr(['catch'|B], Env, L, St) ->
    check_body(B, Env, L, St);
check_expr(['try'|B], Env, L, St) ->
    check_try(B, Env, L, St);
check_expr(['funcall'|As], Env, L, St) ->
    check_args(As, Env, L, St);
check_expr(['call'|As], Env, L, St) ->
    check_args(As, Env, L, St);
%% Finally the general cases.
check_expr([Symb|As], Env, L, St0) when is_atom(Symb) ->
    St1 = check_args(As, Env, L, St0),	%Check arguments first
    %% Here we are not interested in HOW symb is associated to a
    %% function, just that it is.
    %% io:fwrite("fb: ~w ~p   ~p\n", [Symb,As,Env]),
    case is_fbound(Symb, safe_length(As), Env) of
	true -> St1;
	false -> add_error(L, {unbound_func,{Symb,safe_length(As)}}, St1)
    end;
check_expr([_|As], Env, L, St0) ->
    %% Function here is an expression, report error and check args.
    St1 = bad_form_error(L, application, St0),
    check_args(As, Env, L, St1);
check_expr([], _, _, St) -> St;			%Self evaluating []
check_expr(Symb, Env, L, St) when is_atom(Symb) ->
    %% Predefined functions exist only when called.
    case is_vbound(Symb, Env) of
	true -> St;
	false -> add_error(L, {unbound_symb,Symb}, St)
    end;
check_expr(Tup, _, _, St) when is_tuple(Tup) ->
    %% This just builds a tuple constant.
    St;
check_expr(_, _, _, St) -> St.		%Everything else is atomic

%% check_body(Body, Env, Line, State) -> State.
%% Check the calls in a body. A body is a proper list of calls. Env is
%% the set of known bound variables.

check_body(Body, Env, L, St) ->
    case is_proper_list(Body) of
	true -> check_exprs(Body, Env, L, St);
	false -> add_error(L, bad_body, St)
    end.

%% check_args(Args, Env, Line, State) -> State.
%% Check the expressions in an argument list.

check_args(Args, Env, L, St) ->
    case is_proper_list(Args) of
	true -> check_exprs(Args, Env, L, St);
	false -> add_error(L, bad_args, St)
    end.

%% check_exprs(Exprs, Env, Line, State) -> State.
%% Check a list of expressions. We know it's a proper list.

check_exprs(Es, Env, L, St) ->
    foldl(fun (E, S) -> check_expr(E, Env, L, S) end, St, Es).

%% expr_bitsegs(BitSegs, Env, Line, State) -> State.
%% expr_bitseg(BitSeg, Env, Line, State) -> State.
%% expr_bitspecs(BitSpecs, Env, Line, State) -> State.
%% expr_bitspec(BitSpec, Env, Line, State) -> State.
%% Functions for checking expression bitsegments.

expr_bitsegs(Segs, Env, L, St0) ->
    foldl(fun (S, St) -> expr_bitseg(S, Env, L, St) end, St0, Segs).

expr_bitseg([N|Specs], Env, L, St0) ->
    St1 = check_expr(N, Env, L, St0),		%Be lazy here
    expr_bitspecs(Specs, Env, L, St1);
expr_bitseg(N, Env, L, St) ->
    check_expr(N, Env, L, St).
 
expr_bitspecs(Specs, Env, L, St0) ->
    foldl(fun (S, St) -> expr_bitspec(S, Env, L, St) end, St0, Specs).    

expr_bitspec([size,N], Env, L, St) ->		%Shadow for (size expr)
    check_expr(N, Env, L, St);
expr_bitspec(S, Env, L, St) ->
    pat_bitspec(S, Env, L, St).

%% check_lambda(LambdaBody, Env, Line, State) -> State.
%% Check form (lambda Args ...).

check_lambda([Args|Body], Env, L, St0) ->
    {Vs,St1} = check_lambda_args(Args, L, St0),
    check_body(Body, add_vbindings(Vs, Env), L, St1);
check_lambda(_, _, L, St) -> bad_form_error(L, lambda, St).

check_lambda_args(Args, L, St) ->
    %% Check for multiple variables but allow don't care variables,
    %% same rules as for pattern symbols.
    Check = fun (A, {As,S}) -> pat_symb(A, As, L, S) end,
    case is_symb_list(Args) of
	true -> foldl(Check, {[],St}, Args);
	false -> {[],bad_form_error(L, lambda, St)}
    end.

%% check_match_lambda(MatchBody, Env, Line, State) -> State.
%% Check form (match-lambda Clause ...), must be at least one clause.
%% First check arities then each clause, don't assume anything.

check_match_lambda([[Pat|_]|_]=Cls, Env, L, St0) ->
    St1 = case is_proper_list(Pat) of
	      true -> check_ml_arity(tl(Cls), length(Pat), L, St0);
	      false -> St0
	  end,
    check_ml_clauses(Cls, Env, L, St1);
check_match_lambda(_, _, L, St) ->		%Totally wrong
    bad_form_error(L, 'match-lambda', St).

check_ml_arity([[Pat|_]|Cls], Ar, L, St) ->
    case is_proper_list(Pat) andalso length(Pat) == Ar of
	true -> check_ml_arity(Cls, Ar, L, St);
	false -> add_error(L, match_lambda_arity, St)
    end;
check_ml_arity([], _, _, St) -> St.

check_ml_clauses(Cls, Env, L, St) ->
    check_foreach(fun (Cl, S) -> check_clause(Cl, Env, L, S) end,
		  'match-lambda', L, St, Cls).

%% check_let(LetBody, Env, Line, State) -> {Env,State}.
%%  Check let variable bindings and then body. Must be careful to use
%%  correct bindings.

check_let([Vbs|Body], Env, L, St0) ->
    Check = fun (Vb, Pvs, Sta) ->
		    {Pv,Stb} = check_let_vb(Vb, Env, L, Sta),
		    Stc = case intersection(Pv, Pvs) of
			      [] -> Stb;
			      Ivs -> multi_var_error(L, Ivs, Stb)
			  end,
		    {union(Pv, Pvs), Stc}
	    end,
    {Pvs,St1} = check_foldl(Check, 'let', L, St0, [], Vbs),
    check_body(Body, add_vbindings(Pvs, Env), L, St1);
check_let(_, _, L, St) ->
    bad_form_error(L, 'let', St).

check_let_vb([Pat,['when',G],Val], Env, L, St0) ->
    St1 = check_expr(Val, Env, L, St0),
    {Pvs,St2} = check_pat(Pat, Env, L, St1),
    St3 = check_gexpr(G, add_vbindings(Pvs, Env), L, St2), %The guard
    {Pvs,St3};
check_let_vb([Pat,Val], Env, L, St0) ->
    St1 = check_expr(Val, Env, L, St0),
    check_pat(Pat, Env, L, St1);
check_let_vb(_, _, L, St) -> {[],bad_form_error(L, 'let', St)}.

%% check_let_function(FletBody, Env, Line, State) -> {Env,State}.
%%  Check a let-function form (let-function FuncBindings ... ). 

check_let_function([Fbs0|Body], Env0, L, St0) ->
    %% Collect correct function definitions.
    {Fbs1,St1} = collect_let_funcs(Fbs0, 'let-function', L, St0),
    {_,Env1,St2} = check_let_bindings(Fbs1, Env0, St1),
    check_body(Body, Env1, L, St2).

%% check_letrec_function(FletrecBody, Env, Line, State) -> {Env,State}.
%%  Check a letrec-function form (letrec-function FuncBindings ... ). 

check_letrec_function([Fbs0|Body], Env0, L, St0) ->
    %% Collect correct function definitions.
    {Fbs1,St1} = collect_let_funcs(Fbs0, 'letrec-function', L, St0),
    {_,Env1,St2} = check_letrec_bindings(Fbs1, Env0, St1),
    check_body(Body, Env1, L, St2).

%% collect_let_funcs(FuncDefs, Type, Line, State) -> {Funcbindings,State}.
%%  Collect the function definitions for a let/letrec-function
%%  checking right types. Returns same format as top-level collect.

collect_let_funcs(Fbs0, Type, L, St0) ->
    Check = fun ([V,['lambda'|_]=Lambda], Fbs, St) when is_atom(V) ->
		    {[{V,Lambda,L}|Fbs],St};
		([V,['match-lambda'|_]=Match], Fbs, St) when is_atom(V) ->
		    {[{V,Match,L}|Fbs],St};
		(_, Fbs, St) -> {Fbs,bad_form_error(L, Type, St)}
	    end,
    check_foldr(Check, Type, L, St0, [], Fbs0).	%Preserve order

%% check_fbindings(FuncBindings, State) -> {Funcs,State}.
%%  Check function bindings for format and for multiple fucntion
%%  definitions.

check_fbindings(Fbs0, St0) ->
    AddFb = fun(F, Fs, L, St) ->
		    case member(F, Fs) of
			true -> {Fs,add_error(L, {redef_fun,F}, St)};
			false -> {add_element(F, Fs),St}
		    end
	    end,
    Check = fun ({V,[lambda,Args|_],L}, {Fs,St}) ->
		    case is_symb_list(Args) of
			true -> AddFb({V,length(Args)}, Fs, L, St);
			false -> {Fs,bad_form_error(L, lambda, St)}
		    end;
		({V,['match-lambda',[Pats|_]|_],L}, {Fs,St}) ->
		    case is_proper_list(Pats) of
			true -> AddFb({V,length(Pats)}, Fs, L, St);
			false -> {Fs,bad_form_error(L, 'match-lambda', St)}
		    end
	    end,
    foldl(Check, {[],St0}, Fbs0).

%% check_let_bindings(FuncBindings, Env, State) -> {Funcs,Env,State}.
%%  Check the function bindings and return new environment. We only
%%  have to worry about checking for the valid forms as the rest will
%%  already be reported. Use explicit line number in element.

check_let_bindings(Fbs, Env0, St0) ->
    {Fs,St1} = check_fbindings(Fbs, St0),
    %% Now check function definitions.
    St2 = foldl(fun ({_,[lambda|Lambda],L}, St) ->
			check_lambda(Lambda, Env0, L, St);
		    ({_,['match-lambda'|Match],L}, St) ->
			check_match_lambda(Match, Env0, L, St)
		end, St1, Fbs),
    %% Add to environment
    Env1 = foldl(fun ({F,A}, Env) -> add_fbinding(F, A, Env) end, Env0, Fs),
    {Fs,Env1,St2}.

%% check_letrec_bindings(FuncBindings, Env, State) -> {Funcs,Env,State}.
%%  Check the function bindings and return new environment. We only
%%  have to worry about checking for the valid forms as the rest will
%%  already be reported. Use explicit line number in element.

check_letrec_bindings(Fbs, Env0, St0) ->
    {Fs,St1} = check_fbindings(Fbs, St0),
    %% Add to environment
    Env1 = foldl(fun ({F,A}, Env) -> add_fbinding(F, A, Env) end, Env0, Fs),
    %% Now check function definitions.
    St2 = foldl(fun ({_,[lambda|Lambda],L}, St) ->
			check_lambda(Lambda, Env1, L, St);
		    ({_,['match-lambda'|Match],L}, St) ->
			check_match_lambda(Match, Env1, L, St)
		end, St1, Fbs),
    {Fs,Env1,St2}.

%% check_case(CaseBody, Env, Line, State) -> State.
%% Check form (case Expr Clause ...), must be at least one clause.

check_case([E|[_|_]=Cls], Env, L, St0) ->
    St1 = check_expr(E, Env, L, St0),
    check_case_clauses(Cls, Env, L, St1);
check_case(_, _, L, St) ->
    bad_form_error(L, 'case', St).

check_case_clauses(Cls, Env, L, St) ->
    check_foreach(fun (Cl, S) -> check_clause(Cl, Env, L, S) end,
		  'case', L, St, Cls).
	    
check_rec_clauses([['after',T|B]], Env, L, St0) ->
    St1 = check_expr(T, Env, L, St0),
    check_body(B, Env, L, St1);
check_rec_clauses([['after'|_]|Cls], Env, L, St) ->
    %% Only allow after last and with timeout.
    check_rec_clauses(Cls, Env, L, bad_form_error(L, 'receive', St));
check_rec_clauses([Cl|Cls], Env, L, St) ->
    check_rec_clauses(Cls, Env, L, check_clause(Cl, Env, L, St));
check_rec_clauses([], _, _, St) -> St;
check_rec_clauses(_, _, L, St) -> bad_form_error(L, 'receive', St).

check_clause([_|_]=Cl, Env0, L, St0) ->
    {B,_,Env1,St1} = check_pat_guard(Cl, Env0, L, St0),
    check_body(B, Env1, L, St1);
check_clause(_, _, L, St) -> bad_form_error(L, clause, St).

%% check_try(TryBody, Env, Line, State) -> State.
%% Check a (try ...) form making sure that the right combination of
%% options are present. Case is optional, but we must have at least
%% one of catch and after.

check_try([E,['case'|Cls]|Catch], Env, L, St0) ->
    St1 = check_expr(E, Env, L, St0),
    St2 = check_case_clauses(Cls, Env, L, St1),
    check_try_catch(Catch, Env, L, St2);
check_try([E|Catch], Env, L, St0) ->
    St1 = check_expr(E, Env, L, St0),
    check_try_catch(Catch, Env, L, St1);
check_try(_, _, L, St) -> bad_form_error(L, 'try', St).

check_try_catch([['catch'|Cls]], Env, L, St) ->
    check_case_clauses(Cls, Env, L, St);
check_try_catch([['catch'|Cls],['after'|B]], Env, L, St0) ->
    St1 = check_case_clauses(Cls, Env, L, St0),
    check_body(B, Env, L, St1);
check_try_catch([['after'|B]], Env, L, St) ->
    check_body(B, Env, L, St);
check_try_catch(_, _, L, St) -> bad_form_error(L, 'try', St).

%% check_pat_guard([Pat{,Guard}|Body, Env, L, State) ->
%%      {Body,PatVars,Env,State}.
%%  Check pattern and guard in a clause. We know there is at least pattern!

check_pat_guard([Pat,['when',G]|Body], Env0, L, St0) ->
    {Pvs,St1} = check_pat(Pat, Env0, L, St0),
    Env1 = add_vbindings(Pvs, Env0),
    St2 = check_gexpr(G, Env1, L, St1),
    {Body,Pvs,Env1,St2};
check_pat_guard([Pat|Body], Env0, L, St0) ->
    {Pvs,St1} = check_pat(Pat, Env0, L, St0),
    Env1 = add_vbindings(Pvs, Env0),
    {Body,Pvs,Env1,St1}.

%% check_gexpr(Call, Env, Line, State) -> State.
%% Check a guard expression. This is a restricted body expression.

check_gexpr([_|_]=Call, Env, L, St) ->
    check_gcall(Call, Env, L, St);
check_gexpr([], _, _, St) -> St;
check_gexpr(Symb, Env, L, St) when is_atom(Symb) ->
    case is_vbound(Symb, Env) of
	true -> St;
	false -> add_error(L, {unbound_symb,Symb}, St)
    end;
check_gexpr(Tup, _, _, St) when is_tuple(Tup) ->
    %% This just builds a tuple constant.
    St;
check_gexpr(_, _, _, St) -> St.			%Everything else is atomic

%% check_gcall(Gcall, Env, Line, State) -> State.
%% Check a function gcall.

%% Check the known special cases.
check_gcall([quote,_], _, _, St) -> St;
check_gcall([cons|[_,_]=As], Env, L, St) ->
    check_gargs(As, Env, L, St);
check_gcall([list|As], Env, L, St) ->
    check_gargs(As, Env, L, St);
check_gcall([tuple|As], Env, L, St) ->
    check_gargs(As, Env, L, St);
check_gcall([binary|Segs], Env, L, St) ->
    gexpr_bitsegs(Segs, Env, L, St);
check_gcall(['case',E|Cls], Env, L, St0) ->
    St1 = check_gexpr(E, Env, L, St0),
    check_gclauses(Cls, Env, L, St1);
check_gcall(['try'|B], Env, L, St) ->
    check_gtry(B, Env, L, St);
%% Finally the general case.
check_gcall([Symb|As], Env, L, St0) when is_atom(Symb) ->
    St1 = check_gargs(As, Env, L, St0),
    %% Here we are not interested in HOW symb is associated to a
    %% function, just that it is.
    case is_gbound(Symb, safe_length(As), Env) of
	true -> St1;
	false -> add_error(L, {unbound_func,{Symb,safe_length(As)}}, St1)
    end;
check_gcall(_, _, L, St) ->
    add_error(L, illegal_guard, St).

%% check_gbody(Body, Env, Line, State) -> State.
%% check_gargs(Args, Env, Line, State) -> State.
%% check_gexprs(Exprs, Env, Line, State) -> State.
%% The guard counter parts. Check_gexprs assumes a proper list.

check_gbody(Body, Env, L, St) ->
    case is_proper_list(Body) of
	true -> check_gexprs(Body, Env, L, St);
	false -> add_error(L, bad_body, St)
    end.

check_gargs(Args, Env, L, St) ->
    case is_proper_list(Args) of
	true -> check_gexprs(Args, Env, L, St);
	false -> add_error(L, bad_args, St)
    end.

check_gexprs(Es, Env, L, St) ->
    foldl(fun (E, S) -> check_gexpr(E, Env, L, S) end, St, Es).

check_gclauses(Cls, Env, L, St) ->
    check_foreach(fun (Cl, S) -> check_gclause(Cl, Env, L, S) end,
		  'case', L, St, Cls).

check_gclause([_|_]=Cl, Env0, L, St0) ->	%Check basic clause length
    {B,_,Env1,St1} = check_pat_guard(Cl, Env0, L, St0),
    check_gbody(B, Env1, L, St1);
check_gclause(_, _, L, St) -> bad_form_error(L, clause, St).

%% check_gtry(GtryBody, Env, Line, State) -> State.
%% Check a (try ...) form making sure that the right combination of
%% options are present.

check_gtry([E,['case'|Cls]|Catch], Env, L, St0) ->
    St1 = check_gexpr(E, Env, L, St0),
    St2 = check_case_clauses(Cls, Env, L, St1),
    check_gtry_catch(Catch, Env, L, St2);
check_gtry([E|Catch], Env, L, St0) ->
    St1 = check_gexpr(E, Env, L, St0),
    check_gtry_catch(Catch, Env, L, St1);
check_gtry(_, _, L, St) -> bad_form_error(L, 'try', St).

check_gtry_catch([['catch'|Cls]], Env, L, St) ->
    check_gclauses(Cls, Env, L, St);
check_gtry_catch([['catch'|Cls],['after'|B]], Env, L, St0) ->
    St1 = check_gclauses(Cls, Env, L, St0),
    check_gbody(B, Env, L, St1);
check_gtry_catch([['after'|B]], Env, L, St) ->
    check_gbody(B, Env, L, St);
check_gtry_catch(_, _, L, St) -> bad_form_error(L, 'try', St).

%% gexpr_bitsegs(BitSegs, Env, Line, State) -> State.
%% gexpr_bitseg(BitSeg, Env, Line, State) -> State.
%% gexpr_bitspecs(BitSpecs, Env, Line, State) -> State.
%% gexpr_bitspec(BitSpec, Env, Line, State) -> State.
%% Functions for checking guard expression bitsegments.

gexpr_bitsegs(Segs, Env, L, St0) ->
    foldl(fun (S, St) -> gexpr_bitseg(S, Env, L, St) end, St0, Segs).

gexpr_bitseg([N|Specs], Env, L, St0) ->
    St1 = check_gexpr(N, Env, L, St0),		%Be lazy here
    gexpr_bitspecs(Specs, Env, L, St1);
gexpr_bitseg(N, Env, L, St) ->
    check_gexpr(N, Env, L, St).
 
gexpr_bitspecs(Specs, Env, L, St0) ->
    foldl(fun (S, St) -> gexpr_bitspec(S, Env, L, St) end, St0, Specs).    

gexpr_bitspec([size,N], Env, L, St) ->		%Shadow for (size expr)
    check_gexpr(N, Env, L, St);
gexpr_bitspec(S, Env, L, St) ->
    pat_bitspec(S, Env, L, St).

%% check_pat(Pattern, Env, L, State) -> {PatVars,State}.
%% Return the *set* of Variables in Pattern.

check_pat(Pat, Env, L, St) ->
    %% io:fwrite("pat: ~p\n", [Pat]),
    try
	check_pat(Pat, [], Env, L, St)
    catch
	_:_ -> add_error(L, illegal_pat, St)
    end.

check_pat([quote,_], Vs, _, _, St) -> {Vs,St};	%Go no deeper with quote
check_pat([tuple|Ps], Vs, Env, L, St) ->	%Tuple elements
    check_pat(Ps, Vs, Env, L, St);
check_pat([binary|Segs], Vs, Env, L, St) ->
    pat_bitsegs(Segs, Vs, Env, L, St);
check_pat(['=',P1,P2], Vs0, Env, L, St0) ->
    %% Must check each pattern separately as same variable can occur
    %% in both branches.
    {Vs1,St1} = check_pat(P1, Vs0, Env, L, St0),
    {Vs2,St2} = check_pat(P2, Vs0, Env, L, St1),
    St3 = case check_alias(P1, P2) of
	      true -> St2;		%Union of variables now visible
	      false -> add_error(L, bad_alias, St2)
	  end,
    {union(Vs1, Vs2),St3};
check_pat([H|T], Vs0, Env, L, St0) ->
    {Vs1,St1} = check_pat(H, Vs0, Env, L, St0),
    check_pat(T, Vs1, Env, L, St1);
check_pat([], Vs, _, _, St) -> {Vs,St};
check_pat(Symb, Vs, _, L, St) when is_atom(Symb) ->
    pat_symb(Symb, Vs, L, St);
check_pat(_, Vs, _, _, St) -> {Vs,St}.		%Atomic

pat_symb('_', Vs, _, St) -> {Vs,St};		%Don't care variable
pat_symb(Symb, Vs, L, St) ->
    case is_element(Symb, Vs) of
	true -> {Vs,multi_var_error(L, Symb, St)};
	false -> {add_element(Symb, Vs),St}
    end.    

%% check_alias(Pattern, Pattern) -> true | false.
%%  Check if two aliases are compatible. Note that binaries can never
%%  be aliased, this is from erlang.

check_alias([quote,P1], [quote,P2]) -> P1 =:= P2;
check_alias([tuple|Ps1], [tuple|Ps2]) ->
    check_alias_list(Ps1, Ps2);
%% check_alias([tuple|Ps1], P2) when is_tuple(P2) ->
%%     check_alias_list(Ps1, tuple_to_list(P2));
%% check_alias(P1, [tuple|Ps2]) when is_tuple(P1) ->
%%     check_alias_list(tuple_to_list(P1), Ps2);
check_alias([binary|_], [binary|_]) -> false;
check_alias([P1|Ps1], [P2|Ps2]) ->
    check_alias(P1, P2) andalso check_alias(Ps1, Ps2);
check_alias(P1, _) when is_atom(P1) -> true;	%Variable
check_alias(_, P2) when is_atom(P2) -> true;
check_alias(P1, P2) -> P1 =:= P2.		%Atomic

check_alias_list([P1|Ps1], [P2|Ps2]) ->
    check_alias(P1, P2) andalso check_alias_list(Ps1, Ps2);
check_alias_list([], []) -> true;
check_alias_list(_, _) -> false.

%% pat_bitsegs(BitSegs, PatVars, Env, Line, State) -> {PatVars,State}.
%% pat_bitseg(BitSeg, PatVars, Env, Line, State) -> {PatVars,State}.
%% pat_bitel(BitElement, PatVars, Env, Line, State) -> {PatVars,State}.
%% pat_bitspecs(BitSpecs, Env, Line, State) -> State.
%% pat_bitspec(BitSpec, Env, Line, State) -> State.
%% Functions for checking pattern bitsegments.

pat_bitsegs(Segs, Vs0, Env, L, St0) ->
    foldl(fun (S, {Vs,St}) -> pat_bitseg(S, Vs, Env, L, St) end,
	  {Vs0,St0}, Segs).

pat_bitseg([N|Specs], Vs0, Env, L, St0) ->
    {Vs1,St1} = pat_bitel(N, Vs0, Env, L, St0),
    {Vs1,pat_bitspecs(Specs, Env, L, St1)};
pat_bitseg(N, Vs, Env, L, St) ->
    pat_bitel(N, Vs, Env, L, St).

pat_bitel(N, Vs, _, _, St) when is_number(N) -> {Vs,St};
pat_bitel(Symb, Vs, _, L, St) when is_atom(Symb) ->
    pat_symb(Symb, Vs, L, St);
pat_bitel(_, Vs, _, L, St) -> {Vs,add_error(L, illegal_bitseg, St)}.

pat_bitspecs(Specs, Env, L, St0) ->
    foldl(fun (S, St) -> pat_bitspec(S, Env, L, St) end, St0, Specs).    

%% Types.
pat_bitspec(integer, _, _, St) -> St;
pat_bitspec(float, _, _, St) -> St;
pat_bitspec(binary, _, _, St) -> St;
pat_bitspec(bytes, _, _, St) -> St;
pat_bitspec(bitstring, _, _, St) -> St;
pat_bitspec(bits, _, _, St) -> St;
%% Unicode types.
pat_bitspec('utf-8', _, _, St) -> St;
pat_bitspec('utf-16', _, _, St) -> St;
pat_bitspec('utf-32', _, _, St) -> St;
%% Endianness
pat_bitspec('big-endian', _, _, St) -> St;
pat_bitspec('little-endian', _, _, St) -> St;
pat_bitspec('native-endian', _, _, St) -> St;
%% Sign.
pat_bitspec(signed, _, _, St) -> St;
pat_bitspec(unsigned, _, _, St) -> St;
%% Size.
pat_bitspec([unit,N], _, _, St) when is_integer(N), N >= 1, N =< 256 -> St;
pat_bitspec([size,N], _, _, St) when is_integer(N), N > 0 -> St;
pat_bitspec([size,Symb], Env, L, St) when is_atom(Symb) ->
    %% Size must be bound here.
    case is_vbound(Symb, Env) of
	true -> St;
	false -> add_error(L, {unbound_symb,Symb}, St)
    end;
pat_bitspec(Spec, _, L, St) ->
    add_error(L, {illegal_bitspec,Spec}, St).

%% check_foreach(Function, Type, Line, State, Forms) -> State.
%% check_map(Function, Type, Line, State, Forms) -> {Results,State}.
%% check_foldl(Function, Type, Line, State, Acc, Forms) -> {Acc,State}.
%% check_foldr(Function, Type, Line, State, Acc, Forms) -> {Acc,State}.
%%  These functions automatically manage a state variable and check for
%%  proper top list. Could easily and clearly be done with a Lisp
%%  macro.

%% Versions which only check for proper top list.
check_foreach(Fun, T, L, St0, [F|Fs]) ->
    St1 = Fun(F, St0),
    check_foreach(Fun, T, L, St1, Fs);
check_foreach(_, _, _, St, []) -> St;
check_foreach(_, T, L, St, _) -> bad_form_error(L, T, St).

check_map(Fun, T, L, St0, [F|Fs]) ->
    {R,St1} = Fun(F, St0),
    {Rs,St2} = check_map(Fun, T, L, St1, Fs),
    {[R|Rs],St2};
check_map(_, _, _, St, []) -> {[],St};
check_map(_, T, L, St, _) -> {[],bad_form_error(L, T, St)}.

check_foldl(Fun, T, L, St0, Acc0, [F|Fs]) ->
    {Acc1,St1} = Fun(F, Acc0, St0),
    check_foldl(Fun, T, L, St1, Acc1, Fs);
check_foldl(_, _, _, St, Acc, []) -> {Acc,St};
check_foldl(_, T, L, St, Acc, _) -> {Acc,bad_form_error(L, T, St)}.

check_foldr(Fun, T, L, St0, Acc0, [F|Fs]) ->
    {Acc1,St1} = check_foldr(Fun, T, L, St0, Acc0, Fs),
    Fun(F, Acc1, St1);
check_foldr(_, _, _, St, Acc, []) -> {Acc,St};
check_foldr(_, T, L, St, Acc, _) -> {Acc,bad_form_error(L, T, St)}.

%% Non-catching versions for checking the code.
%% check_foreach(Fun, T, L, St, Fs) -> foldl(Fun, St, Fs).
	      
%% check_map(Fun, T, L, St, Fs) -> mapfoldl(Fun, St, Fs).

%% check_foldl(Fun, T, L, St, Acc, Fs) ->
%%     foldl(fun (F, {A,S}) -> Fun(F, A, S) end, {Acc,St}, Fs).

%% check_foldr(Fun, T, L, St, Acc, Fs) ->
%%     foldr(fun (F, {A,S}) -> Fun(F, A, S) end, {Acc,St}, Fs).

%% Versions which completely wrap with a try.
%% check_foreach(Fun, T, L, St, Fs) ->
%%     try
%% 	foldl(Fun, St, Fs)
%%     catch
%% 	_:_ -> bad_form_error(L, T, St)
%%     end.
	      
%% check_map(Fun, T, L, St, Fs) ->
%%     try
%% 	mapfoldl(Fun, St, Fs)
%%     catch
%% 	_:_ -> {[],bad_form_error(L, T, St)}
%%     end.

%% check_foldl(Fun, T, L, St, Acc, Fs) ->
%%     try
%% 	foldl(fun (F, {A,S}) -> Fun(F, A, S) end, {Acc,St}, Fs)
%%     catch
%% 	_:_ -> {{Acc,St},bad_form_error(L, T, St)}
%%     end.

%% check_foldr(Fun, T, L, St, Acc, Fs) ->
%%     try
%% 	foldr(fun (F, {A,S}) -> Fun(F, A, S) end, {Acc,St}, Fs)
%%     catch
%% 	_:_ -> {{Acc,St},bad_form_error(L, T, St)}
%%     end.

%% safe_length(List) -> Length.
%%  Safely check length of list, can handle improper lists.

safe_length(L) -> safe_length(L, 0).

safe_length([_|L], Acc) -> safe_length(L, Acc+1);
safe_length(_, Acc) -> Acc.

%% add_error(Error, State) -> State.
%% add_error(Line, Error, State) -> State.
%% add_warning(Line, Warning, State) -> State.

add_error(L, E, St) ->
    St#lint{errors=St#lint.errors ++ [{L,?MODULE,E}]}.

%% add_warning(L, W, St) ->
%%     St#lint{warnings=St#lint.warnings ++ [{L,?MODULE,W}]}.

bad_form_error(L, F, St) ->
    add_error(L, {bad_form,F}, St).

multi_var_error(L, V, St) ->
    add_error(L, {multi_var,V}, St).

%% Interface to the binding functions in lfe_lib.
%% These just add arity as a dummy values as we are not interested in
%% value but it might be useful.

add_fbinding(N, A, Env) -> lfe_lib:add_fbinding(N, A, A, Env).

add_vbindings(Vs, Env) ->
    foldl(fun (V, E) -> lfe_lib:add_vbinding(V, dummy, E) end, Env, Vs).

%% safe_fetch(Key, Dict, Default) -> Value.

safe_fetch(Key, D, Def) ->
    case find(Key, D) of
	{ok,Val} -> Val;
	error -> Def
    end.
