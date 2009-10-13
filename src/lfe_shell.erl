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

%% File    : lfe_shell.erl
%% Author  : Robert Virding
%% Purpose : A simple Lisp Flavoured Erlang shell.

-module(lfe_shell).

-export([start/0,start/1,server/0,server/1]).

-import(lfe_lib, [new_env/0,add_env/2,
		  add_vbinding/3,add_vbindings/2,is_vbound/2,get_vbinding/2,
		  fetch_vbinding/2,update_vbinding/3,
		  add_fbinding/4,add_fbindings/3,get_fbinding/3,add_ibinding/5,
		  get_gbinding/3,add_mbinding/3]).

-import(orddict, [store/3,find/2]).
-import(ordsets, [add_element/2]).
-import(lists, [map/2,foreach/2,foldl/3]).

%% -compile([export_all]).

start() ->
    spawn(fun () -> server(default) end).

start(P) ->
    spawn(fun () -> server(P) end).

server() -> server(default).

server(_) ->
    io:fwrite("LFE Shell V~s (abort with ^G)\n",
	      [erlang:system_info(version)]),
    %% Add default nil bindings to predefined shell variables.
    Env0 = add_shell_macros(new_env()),
    Env1 = add_shell_vars(Env0),
    server_loop(Env1, Env1).

server_loop(Env0, BaseEnv) ->
    Env = try
	      %% Read the form
	      io:put_chars("> "),
	      Form = lfe_io:read(),
	      Env1 = update_vbinding('-', Form, Env0),
	      %% Macro expand and evaluate it.
	      {Value,Env2} = eval_form(Form, Env1, BaseEnv),
	      %% Print the result, but only to depth 30.
	      VS = lfe_io:prettyprint1(Value, 30),
	      io:requests([{put_chars,VS},nl]),
	      %% Update bindings.
	      Env3 = update_shell_vars(Form, Value, Env2),
	      %% lfe_io:prettyprint({Env1,Env2}), io:nl(),
	      Env3
	  catch
	      %% Very naive error handling, just catch, report and
	      %% ignore whole caboodle.
	      Class:Error ->
		  %% Use LFE's simplified version of erlang shell's error
		  %% reporting but which LFE prettyprints data.
		  St = erlang:get_stacktrace(),
		  Sf = fun (M, _F, _A) ->
			       (M == lfe_eval) or (M == lfe_shell)
		       end,
		  Ff = fun (T, I) -> lfe_io:prettyprint1(T, 15, I, 80) end,
		  Cs = lfe_lib:format_exception(Class, Error, St, Sf, Ff, 1),
		  io:put_chars(Cs),
		  io:nl(),
		  %% lfe_io:prettyprint({'EXIT',Class,Error,St}), io:nl(),
		  Env0
	  end,
    server_loop(Env, BaseEnv).

add_shell_vars(Env) ->
    %% Add default shell expression variables.
    foldl(fun (Symb, E) -> add_vbinding(Symb, [], E) end, Env,
	  ['+','++','+++','-','*','**','***']).

update_shell_vars(Form, Value, Env) ->
    foldl(fun ({Symb,Val}, E) -> update_vbinding(Symb, Val, E) end,
	  Env,
	  [{'+++',fetch_vbinding('++', Env)},
	   {'++',fetch_vbinding('+', Env)},
	   {'+',Form},
	   {'***',fetch_vbinding('**', Env)},
	   {'**',fetch_vbinding('*', Env)},
	   {'*',Value}]).

add_shell_macros(Env0) ->
    %% We write macros in LFE and expand them with macro package.
    Ms = [
	 ],
    {_,Env1} = lfe_macro:macro_forms(Ms, Env0),
    %% io:fwrite("asm: ~p\n", [Env1]),
    Env1.

%% eval_form(Form, EvalEnv, BaseEnv) -> {Value,Env}.

eval_form(Form, Env0, Benv) ->
    %% lfe_io:prettyprint({Form,Env0}),
    %% io:fwrite("ef: ~p\n", [{Form,Env0}]),
    Eform = lfe_macro:expand_form(Form, Env0),
    case eval_internal(Eform, Env0, Benv) of
	{yes,Value,Env1} -> {Value,Env1};
	no ->
	    %% Normal evaluation of form.
	    {lfe_eval:expr(Eform, Env0),Env0}
    end.

%% eval_internal(Form, EvalEnv, BaseEnv) -> {yes,Value,Env} | no.
%%  Check for and evaluate internal functions. These all evaluate
%%  their arguments.

eval_internal([slurp|Args], Eenv, Benv) ->	%Slurp in a file
    slurp(Args, Eenv, Benv);
eval_internal([unslurp|_], _, Benv) ->		%Forget everything
    {yes,ok,Benv};
eval_internal([c|Args], Eenv, Benv) ->		%Compile an LFE file
    c(Args, Eenv, Benv);
eval_internal([ec|Args], Eenv, Benv) ->		%Compile an Erlang file
    ec(Args, Eenv, Benv);
eval_internal([l|Args], Eenv, Benv) ->		%Load modules
    l(Args, Eenv, Benv);
eval_internal([m|Args], Eenv, Benv) ->		%Module info
    m(Args, Eenv, Benv);
eval_internal([macroexpand,S], Eenv, Benv) ->	%Macroexpand top of form
    macroexpand(S, Eenv, Benv);
eval_internal(['macroexpand-1',S], Eenv, Benv) ->
    macroexpand_1(S, Eenv, Benv);
eval_internal(['macroexpand-all',S], Eenv, Benv) ->
    macroexpand_all(S, Eenv, Benv);
eval_internal([set|Args], Eenv, Benv) ->	%Set variables in shell
    set(Args, Eenv, Benv);
eval_internal(_, _, _) -> no.			%Not an internal function

%% c(Args, EvalEnv, BaseEnv) -> {yes,Res,Env}.
%%  Compile and load an LFE file.

c([F], Eenv, Benv) -> c([F,[]], Eenv, Benv);
c([F,Os], Eenv, _) ->
    Name = lfe_eval:expr(F, Eenv),		%Evaluate arguments
    Opts = lfe_eval:expr(Os, Eenv),
    Loadm = fun (Mod) ->
		    Base = filename:basename(Name, ".lfe"),
		    code:purge(Mod),
		    R = code:load_abs(Base),
		    {yes,R,Eenv}
	    end,
    case lfe_comp:file(Name, [report,verbose|Opts]) of
	{ok,Mod,_} -> Loadm(Mod);
	{ok,Mod} -> Loadm(Mod);
	Other -> {yes,Other,Eenv}
    end;
c(_, _, _) -> no.				%Unknown function,

%% ec(Args, EvalEnv, BaseEnv) -> {yes,Res,Env}.
%%  Compile and load an Erlang file.

ec([F], Eenv, Benv) -> ec([F,[]], Eenv, Benv);
ec([F,Os], Eenv, _) ->
    Name = lfe_eval:expr(F, Eenv),		%Evaluate arguments
    Opts = lfe_eval:expr(Os, Eenv),
    {yes,c:c(Name, Opts),Eenv};
ec(_, _, _) -> no.				%Unknown function

%% l(Args, EvalEnv, BaseEnv) -> {yes,Res,Env}.
%%  Load the modules in Args.

l(Args, Eenv, _) ->
    {yes,map(fun (M) -> c:l(lfe_eval:expr(M, Eenv)) end, Args), Eenv}.

%% m(Args, EvalEnv, BaseEnv) -> {yes,Res,Env}.
%%  Module info.

m([], Eenv, _) -> {yes,c:m(),Eenv};
m(Args, Eenv, _) ->
    {yes,map(fun (M) -> c:m(lfe_eval:expr(M, Eenv)) end, Args), Eenv}.

%% macroexpand(Sexpr, EvalEnv, BaseEnv) -> {yes,Res,Env}.
%% macroexpand_1(Sexpr, EvalEnv, BaseEnv) -> {yes,Res,Env}.
%% macroexpand_all(Sexpr, EvalEnv, BaseEnv) -> {yes,Res,Env}.
%%  We special case these at shell level so as to get shell environment.

macroexpand(S, Eenv, _) ->
    case lfe_macro:expand_macro(lfe_eval:expr(S, Eenv), Eenv) of
	{yes,Exp} -> {yes,Exp,Eenv};
	no -> {yes,S,Eenv}
    end.

macroexpand_1(S, Eenv, _) ->
    case lfe_macro:expand_macro_1(lfe_eval:expr(S, Eenv), Eenv) of
	{yes,Exp} -> {yes,Exp,Eenv};
	no -> {yes,S,Eenv}
    end.

macroexpand_all(S, Eenv, _) ->
    Exp = lfe_macro:expand_form(lfe_eval:expr(S, Eenv), Eenv),
    {yes,Exp,Eenv}.

%% set(Args, EvalEnv, BaseEnv) -> {yes,Result,Env} | no.

set([Pat,Exp], Eenv, _) ->
    Val = lfe_eval:expr(Exp, Eenv),		%Evaluate expression
    case lfe_eval:match(Pat, Val, Eenv) of
	{yes,Bs} ->
	    Env1 = foldl(fun ({N,V}, E) -> add_upd_vbinding(N, V, E) end,
			 Eenv, Bs),
	    {yes,Val,Env1};
	no -> erlang:error({badmatch,Val})
    end;
%% set([Pat,['when',G],Exp], Eenv, _) ->
set(_, _, _) -> no.

%% slurp(File, EvalEnv, BaseEnv) -> {yes,{mod,Mod},Env} | no.
%%  Load in a file making all functions available. The module is
%%  loaded in an empty environment and that environment is finally
%%  added to the standard base environment.

-record(slurp, {mod,imps=[]}).			%For slurping

slurp([File], Eenv, Benv) ->
    Name = lfe_eval:expr(File, Eenv),		%Get file name
    {ok,Fs0} = lfe_io:parse_file(Name),
    St0 = #slurp{mod='-no-mod-',imps=[]},
    {Fs1,Fenv0} = lfe_macro:macro_forms(Fs0, new_env()),
    {Fbs,St1} = lfe_lib:proc_forms(fun collect_form/3, Fs1, St0),
    %% Add imports to environment.
    Fenv1 = foldl(fun ({M,Is}, Env) ->
			  foldl(fun ({{F,A},R}, E) ->
					add_ibinding(M, F, A, R, E)
				end, Env, Is)
		  end, Fenv0, St1#slurp.imps),
    %% Get a new environment with all functions defined.
    Fenv2 = lfe_eval:make_letrec_env(Fbs, Fenv1),
    {yes,{ok,St1#slurp.mod},add_env(Fenv2, Benv)};
slurp(_, _, _) -> no.

collect_form(['define-module',Mod|Mdef], _, St0) when is_atom(Mod) ->
    St1 = collect_mdef(Mdef, St0),
    {[],St1#slurp{mod=Mod}};
collect_form(['define-function',F,[lambda,As|_]=Lambda], _, St)
  when is_atom(F) ->
    {[{F,length(As),Lambda}],St};
collect_form(['define-function',F,['match-lambda',[Pats|_]|_]=Match], _, St)
  when is_atom(F) ->
    {[{F,length(Pats),Match}],St};
collect_form(_, _, _) ->
    exit(unknown_form).

collect_mdef([[import|Imps]|Mdef], St) ->
    collect_mdef(Mdef, collect_imps(Imps, St));
collect_mdef([_|Mdef], St) ->
    %% Ignore everything else.
    collect_mdef(Mdef, St);
collect_mdef([], St) -> St.

collect_imps([['from',Mod|Fs]|Is], St0) when is_atom(Mod) ->
    St1 = collect_imp(fun ([F,A], Imps) when is_atom(F), is_integer(A) ->
			      store({F,A}, F, Imps)
		      end, Mod, St0, Fs),
    collect_imps(Is, St1);
collect_imps([['rename',Mod|Rs]|Is], St0) when is_atom(Mod) ->
    St1 = collect_imp(fun ([[F,A],R], Imps)
			  when is_atom(F), is_integer(A), is_atom(R) ->
			      store({F,A}, R, Imps)
		      end, Mod, St0, Rs),
    collect_imps(Is, St1);
collect_imps([], St) -> St.

collect_imp(Fun, Mod, St, Fs) ->
    Imps0 = safe_fetch(Mod, St#slurp.imps, []),
    Imps1 = foldl(Fun, Imps0, Fs),
    St#slurp{imps=store(Mod, Imps1, St#slurp.imps)}.

%% add_upd_vbinding(Name, Val, Env) -> Env.

add_upd_vbinding(N, V, Env) ->
    case is_vbound(N, Env) of
	true -> update_vbinding(N, V, Env);
	false -> add_vbinding(N, V, Env)
    end.

%% safe_fetch(Key, Dict, Default) -> Value.

safe_fetch(Key, D, Def) ->
    case find(Key, D) of
	{ok,Val} -> Val;
	error -> Def
    end.

%% (defsyntax safe_fetch
%%   ((key d def)
%%    (case (find key d)
%%      ((tuple 'ok val) val)
%%      ('error def))))
