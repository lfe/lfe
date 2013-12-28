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

%% File    : lfe_shell.erl
%% Author  : Robert Virding
%% Purpose : A simple Lisp Flavoured Erlang shell.

%% We keep two environments, the BaseEnv which contains the predefined
%% shell variables and shell macros, and a current environment. The
%% BaseEnv is used when we need to revert to an "empty" environment.
%% When we slurp in a file the functions are stored in the BaseEnv and
%% the current environment is thrown away.

-module(lfe_shell).

-export([start/0,start/1,server/0,server/1]).

-import(lfe_env, [new/0,add_env/2,
		  add_vbinding/3,add_vbindings/2,is_vbound/2,get_vbinding/2,
		  fetch_vbinding/2,update_vbinding/3,del_vbinding/2,
		  add_fbinding/4,add_fbindings/3,get_fbinding/3,add_ibinding/5,
		  get_gbinding/3,add_mbinding/3]).

-import(orddict, [store/3,find/2]).
-import(ordsets, [add_element/2]).
-import(lists, [map/2,foreach/2,foldl/3]).

%% -compile([export_all]).

%% Shell state.
-record(state, {env,base}).			%Current and base env

start() ->
    spawn(fun () -> server(default) end).

start(P) ->
    spawn(fun () -> server(P) end).

server() -> server(default).

server(_) ->
    io:fwrite("LFE Shell V~s (abort with ^G)\n",
	      [erlang:system_info(version)]),
    %% Create a default base env of predefined shell variables with
    %% default nil bindings and basic shell macros.
    Base0 = add_shell_macros(lfe_env:new()),
    Base1 = add_shell_vars(Base0),
    St = #state{env=Base1,base=Base1},
    server_loop(St).

server_loop(St0) ->
    StX = try
	      %% Read the form
	      Prompt = prompt(),
	      io:put_chars(Prompt),
	      Form = lfe_io:read(),
	      Ee1 = update_vbinding('-', Form, St0#state.env),
	      %% Macro expand and evaluate it.
	      {Value,St1} = eval_form(Form, St0#state{env=Ee1}),
	      %% Print the result, but only to depth 30.
	      VS = lfe_io:prettyprint1(Value, 30),
	      io:requests([{put_chars,VS},nl]),
	      %% Update bindings.
	      Ee2 = update_shell_vars(Form, Value, St1#state.env),
	      %% lfe_io:prettyprint({Env1,Env2}), io:nl(),
	      St1#state{env=Ee2}
	  catch
	      %% Very naive error handling, just catch, report and
	      %% ignore whole caboodle.
	      Class:Error ->
		  %% Use LFE's simplified version of erlang shell's error
		  %% reporting but which LFE prettyprints data.
		  Stk = erlang:get_stacktrace(),
		  Sf = fun ({M,_F,_A}) ->	%Pre R15
			       %% Don't want to see these in stacktrace.
			       (M == lfe_eval) or (M == lfe_shell)
				   or (M == lfe_macro) or (M == lists);
			   ({M,_F,_A,_L}) ->	%R15 and later
			       %% Don't want to see these in stacktrace.
			       (M == lfe_eval) or (M == lfe_shell)
				   or (M == lfe_macro) or (M == lists)
		       end,
		  Ff = fun (T, I) -> lfe_io:prettyprint1(T, 15, I, 80) end,
		  Cs = lfe_lib:format_exception(Class, Error, Stk, Sf, Ff, 1),
		  io:put_chars(Cs),
		  io:nl(),
		  %% lfe_io:prettyprint({'EXIT',Class,Error,St}), io:nl(),
		  St0
	  end,
    server_loop(StX).

add_shell_vars(Env0) ->
    %% Add default shell expression variables.
    Env1 = foldl(fun (Symb, E) -> add_vbinding(Symb, [], E) end, Env0,
		 ['+','++','+++','-','*','**','***']),
    add_vbinding('$ENV', Env1, Env1).		%This gets it all

update_shell_vars(Form, Value, Env0) ->
    Env1 = foldl(fun ({Symb,Val}, E) -> update_vbinding(Symb, Val, E) end,
		 Env0,
		 [{'+++',fetch_vbinding('++', Env0)},
		  {'++',fetch_vbinding('+', Env0)},
		  {'+',Form},
		  {'***',fetch_vbinding('**', Env0)},
		  {'**',fetch_vbinding('*', Env0)},
		  {'*',Value}]),
    %% Be cunning with $ENV, remove self references so it doesn't grow
    %% indefinitely.
    Env2 = del_vbinding('$ENV', Env1),
    add_vbinding('$ENV', Env2, Env2).

add_shell_macros(Env0) ->
    %% We write macros in LFE and expand them with macro package.
    Ms = [
	 ],
    %% Any errors here will crash shell startup!
    {ok,_,Env1,_} = lfe_macro:macro_forms(Ms, Env0),
    %% io:fwrite("asm: ~p\n", [Env1]),
    Env1.

%% prompt() -> Prompt.

prompt() ->
    %% Don't bother flattening the list, no need.
    case is_alive() of
	true -> io_lib:format("(~s)> ", [node()]);
	false -> "> "
    end.

%% eval_form(Form, State) -> {Value,State}.

eval_form(Form, #state{env=Ee}=St) ->
    eval_exp_form(lfe_macro:expand_expr_all(Form, Ee), St).

eval_exp_form([set,Pat,Expr], #state{env=Ee}=St0) ->
    %% Special case to lint pattern.
    case lfe_lint:pattern(Pat, Ee) of
	{ok,Ws} -> list_warnings(Ws);
	{error,Es,Ws} ->
	    list_errors(Es),
	    list_warnings(Ws)
    end,
    {yes,Value,St1} = set([Pat,Expr], St0),
    {Value,St1};
eval_exp_form(Eform, St0) ->
    case eval_internal(Eform, St0) of
	{yes,Value,St1} -> {Value,St1};
	no ->
	    %% Normal evaluation of form.
	    {lfe_eval:expr(Eform, St0#state.env),St0}
    end.

list_errors(Es) ->
    foreach(fun ({L,M,E}) ->
		    Cs = M:format_error(E),
		    lfe_io:format("~w: ~s\n", [L,Cs])
	    end, Es).

list_warnings(Ws) ->
    foreach(fun ({L,M,W}) ->
		    Cs = M:format_error(W),
		    lfe_io:format("~w: Warning: ~s\n", [L,Cs])
	    end, Ws).

%% eval_internal(Form, State) -> {yes,Value,State} | no.
%%  Check for and evaluate internal functions. These all evaluate
%%  their arguments.

eval_internal([p|Args], St) ->			%Print out a value
    p(Args, St);
eval_internal([pp|Args], St) ->			%Prettyprint out a value
    pp(Args, St);
eval_internal([slurp|Args], St) ->		%Slurp in a file
    slurp(Args, St);
eval_internal([unslurp|_], St) ->		%Forget everything
    Be = St#state.base,
    {yes,ok,St#state{env=Be}};
eval_internal([c|Args], St) ->			%Compile an LFE file
    c(Args, St);
eval_internal([ec|Args], St) ->			%Compile an Erlang file
    ec(Args, St);
eval_internal([l|Args], St) ->			%Load modules
    l(Args, St);
eval_internal([m|Args], St) ->			%Module info
    m(Args, St);
eval_internal([set|Args], St) ->		%Set variables in shell
    set(Args, St);
eval_internal(_, _) -> no.			%Not an internal function

%% p([Arg], State) -> {yes,Res,State} | no.
%% pp([Arg], State) -> {yes,Res,State} | no.

p([A], St) ->
    E = lfe_eval:expr(A, St#state.env),
    Cs = lfe_io:print1(E),
    {yes,io:put_chars([Cs,$\n]),St};
p(_, _) -> no.

pp([A], St) ->
    E = lfe_eval:expr(A, St#state.env),
    Cs = lfe_io:prettyprint1(E),
    {yes,io:put_chars([Cs,$\n]),St};
pp(_, _) -> no.

%% c(Args, State) -> {yes,Res,State}.
%%  Compile and load an LFE file.

c([F], St) -> c([F,[]], St);
c([F,Os], #state{env=Ee}=St) ->
    Name = lfe_eval:expr(F, Ee),		%Evaluate arguments
    Opts = lfe_eval:expr(Os, Ee),
    Loadm = fun ([]) -> {yes,{module,[]},St};
		(Mod) ->
		    Base = filename:basename(Name, ".lfe"),
		    code:purge(Mod),
		    R = code:load_abs(Base),
		    {yes,R,St}
	    end,
    case lfe_comp:file(Name, [report,verbose|Opts]) of
	{ok,Mod,_} -> Loadm(Mod);
	{ok,Mod} -> Loadm(Mod);
	Other -> {yes,Other,St}
    end;
c(_, _) -> no.					%Unknown function,

%% ec(Args, State) -> {yes,Res,State}.
%%  Compile and load an Erlang file.

ec([F], St) -> ec([F,[]], St);
ec([F,Os], #state{env=Ee}=St) ->
    Name = lfe_eval:expr(F, Ee),		%Evaluate arguments
    Opts = lfe_eval:expr(Os, Ee),
    {yes,c:c(Name, Opts),St};
ec(_, _) -> no.					%Unknown function

%% l(Args, State) -> {yes,Res,State}.
%%  Load the modules in Args.

l(Args, #state{env=Ee}=St) ->
    {yes,map(fun (M) -> c:l(lfe_eval:expr(M, Ee)) end, Args), St}.

%% m(Args, State) -> {yes,Res,State}.
%%  Module info.

m([], St) -> {yes,c:m(),St};
m(Args, #state{env=Ee}=St) ->
    {yes,map(fun (M) -> c:m(lfe_eval:expr(M, Ee)) end, Args), St}.

%% set(Args, State) -> {yes,Result,State} | no.

set([Pat,Exp], #state{env=Ee0}=St0) ->
    Val = lfe_eval:expr(Exp, Ee0),		%Evaluate expression
    case lfe_eval:match(Pat, Val, Ee0) of
	{yes,Bs} ->
	    Ee1 = foldl(fun ({N,V}, E) -> add_upd_vbinding(N, V, E) end,
			Ee0, Bs),
	    {yes,Val,St0#state{env=Ee1}};
	no -> erlang:error({badmatch,Val})
    end;
%% set([Pat,['when',G],Exp], St) ->
set(_, _) -> no.

%% slurp(File, State) -> {yes,{ok,Mod},State} | no.
%%  Load in a file making all functions available. The module is
%%  loaded in an empty environment and that environment is finally
%%  added to the standard base environment. We could not use the
%%  compiler here as we need the macro environment.

-record(slurp, {mod,imps=[]}).			%For slurping

slurp([File], #state{env=Ee,base=Be}=St0) ->
    Name = lfe_eval:expr(File, Ee),		%Get file name
    case slurp_file(Name) of			%Parse, expand and lint file
	{ok,Fs,Fenv0,_} ->
	    Sl0 = #slurp{mod='-no-mod-',imps=[]},
	    {Fbs,Sl1} = lfe_lib:proc_forms(fun collect_form/3, Fs, Sl0),
	    %% Add imports to environment.
	    Fenv1 = foldl(fun ({M,Is}, Env) ->
				  foldl(fun ({{F,A},R}, E) ->
						add_ibinding(M, F, A, R, E)
					end, Env, Is)
			  end, Fenv0, Sl1#slurp.imps),
	    %% Get a new environment with all functions defined.
	    Fenv2 = lfe_eval:make_letrec_env(Fbs, Fenv1),
	    St1 = St0#state{env=add_env(Fenv2, Be)},
	    {yes,{ok,Sl1#slurp.mod},St1};
	{error,Es,_} ->
	    slurp_errors(Name, Es),
	    {yes,error,St0#state{env=Be}}
    end;
slurp(_, _) -> no.

slurp_file(Name) ->
    %% Parse, expand macros and lint file.
    case lfe_io:parse_file(Name) of
	{ok,Fs0} ->
	    case lfe_macro:expand_forms(Fs0, lfe_env:new()) of
		{ok,Fs1,Fenv,_} ->
		    case lfe_lint:module(Fs1, []) of
			{ok,Ws} -> {ok,Fs1,Fenv,Ws};
			{error,_,_}=Error -> Error
		    end;
		{error,_,_}=Error -> Error
	    end;
	{error,E} -> {error,[E],[]}
    end.

slurp_errors(F, Es) ->
    %% Directly from lfe_comp.
    foreach(fun ({Line,Mod,Error}) ->
		    Cs = Mod:format_error(Error),
		    lfe_io:format("~s:~w: ~s\n", [F,Line,Cs]);
		({Mod,Error}) ->
		    Cs = Mod:format_error(Error),
		    lfe_io:format("~s: ~s\n", [F,Cs])
	    end, Es).

collect_form(['define-module',Mod|Mdef], _, St0) ->
    St1 = collect_mdef(Mdef, St0),
    {[],St1#slurp{mod=Mod}};
collect_form(['extend-module'|Mdef], _, St0) ->
    St1 = collect_mdef(Mdef, St0),
    {[],St1};
collect_form(['define-function',F,[lambda,As|_]=Lambda], _, St) ->
    {[{F,length(As),Lambda}],St};
collect_form(['define-function',F,['match-lambda',[Pats|_]|_]=Match], _, St) ->
    {[{F,length(Pats),Match}],St}.

collect_mdef([[import|Is]|Mdef], St) ->
    collect_mdef(Mdef, collect_imps(Is, St));
collect_mdef([_|Mdef], St) ->			%Ignore everything else
    collect_mdef(Mdef, St);
collect_mdef([], St) -> St.

collect_imps(Is, St) ->
    foldl(fun (I, S) -> collect_imp(I, S) end, St, Is).

collect_imp(['from',Mod|Fs], St) ->
    collect_imp(fun ([F,A], Imps) -> store({F,A}, F, Imps) end,
		Mod, St, Fs);
collect_imp(['rename',Mod|Rs], St) ->
    collect_imp(fun ([[F,A],R], Imps) -> store({F,A}, R, Imps) end,
		Mod, St, Rs);
collect_imp(_, St) -> St.			%Ignore everything else

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

%% (defmacro safe_fetch (key d def)
%%   `(case (find ,key ,d)
%%      ((tuple 'ok val) val)
%%      ('error ,def)))
