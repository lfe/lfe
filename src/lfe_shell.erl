%% Copyright (c) 2008-2014 Robert Virding
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

%% The shell commands which generally callable.
-export([p/1,pp/1,m/1,l/1,c/1,ec/1]).

-import(lfe_env, [new/0,add_env/2,
		  add_vbinding/3,add_vbindings/2,is_vbound/2,get_vbinding/2,
		  fetch_vbinding/2,update_vbinding/3,del_vbinding/2,
		  add_fbinding/4,add_fbindings/3,get_fbinding/3,add_ibinding/5,
		  get_gbinding/3,add_mbinding/3]).

-import(orddict, [store/3,find/2]).
-import(ordsets, [add_element/2]).
-import(lists, [map/2,foreach/2,foldl/3]).

%% -compile([export_all]).

%% We do a lot of quoting!
-define(Q(E), [quote,E]).
-define(BQ(E), [backquote,E]).
-define(UQ(E), [unquote,E]).
-define(UQ_S(E), ['unquote-splicing',E]).

%% Shell state.
-record(state, {curr,local,base}).		%Current, local and base env

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
    Base0 = add_shell_functions(lfe_env:new()),
    Base1 = add_shell_macros(Base0),
    Base2 = add_shell_vars(Base1),
    St = #state{curr=Base2,local=Base2,base=Base2},
    server_loop(St).

server_loop(St0) ->
    StX = try
	      %% Read the form
	      Prompt = prompt(),
	      io:put_chars(Prompt),
	      Form = lfe_io:read(),
	      Ee1 = update_vbinding('-', Form, St0#state.curr),
	      %% Macro expand and evaluate it.
	      {Value,St1} = eval_form(Form, St0#state{curr=Ee1}),
	      %% Print the result, but only to depth 30.
	      VS = lfe_io:prettyprint1(Value, 30),
	      io:requests([{put_chars,VS},nl]),
	      %% Update bindings.
	      Ee2 = update_shell_vars(Form, Value, St1#state.curr),
	      %% lfe_io:prettyprint({Env1,Env2}), io:nl(),
	      St1#state{curr=Ee2}
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

add_shell_functions(Env0) ->
    Fs = [{p,1,[lambda,[e],[':',lfe_shell,p,e]]},
	  {pp,1,[lambda,[e],[':',lfe_shell,pp,e]]},
	  {i,0,[lambda,[],[':',c,i]]},
	  {i,1,[lambda,[ps],[':',c,i,ps]]},
	  {regs,0,[lambda,[],[':',c,regs]]}
	 ],
    Add = fun ({N,Ar,Def}, E) ->
		  lfe_env:add_fbinding(N, Ar, {lexical_expr,Def,Env0}, E)
	  end,
    Env1 = foldl(Add, Env0, Fs),
    Env1.

add_shell_macros(Env0) ->
    %% We write macros in LFE and expand them with macro package.
    Ms = [{m,[lambda,[args,'$ENV'],?BQ([':',lfe_shell,m,[list|?UQ(args)]])]},
	  {l,[lambda,[args,'$ENV'],?BQ([':',lfe_shell,l,[list|?UQ(args)]])]},
	  {c,[lambda,[args,'$ENV'],?BQ([':',lfe_shell,c,[list|?UQ(args)]])]},
	  {ec,[lambda,[args,'$ENV'],?BQ([':',lfe_shell,ec,[list|?UQ(args)]])]}
	 ],
    %% Any errors here will crash shell startup!
    Env1 = lfe_env:add_mbindings(Ms, Env0),
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

eval_form(Form, #state{curr=Ce}=St) ->
    eval_exp_form(lfe_macro:expand_expr_all(Form, Ce), St).

eval_exp_form([set,Pat,Expr], #state{curr=Ce}=St0) ->
    %% Special case to lint pattern.
    case lfe_lint:pattern(Pat, Ce) of
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
	    {lfe_eval:expr(Eform, St0#state.curr),St0}
    end.

list_errors(Es) -> list_ews("~w: ~s\n", Es).

list_warnings(Ws) -> list_ews("~w: Warning: ~s\n", Ws).

list_ews(Format, Ews) ->
    foreach(fun ({L,M,E}) ->
		    Cs = M:format_error(E),
		    lfe_io:format(Format, [L,Cs])
	    end, Ews).

%% eval_internal(Form, State) -> {yes,Value,State} | no.
%%  Check for and evaluate internal forms. These are tested before any
%%  functions and all evaluate their arguments.

eval_internal([slurp|Args], St) ->		%Slurp in a file
    slurp(Args, St);
eval_internal([unslurp|_], St) ->		%Forget everything
    Be = St#state.base,
    {yes,ok,St#state{curr=Be}};
eval_internal([set|Args], St) ->		%Set variables in shell
    set(Args, St);
eval_internal(_, _) -> no.			%Not an internal function

%% set(Args, State) -> {yes,Result,State} | no.

set([Pat,Exp], #state{curr=Ce0}=St) ->
    Val = lfe_eval:expr(Exp, Ce0),		%Evaluate expression
    case lfe_eval:match(Pat, Val, Ce0) of
	{yes,Bs} ->
	    Ce1 = foldl(fun ({N,V}, E) -> add_upd_vbinding(N, V, E) end,
			Ce0, Bs),
	    {yes,Val,St#state{curr=Ce1}};
	no -> erlang:error({badmatch,Val})
    end;
set([Pat,['when',G],Exp], #state{curr=Ce0}=St) ->
    Val = lfe_eval:expr(Exp, Ce0),
    case lfe_eval:match_when(Pat, Val, [['when'|G]], Ce0) of
	{yes,_,Bs} ->
	    Ce1 = foldl(fun ({N,V}, E) -> add_upd_vbinding(N, V, E) end,
			Ce0, Bs),
	    {yes,Val,St#state{curr=Ce1}};
	no -> erlang:error({badmatch,Val})
    end;
set(_, _) -> no.

%% slurp(File, State) -> {yes,{ok,Mod},State} | no.
%%  Load in a file making all functions available. The module is
%%  loaded in an empty environment and that environment is finally
%%  added to the standard base environment. We could not use the
%%  compiler here as we need the macro environment.

-record(slurp, {mod,imps=[]}).			%For slurping

slurp([File], #state{curr=Ee,base=Be}=St0) ->
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
	    St1 = St0#state{curr=add_env(Fenv2, Be)},
	    {yes,{ok,Sl1#slurp.mod},St1};
	{error,Es,_} ->
	    slurp_errors(Name, Es),
	    {yes,error,St0#state{curr=Be}}
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

%% The LFE shell command functions.
%%  These are callable from outside the shell as well.

p(E) ->
    Cs = lfe_io:print1(E),
    io:put_chars([Cs,$\n]).

pp(E) ->
    Cs = lfe_io:prettyprint1(E),
    io:put_chars([Cs,$\n]).

%% c(Args) -> Res,.
%%  Compile and load an LFE file.

c([F]) -> c([F,[]]);
c([F,Os]) ->
    Loadm = fun ([]) -> {module,[]};
		(Mod) ->
		    Base = filename:basename(F, ".lfe"),
		    code:purge(Mod),
		    R = code:load_abs(Base),
		    R
	    end,
    case lfe_comp:file(F, [report,verbose|Os]) of
	{ok,Mod,_} -> Loadm(Mod);
	{ok,Mod} -> Loadm(Mod);
	Other -> Other
    end.

%% ec(Args) -> Res.
%%  Compile and load an Erlang file.

ec([F]) -> ec([F,[]]);
ec([F,Os]) ->
    c:c(F, Os).

%% l(Modules) -> ok.
%%  Load the modules.

l(Ms) ->
    foreach(fun (M) -> c:l(M) end, Ms).

%% m(Modules) -> ok.
%%  Print module information.

m([]) -> c:m();
m(Ms) ->
    foreach(fun (M) -> c:m(M) end, Ms).
