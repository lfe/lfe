%% Copyright (c) 2008-2015 Robert Virding
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

%% We keep three environments: the current environment; the saved
%% environment which contains the environment from before a slurp; and
%% the base environment which contains the predefined shell variables,
%% functions and macros. The base environment is used when we need to
%% revert to an "empty" environment. The save environment is used to
%% store the current environment when we 'slurp' a file which we can
%% revert back to when we do an 'unslurp'.

-module(lfe_shell).

-export([start/0,start/1,server/0,server/1,
         run_script/2,run_script/3,run_string/2,run_string/3]).

%% The shell commands which generally callable.
-export([c/1,c/2,cd/1,ec/1,ec/2,help/0,i/0,i/1,l/1,ls/1,clear/0,m/0,m/1,
         pid/3,p/1,pp/1,pwd/0,q/0,flush/0,regs/0,exit/0]).

-import(lfe_env, [new/0,add_env/2,
                  add_vbinding/3,add_vbindings/2,is_vbound/2,get_vbinding/2,
                  fetch_vbinding/2,del_vbinding/2,
                  add_fbinding/4,add_fbindings/2,get_fbinding/3,add_ibinding/5,
                  get_gbinding/3,add_mbinding/3]).

-import(orddict, [store/3,find/2]).
-import(ordsets, [add_element/2]).
-import(lists, [reverse/1,foreach/2]).

-include("lfe.hrl").

%% -compile([export_all]).

%% Implement our own lists functions to get around stacktrace printing
%% problems.

foldl(F, Accu, [Hd|Tail]) ->
    foldl(F, F(Hd, Accu), Tail);
foldl(F, Accu, []) when is_function(F, 2) -> Accu.

%% Shell state.
-record(state, {curr,save,base,             %Current, save and base env
                slurp=false}).              %Are we slurped?

run_script(File, Args) ->
    run_script(File, Args, lfe_env:new()).

run_script(File, Args, Env) ->
    St = new_state(File, Args, Env),
    run([File], St).

run_string(String, Args) ->
    run_string(String, Args, lfe_env:new()).

run_string(String, As, Env) ->
    St = new_state("lfe", As, Env),
    case read_script_string(String) of
        {ok,Forms} ->
            run_loop(Forms, [], St);
        {error,E} ->
            slurp_errors("lfe", [E]),
            {error,St}
    end.

start() -> start(default).

start(Env) ->
    spawn(fun () -> server(Env) end).

server() -> server(default).

server(default) ->
    server(lfe_env:new());
server(Env) ->
    process_flag(trap_exit, true),              %Must trap exists
    io:fwrite(get_banner()),
    %% Create a default base env of predefined shell variables with
    %% default nil bindings and basic shell macros.
    St = new_state("lfe", [], Env),
    %% Set shell io to use LFE expand in edlin, ignore error.
    io:setopts([{expand_fun,fun (B) -> lfe_edlin_expand:expand(B) end}]),
    Eval = start_eval(St),                      %Start an evaluator
    server_loop(Eval, St).                      %Run the loop

server_loop(Eval0, St0) ->
    %% Read the form
    Prompt = prompt(),
    {Ret,Eval1} = read_expression(Prompt, Eval0, St0),
    case Ret of
        {ok,Form} ->
            {Eval2,St1} = shell_eval(Form, Eval1, St0),
            server_loop(Eval2, St1);
        {error,E} ->
            list_errors([E]),
            server_loop(Eval1, St0)
    end.

%% shell_eval(From, Evaluator, State) -> {Evaluator,State}.
%%  Evaluate one shell expression. The evaluator may crash in which
%%  case we restart it and return the pid of the new evaluator.

shell_eval(Form, Eval0, St0) ->
    Eval0 ! {eval_expr,self(),Form},
    receive
        {eval_value,Eval0,_Value,St1} ->
            %% We actually don't use the returned value here.
            {Eval0,St1};
        {eval_error,Eval0,Class} ->
            %% Eval errored out, get the exit signal.
            receive {'EXIT',Eval0,{Reason,Stk}} -> ok end,
            report_exception(Class, Reason, Stk),
            Eval1 = start_eval(St0),
            {Eval1,St0};
        {'EXIT',Eval0,Error} ->
            %% Eval exited or was killed
            report_exception(error, Error, []),
            Eval1 = start_eval(St0),
            {Eval1,St0}
    end.

%% prompt() -> Prompt.

prompt() ->
    %% Don't bother flattening the list, no need.
    case is_alive() of
        true -> lfe_io:format1("(~s)> ", [node()]);
        false -> "> "
    end.

report_exception(Class, Reason, Stk) ->
    %% Use LFE's simplified version of erlang shell's error
    %% reporting but which LFE prettyprints data.
    Sf = fun ({M,_F,_A}) ->                     %Pre R15
                 %% Don't want to see these in stacktrace.
                 (M == lfe_eval) or (M == ?MODULE);
             ({M,_F,_A,_L}) ->                  %R15 and later
                 %% Don't want to see these in stacktrace.
                 (M == lfe_eval) or (M == ?MODULE)
         end,
    Ff = fun (T, I) -> lfe_io:prettyprint1(T, 15, I, 80) end,
    Cs = lfe_lib:format_exception(Class, Reason, Stk, Sf, Ff, 1),
    io:put_chars(Cs),
    io:nl().

%% read_expression(Prompt, Evaluator, State) -> {Return,Evaluator}.
%%  Start a reader process and wait for an expression. We are cunning
%%  here and just use the exit reason to pass the expression. We must
%%  also handle the evaluator dying and restart it.

read_expression(Prompt, Eval, St) ->
    Read = fun () ->
                   %% Ret = lfe_io:read(Prompt),
                   io:put_chars(Prompt),
                   Ret = lfe_io:read(),
                   exit(Ret)
           end,
    Rdr = spawn_link(Read),
    read_expression_1(Rdr, Eval, St).

read_expression_1(Rdr, Eval, St) ->
    %% Eval is not doing anything here, so exits from it can only
    %% occur if it was terminated by a signal from another process.
    receive
        {'EXIT',Rdr,Ret} ->
            {Ret,Eval};
        {'EXIT',Eval,{Reason,Stk}} ->
            report_exception(error, Reason, Stk),
            read_expression_1(Rdr, start_eval(St), St);
        {'EXIT',Eval,Reason} ->
            report_exception(exit, Reason, []),
            read_expression_1(Rdr, start_eval(St), St)
    end.

get_banner() ->
    [io_lib:format("         (\n" ++
                   "     (    )  )\n" ++
                   "      )_.(._(\n" ++
                   "   .-(   \\\\  )-.       |   A Lisp-2+ on the Erlang VM\n" ++
                   "  (     / \\\\    )      |   Docs: http://docs.lfe.io/ \n" ++
                   "  |`-.._____..-';.     |   \n" ++
                   "  |         g  (_ \\    |   Type `(help)` for usage info.\n" ++
                   "  |        n    || |   |   \n" ++
                   "  |       a     '/ ;   |   Source code:\n" ++
                   "  (      l      / /    |   http://github.com/rvirding/lfe\n" ++
                   "   \\    r      ( /     |   \n" ++
                   "    \\  E      ,y'      |   LFE v~s\n" ++
                   "     `-.___.-'         |   LFE Shell V~s ~s\n\n",
            [get_lfe_version(),
             erlang:system_info(version),
             get_abort_message()])].

get_abort_message() ->
    %% We can update this later to check for env variable settings for
    %% shells that require a different control character to abort, such
    %% as jlfe.
    "(abort with G^)".

get_lfe_version() ->
    {ok, [App]} = file:consult(code:where_is_file("lfe.app")),
    proplists:get_value(vsn, element(3, App)).

%% new_state(ScriptName, Args [,Env]) -> State.
%%  Generate a new shell state with all the default functions, macros
%%  and variables.

%% new_state(Script, Args) -> new_state(Script, Args, lfe_env:new()).

new_state(Script, Args, Env0) ->
    Env1 = add_vbinding('script-name', Script, Env0),
    Env2 = add_vbinding('script-args', Args, Env1),
    Base0 = add_shell_functions(Env2),
    Base1 = add_shell_macros(Base0),
    Base2 = add_shell_vars(Base1),
    #state{curr=Base2,save=Base2,base=Base2,slurp=false}.

add_shell_vars(Env0) ->
    %% Add default shell expression variables.
    Env1 = foldl(fun (Symb, E) -> add_vbinding(Symb, [], E) end, Env0,
                 ['+','++','+++','-','*','**','***']),
    add_vbinding('$ENV', Env1, Env1).        %This gets it all

update_shell_vars(Form, Value, Env0) ->
    Env1 = foldl(fun ({Symb,Val}, E) -> add_vbinding(Symb, Val, E) end,
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
    Fs = [{cd,1,[lambda,[d],[':',lfe_shell,cd,d]]},
          {help,0,[lambda,[],[':',lfe_shell,help]]},
          {i,0,[lambda,[],[':',lfe_shell,i]]},
          {i,1,[lambda,[ps],[':',lfe_shell,i,ps]]},
          {clear,0,[lambda,[],[':',lfe_shell,clear]]},
          {pid,3,[lambda,[i,j,k],[':',lfe_shell,pid,i,j,k]]},
          {p,1,[lambda,[e],[':',lfe_shell,p,e]]},
          {pp,1,[lambda,[e],[':',lfe_shell,pp,e]]},
          {pwd,0,[lambda,[],[':',lfe_shell,pwd]]},
          {q,0,[lambda,[],[':',lfe_shell,exit]]},
          {flush,0,[lambda,[],[':',lfe_shell,flush]]},
          {regs,0,[lambda,[],[':',lfe_shell,regs]]},
          {exit,0,[lambda,[],[':',lfe_shell,exit]]}
         ],
    Add = fun ({N,Ar,Def}, E) ->
                  lfe_eval:add_dynamic_func(N, Ar, Def, E)
          end,
    Env1 = foldl(Add, Env0, Fs),
    Env1.

add_shell_macros(Env0) ->
    %% We write macros in LFE and expand them with macro package.
    Ms = [{c,[lambda,[args,'$ENV'],?BQ([':',lfe_shell,c,?C_A(args)])]},
          {ec,[lambda,[args,'$ENV'],?BQ([':',lfe_shell,ec,?C_A(args)])]},
          {l,[lambda,[args,'$ENV'],?BQ([':',lfe_shell,l,[list|?C(args)]])]},
          {ls,[lambda,[args,'$ENV'],?BQ([':',lfe_shell,ls,[list|?C(args)]])]},
          {m,['match-lambda',
              [[[],'$ENV'],?BQ([':',lfe_shell,m])],
              [[ms,'$ENV'],?BQ([':',lfe_shell,m,[list|?C(ms)]])]]}
         ],
    %% Any errors here will crash shell startup!
    Env1 = lfe_env:add_mbindings(Ms, Env0),
    %% io:fwrite("asm: ~p\n", [Env1]),
    Env1.

%% start_eval(State) -> Evaluator.
%%  Start an evaluator process.

start_eval(St) ->
    Self = self(),
    spawn_link(fun () -> eval_init(Self, St) end).

eval_init(Shell, St) ->
    eval_loop(Shell, St).

eval_loop(Shell, St0) ->
    receive
        {eval_expr,Shell,Form} ->
            St1 = eval_form(Form, Shell, St0),
            eval_loop(Shell, St1)
    end.

%% eval_form(Form, ShellPid, State) -> State.
%%  Evaluate a form, print is reurn value and send updated state back
%%  to the shell manager process. Unfortunately we can't just let it
%%  crash as an error here causes the emulator to generate an error
%%  report. Being cunning and building our own error return value and
%%  doing exit on it seem to fix the problem.

eval_form(Form, Shell, St0) ->
    try
        Ce1 = add_vbinding('-', Form, St0#state.curr),
        %% Macro expand and evaluate it.
        {Value,St1} = eval_form(Form, St0#state{curr=Ce1}),
        %% Print the result, but only to depth 30.
        VS = lfe_io:prettyprint1(Value, 30),
        io:requests([{put_chars,unicode,VS},nl]),
        %% Update bindings.
        Ce2 = update_shell_vars(Form, Value, St1#state.curr),
        St2 = St1#state{curr=Ce2},
        %% Return value and updated state.
        Shell ! {eval_value,self(),Value,St2},
        St2
    catch
        exit:normal -> exit(normal);
        Class:Reason ->
            Stk = erlang:get_stacktrace(),
            %% We don't want the ERROR REPORT generated by the
            %% emulator. Note: exit(kill) needs nothing special.
            Shell ! {eval_error,self(),Class},
            E = nocatch(Class, {Reason,Stk}),
            exit(E)
    end.

nocatch(throw, {Term,Stack}) ->
    {{nocatch,Term},Stack};
nocatch(_, Reason) -> Reason.

%% eval_form(Form, State) -> {Value,State}.
%%  Macro expand the the top form then treat special case top-level
%%  forms.

eval_form(Form, #state{curr=Ce}=St) ->
    %% Flatten progn nested forms.
    case lfe_macro:macro_forms([{Form,1}], Ce) of
        {ok,Eforms,Ce1,Ws} ->
            list_warnings(Ws),
            St1 = St#state{curr=Ce1},
            foldl(fun ({F,_}, {_,S}) -> eval_form_1(F, S) end,
                  {[],St1}, Eforms);
        {error,Es,Ws} ->
            list_errors(Es),
            list_warnings(Ws),
            {error,St}
    end.

eval_form_1([progn|Eforms], St) ->              %Top-level nested progn
    foldl(fun (F, {_,S}) -> eval_form_1(F, S) end,
          {[],St}, Eforms);
eval_form_1(['extend-module'|_], St) ->         %Maybe from macro expansion
    {[],St};
eval_form_1(['eval-when-compile'|_], St) ->	%Maybe from macro expansion
    %% We can happily ignore this.
    {[],St};
eval_form_1([set|Rest], St0) ->
    {Value,St1} = set(Rest, St0),
    {Value,St1};
eval_form_1([slurp|Args], St0) ->               %Slurp in a file
    {Value,St1} = slurp(Args, St0),
    {Value,St1};
eval_form_1([unslurp|_], St) ->
    %% Forget everything back to before current slurp.
    unslurp(St);
eval_form_1([run|Args], St0) ->
    {Value,St1} = run(Args, St0),
    {Value,St1};
eval_form_1(['define-function',Name,Def], #state{curr=Ce0}=St) ->
    Ar = function_arity(Def),
    Ce1 = lfe_eval:add_dynamic_func(Name, Ar, Def, Ce0),
    {Name,St#state{curr=Ce1}};
eval_form_1(['define-macro',Name,Def], #state{curr=Ce0}=St) ->
    Ce1 = add_mbinding(Name, Def, Ce0),
    {Name,St#state{curr=Ce1}};
eval_form_1(['reset-environment'], #state{base=Be}=St) ->
    {ok,St#state{curr=Be}};
eval_form_1(Expr, St) ->
    %% General case just evaluate the expression.
    {lfe_eval:expr(Expr, St#state.curr),St}.

function_arity([lambda,As|_]) ->
    length(As);
function_arity(['match-lambda',[Pats|_]|_]) ->
    length(Pats).

list_errors(Es) -> list_ews("~w: ~s\n", Es).

list_warnings(Ws) -> list_ews("~w: Warning: ~s\n", Ws).

list_ews(Format, Ews) ->
    foreach(fun ({L,M,E}) ->
                    Cs = M:format_error(E),
                    lfe_io:format(Format, [L,Cs])
            end, Ews).

%% set(Args, State) -> {Result,State}.

set([], St) -> {[],St};
set([Pat|Rest], #state{curr=Ce}=St) ->
    Epat = lfe_macro:expand_expr_all(Pat, Ce),  %Expand macros in pattern
    %% Special case to lint pattern.
    case lfe_lint:pattern(Epat, Ce) of
        {ok,_,Ws} -> list_warnings(Ws);
        {error,Es,Ws} ->
            list_errors(Es),
            list_warnings(Ws)
    end,
    set_1(Epat, Rest, St).

set_1(Pat, [['when'|_]=G,Exp], St) ->
    set_1(Pat, [G], Exp, St);                   %Just the guard
set_1(Pat, [Exp], St) ->
    set_1(Pat, [], Exp, St);                    %Empty body
set_1(_, _, _) -> erlang:error({bad_form,'set'}).

set_1(Pat, Guard, Exp, #state{curr=Ce0}=St) ->
    Val = lfe_eval:expr(Exp, Ce0),              %Evaluate expression
    case lfe_eval:match_when(Pat, Val, Guard, Ce0) of
        {yes,_,Bs} ->
            Ce1 = foldl(fun ({N,V}, E) -> add_vbinding(N, V, E) end,
                        Ce0, Bs),
            {Val,St#state{curr=Ce1}};
        no -> erlang:error({badmatch,Val})
    end.

%% unslurp(State) -> {ok,State}.
%% slurp(File, State) -> {{ok,Mod},State}.
%%  Load in a file making all functions available. We call the
%%  compiler directly and only add defined functions, macros are
%%  lost. Any exported macros will be available in the
%%  LFE-EXPAND-USER-MACRO/2 function.

-record(slurp, {mod,imps=[]}).                  %For slurping

unslurp(St0) ->
    St1 = case St0#state.slurp of
              true ->                           %Roll-back slurp
                  Se = St0#state.save,
                  St0#state{save=none,curr=Se,slurp=false};
              false -> St0                      %Do nothing
          end,
    {ok,St1}.

slurp([File], St0) ->
    {ok,#state{curr=Ce0}=St1} = unslurp(St0),   %Reset the environment
    Name = lfe_eval:expr(File, Ce0),            %Get file name
    case slurp_file(Name, Ce0) of
        {ok,Mod,Ce1} ->                         %Set the new environment
            {{ok,Mod},St1#state{save=Ce0,curr=Ce1,slurp=true}};
        error ->
            {error,St1}
    end.

slurp_file(Name, Ce0) ->
    case lfe_comp:file(Name, [binary,to_lint,return]) of
	{ok,[{ok,Mod,Fs,Mws}|_],Ws} ->		%Only do first module
	    slurp_warnings(Ws),
	    slurp_warnings(Mws),
	    Sl0 = #slurp{mod=Mod,imps=[]},
	    {Fbs,Sl1} = lfe_lib:proc_forms(fun collect_form/3, Fs, Sl0),
            %% Add imports to environment.
            Ce1 = foldl(fun ({M,Is}, Env) ->
				foldl(fun ({{F,A},R}, E) ->
					      add_ibinding(M, F, A, R, E)
				      end, Env, Is)
			end, Ce0, Sl1#slurp.imps),
	    %% Add functions to environment.
	    Ce2 = foldl(fun ({N,Ar,Def}, Env) ->
				lfe_eval:add_dynamic_func(N, Ar, Def, Env)
			end, Ce1, Fbs),
	    {ok,Mod,Ce2};
        {error,Mews,Es,Ws} ->
            slurp_errors(Es),
            slurp_warnings(Ws),
	    %% Now the errors and warnings for each module.
	    foreach(fun ({error,Mes,Mws}) ->
			    slurp_errors(Mes),
			    slurp_warnings(Mws)
		    end, Mews),
            error
    end.

slurp_errors(Errors) ->
    foreach(fun ({File,Es}) -> slurp_errors(File, Es) end, Errors).

slurp_warnings(Warnings) ->
    foreach(fun ({File,Ws}) -> slurp_warnings(File, Ws) end, Warnings).

slurp_errors(File, Es) -> slurp_ews(File, "~s:~w: ~s\n", Es).

slurp_warnings(File, Es) -> slurp_ews(File, "~s:~w: Warning: ~s\n", Es).

slurp_ews(File, Format, Ews) ->
    foreach(fun ({Line,Mod,Error}) ->
                    Cs = Mod:format_error(Error),
                    lfe_io:format(Format, [File,Line,Cs])
            end, Ews).

collect_form(['define-module',Mod|Mdef], _, St0) ->
    St1 = collect_mdef(Mdef, St0),
    {[],St1#slurp{mod=Mod}};
collect_form(['extend-module'|Mdef], _, St0) ->
    St1 = collect_mdef(Mdef, St0),
    {[],St1};
collect_form(['eval-when-compile'|_], _, St) ->
    %% Can safely ignore this, everything already in environment.
    {[],St};
collect_form(['define-function',F,Def], _, St) ->
    Ar = function_arity(Def),
    {[{F,Ar,Def}],St}.

collect_mdef([[import|Is]|Mdef], St) ->
    collect_mdef(Mdef, collect_imps(Is, St));
collect_mdef([_|Mdef], St) ->                   %Ignore everything else
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
collect_imp(_, St) -> St.                       %Ignore everything else

collect_imp(Fun, Mod, St, Fs) ->
    Imps0 = safe_fetch(Mod, St#slurp.imps, []),
    Imps1 = foldl(Fun, Imps0, Fs),
    St#slurp{imps=store(Mod, Imps1, St#slurp.imps)}.

%% run(Args, State) -> {Value,State}.
%%  Run the shell expressions in a file. Abort on errors and only
%%  return updated state if there are no errors.

run([File], #state{curr=Ce}=St) ->
    Name = lfe_eval:expr(File, Ce),             %Get file name
    case read_script_file(Name) of              %Read the file
        {ok,Forms} ->
            run_loop(Forms, [], St);
        {error,E} ->
            slurp_errors(Name, [E]),
            {error,St}
    end.

%% read_script_file(FileName) -> {ok,[Sexpr]} | {error,Error}.
%%  Read a file returning the sexprs. Almost the same as
%%  lfe_io:read_file except the we skip the first line if it is a
%%  script line "#! ... ".

read_script_file(File) ->
    case file:open(File, [read]) of
        {ok,F} ->
            %% Check if first a script line, if so skip it.
            case io:get_line(F, '') of
                "#!" ++ _ -> ok;
                _ -> file:position(F, bof)      %Reset to start of file
            end,
            Ret = case io:request(F, {get_until,'',lfe_scan,tokens,[1]}) of
                      {ok,Ts,Lline} -> parse_tokens(Ts, Lline, []);
                      {error,Error,_} -> {error,Error}
                  end,
            file:close(F),                      %Close the file
            Ret;
        {error,Error} -> {error,{none,file,Error}}
    end.

%% read_script_string(FileName) -> {ok,[Sexpr]} | {error,Error}.
%%  Read a file returning the sexprs. Almost the same as
%%  lfe_io:read_string except parse all forms.

read_script_string(String) ->
    case lfe_scan:string(String, 1) of
        {ok,Ts,Lline} -> parse_tokens(Ts, Lline, []);
        {error,E,_} -> {error,E}
    end.

parse_tokens([_|_]=Ts0, Lline, Ss) ->
    case lfe_parse:sexpr(Ts0) of
        {ok,_,S,Ts1} -> parse_tokens(Ts1, Lline, [S|Ss]);
        {more,Pc1} ->
            %% Need more tokens but there are none, so call again to
            %% generate an error message.
            {error,E,_} = lfe_parse:sexpr(Pc1, {eof,Lline}),
            {error,E};
        {error,E,_} -> {error,E}
    end;
parse_tokens([], _, Ss) -> {ok,reverse(Ss)}.

run_loop([F|Fs], _, St0) ->
    Ce1 = add_vbinding('-', F, St0#state.curr),
    {Value,St1} = eval_form(F, St0#state{curr=Ce1}),
    Ce2 = update_shell_vars(F, Value, St1#state.curr),
    run_loop(Fs, Value, St1#state{curr=Ce2});
run_loop([], Value, St) -> {Value,St}.

%% safe_fetch(Key, Dict, Default) -> Value.

safe_fetch(Key, D, Def) ->
    case find(Key, D) of
        {ok,Val} -> Val;
        error -> Def
    end.

%% The LFE shell command functions.
%%  These are callable from outside the shell as well.

%% c(File [,Args]) -> {ok,Module} | error.
%%  Compile and load an LFE file.

c(File) -> c(File, []).

c(File, Opts0) ->
    Opts1 = [report,verbose|Opts0],             %Always report verbosely
    case lfe_comp:file(File, Opts1) of
        Ok when element(1, Ok) =:= ok ->        %Compilation successful
            Return = lists:member(return, Opts1),
            Binary = lists:member(binary, Opts1),
            OutDir = outdir(Opts1),
            load_files(Ok, Return, Binary, OutDir);
        Error -> Error
    end.

load_files(Ok, _, true, _) -> Ok;               %Binary output
load_files(Ok, Ret, _, Out) ->                  %Beam files created.
    Mods = element(2, Ok),
    lists:map(fun (M) -> load_file(M, Ret, Out) end, Mods).

load_file(Ok, _, Out) ->
    case element(2, Ok) of
        [] -> Ok;                               %No module file to load
        Mod ->                                  %We have a module name
            Bfile = filename:join(Out, atom_to_list(Mod)),
            code:purge(Mod),
            code:load_abs(Bfile, Mod)           %Undocumented
    end.

outdir([{outdir,Dir}|_]) -> Dir;                %Erlang way
outdir([[outdir,Dir]|_]) -> Dir;                %LFE way
outdir([_|Opts]) -> outdir(Opts);
outdir([]) -> ".".

%% cd(Dir) -> ok.

cd(Dir) -> c:cd(Dir).

%% ec(File [,Args]) -> Res.
%%  Compile and load an Erlang file.

ec(F) -> c:c(F).

ec(F, Os) -> c:c(F, Os).

%% help() -> ok.

help() ->
    io:put_chars(<<"\nLFE shell built-in functions\n\n"
                   "(c file)    -- compile and load code in <file>\n"
                   "(cd dir)    -- change working directory to <dir>\n"
                   "(ec file)   -- compile and load code in erlang <file>\n"
                   "(exit)      -- quit - an alias for (q)\n"
                   "(help)      -- help info\n"
                   "(i)         -- information about the system\n"
                   "(l module)  -- load or reload <module>\n"
                   "(ls)        -- list files in the current directory\n"
                   "(clear)     -- clear the the REPL output\n"
                   "(ls dir)    -- list files in directory <dir>\n"
                   "(m)         -- which modules are loaded\n"
                   "(m mod)     -- information about module <mod>\n"
                   "(pid x y z) -- convert <x>, <y> and <z> to a pid\n"
                   "(pwd)       -- print working directory\n"
                   "(q)         -- quit - shorthand for init:stop/0\n"
                   "(flush)     -- flushes all messages sent to the shell\n"
                   "(regs)      -- information about registered processes\n\n"
                   "LFE shell built-in commands\n\n"
                   "(reset-environment)             -- resets the environment to its initial state\n"
                   "(set pattern expr)\n"
                   "(set pattern (when guard) expr) -- evaluate <expr> and match the result with\n"
                   "                                   pattern binding\n"
                   "(slurp file)                    -- slurp in a LFE source <file> and makes\n"
                   "                                   everything available in the shell\n"
                   "(unslurp)                       -- revert back to the state before the last\n"
                   "                                   slurp\n"
                   "(run file)                      -- execute all the shell commands in a <file>\n\n"
                   "LFE shell built-in variables\n\n"
                   "+/++/+++      -- the tree previous expressions\n"
                   "*/**/***      -- the values of the previous expressions\n"
                   "-             -- the current expression output\n"
                   "$ENV          -- the current LFE environment\n\n"
                 >>).

%% i([Pids]) -> ok.

i() -> c:i().

i(Pids) -> c:i(Pids).

%% l(Modules) -> ok.
%%  Load the modules.

l(Ms) ->
    foreach(fun (M) -> c:l(M) end, Ms).

%% ls(Dir) -> ok.

ls(Dir) -> apply(c, ls, Dir).

%% clear() -> ok.

clear() -> io:format("\e[H\e[J").

%% m([Modules]) -> ok.
%%  Print module information.

m() -> c:m().

m(Ms) ->
    foreach(fun (M) -> c:m(M) end, Ms).

%% p(Expr) -> ok.
%% pp(Expr) -> ok.
%%  Print/prettyprint a value.

p(E) ->
    Cs = lfe_io:print1(E),
    io:put_chars([Cs,$\n]).

pp(E) ->
    Cs = lfe_io:prettyprint1(E),
    io:put_chars([Cs,$\n]).

%% pid(A, B, C) -> Pid.
%%  Build a pid from its 3 "parts".

pid(A, B, C) -> c:pid(A, B, C).

%% pwd() -> ok.

pwd() -> c:pwd().

%% q() -> ok.

q() -> c:q().

%% flush() -> ok.

flush() -> c:flush().

%% regs() -> ok.

regs() -> c:regs().

%% exit() -> ok.

exit() -> c:q().
