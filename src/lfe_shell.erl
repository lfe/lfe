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
-export([c/1,c/2,ec/1,ec/2,i/0,i/1,l/1,m/0,m/1,pid/3,p/1,pp/1,regs/0]).

-import(lfe_env, [new/0,add_env/2,
                  add_vbinding/3,add_vbindings/2,is_vbound/2,get_vbinding/2,
                  fetch_vbinding/2,del_vbinding/2,
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
-record(state, {curr,save,base,             %Current, save and base env
                slurp=false}).              %Are we slurped?

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
    St = #state{curr=Base2,save=Base2,base=Base2,slurp=false},
    server_loop(St).

server_loop(St0) ->
    StX = try
              %% Read the form
              Prompt = prompt(),
              io:put_chars(Prompt),
              Form = lfe_io:read(),
              Ce1 = add_vbinding('-', Form, St0#state.curr),
              %% Macro expand and evaluate it.
              {Value,St1} = eval_form(Form, St0#state{curr=Ce1}),
              %% Print the result, but only to depth 30.
              VS = lfe_io:prettyprint1(Value, 30),
              io:requests([{put_chars,VS},nl]),
              %% Update bindings.
              Ce2 = update_shell_vars(Form, Value, St1#state.curr),
              %% lfe_io:prettyprint({Env1,Env2}), io:nl(),
              St1#state{curr=Ce2}
          catch
              %% Very naive error handling, just catch, report and
              %% ignore whole caboodle.
              Class:Error ->
                  %% Use LFE's simplified version of erlang shell's error
                  %% reporting but which LFE prettyprints data.
                  Stk = erlang:get_stacktrace(),
                  Sf = fun ({M,_F,_A}) ->    %Pre R15
                               %% Don't want to see these in stacktrace.
                               (M == lfe_eval) or (M == lfe_shell)
                                   or (M == lfe_macro) or (M == lists);
                           ({M,_F,_A,_L}) ->    %R15 and later
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
    Fs = [{i,0,[lambda,[],[':',lfe_shell,i]]},
          {i,1,[lambda,[ps],[':',lfe_shell,i,ps]]},
          %% {m,0,[lambda,[],[':',lfe_shell,m]]},
          %% {m,1,[lambda,[ms],[':',lfe_shell,m,ms]]},
          {pid,3,[lambda,[i,j,k],[':',lfe_shell,pid,i,j,k]]},
          {p,1,[lambda,[e],[':',lfe_shell,p,e]]},
          {pp,1,[lambda,[e],[':',lfe_shell,pp,e]]},
          {regs,0,[lambda,[],[':',lfe_shell,regs]]}
         ],
    Add = fun ({N,Ar,Def}, E) ->
                  lfe_eval:add_dynamic_func(N, Ar, Def, E)
          end,
    Env1 = foldl(Add, Env0, Fs),
    Env1.

add_shell_macros(Env0) ->
    %% We write macros in LFE and expand them with macro package.
    Ms = [{c,[lambda,[args,'$ENV'],?BQ([':',lfe_shell,c,?UQ_S(args)])]},
          {ec,[lambda,[args,'$ENV'],?BQ([':',lfe_shell,ec,?UQ_S(args)])]},
          {l,[lambda,[args,'$ENV'],?BQ([':',lfe_shell,l,[list|?UQ(args)]])]},
	  {m,['match-lambda',
	      [[[],'$ENV'],?BQ([':',lfe_shell,m])],
	      [[ms,'$ENV'],?BQ([':',lfe_shell,m,[list|?UQ(ms)]])]]}
         ],
    %% Any errors here will crash shell startup!
    Env1 = lfe_env:add_mbindings(Ms, Env0),
    %% io:fwrite("asm: ~p\n", [Env1]),
    Env1.

%% prompt() -> Prompt.

prompt() ->
    %% Don't bother flattening the list, no need.
    case is_alive() of
        true -> lfe_io:format("(~s)> ", [node()]);
        false -> "> "
    end.

%% eval_form(Form, State) -> {Value,State}.
%%  Macro expand the the top form then treat special case top-level
%%  forms.

eval_form(Form, #state{curr=Ce}=St) ->
    %% Flatten progn nested forms.
    case lfe_macro:macro_forms([{Form,1}], Ce) of
        {ok,Eforms,Ce1,Ws} ->
            list_warnings(Ws),
            St1 = St#state{curr=Ce1},
            %% lfe_io:format("~p\n", [Eforms]),
            foldl(fun ({F,_}, {_,S}) -> eval_form_1(F, S) end,
                  {[],St1}, Eforms);
        {error,Es,Ws} ->
            list_errors(Es),
            list_warnings(Ws),
            {error,St}
    end.

eval_form_1([set|Rest], St0) ->
    {Value,St1} = set(Rest, St0),
    {Value,St1};
eval_form_1([slurp|Args], St0) ->               %Slurp in a file
    {Value,St1} = slurp(Args, St0),
    {Value,St1};
eval_form_1([unslurp|_], St) ->
    %% Forget everything back to before current slurp.
    unslurp(St);
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
        {ok,Ws} -> list_warnings(Ws);
        {error,Es,Ws} ->
            list_errors(Es),
            list_warnings(Ws)
    end,
    set_1([Epat|Rest], St).

set_1([Pat,['when'|_]=G,Exp], #state{curr=Ce0}=St) ->
    Val = lfe_eval:expr(Exp, Ce0),              %Evaluate expression
    case lfe_eval:match_when(Pat, Val, [G], Ce0) of
        {yes,_,Bs} ->
            Ce1 = foldl(fun ({N,V}, E) -> add_vbinding(N, V, E) end,
                        Ce0, Bs),
            {Val,St#state{curr=Ce1}};
        no -> erlang:error({badmatch,Val})
    end;
set_1([Pat,Exp], #state{curr=Ce0}=St) ->
    Val = lfe_eval:expr(Exp, Ce0),              %Evaluate expression
    case lfe_eval:match(Pat, Val, Ce0) of
        {yes,Bs} ->
            Ce1 = foldl(fun ({N,V}, E) -> add_vbinding(N, V, E) end,
                        Ce0, Bs),
            {Val,St#state{curr=Ce1}};
        no -> erlang:error({badmatch,Val})
    end;
set_1(_, _) -> erlang:error({bad_form,'set'}).

%% unslurp(State) -> {ok,State}.
%% slurp(File, State) -> {{ok,Mod},State}.
%%  Load in a file making all functions available. The module is
%%  loaded in an empty environment and that environment is finally
%%  added to the standard current environment. We could not use the
%%  compiler here as we need the macro environment.

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
    {ok,#state{curr=Ce0}=St1} = unslurp(St0),   %Reset teh environment
    Name = lfe_eval:expr(File, Ce0),            %Get file name
    case slurp_1(Name, Ce0) of
        {ok,Mod,Ce1} ->                         %Set the new environment
            {{ok,Mod},St1#state{save=Ce0,curr=Ce1,slurp=true}};
        error ->
            {error,St1}
    end.

slurp_1(Name, Ce) ->
    case slurp_file(Name) of                    %Parse, expand and lint file
        {ok,Fs,Fenv0,Ws} ->
            slurp_warnings(Name, Ws),
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
            {ok,Sl1#slurp.mod,add_env(Fenv2, Ce)};
        {error,Es,Ws} ->
            slurp_errors(Name, Es),
            slurp_warnings(Name, Ws),
            error
    end.

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

%% c(File [,Args]) -> {ok,Module} | error.
%%  Compile and load an LFE file.

c(F) -> c(F, []).

c(F, Os0) ->
    Os1 = [report,verbose|Os0],                 %Always report verbosely
    Loadm = fun ([]) -> {module,[]};
                (Mod) ->
                    Base = filename:basename(F, ".lfe"),
                    code:purge(Mod),
                    R = code:load_abs(Base),
                    R
            end,
    case lfe_comp:file(F, Os1) of
        {ok,Mod,_} -> Loadm(Mod);
        {ok,Mod} -> Loadm(Mod);
        Other -> Other
    end.

%% ec(File [,Args]) -> Res.
%%  Compile and load an Erlang file.

ec(F) -> c:c(F).

ec(F, Os) -> c:c(F, Os).

%% i([Pids]) -> ok.

i() -> c:i().

i(Pids) -> c:i(Pids).

%% l(Modules) -> ok.
%%  Load the modules.

l(Ms) ->
    foreach(fun (M) -> c:l(M) end, Ms).

%% m([Modules]) -> ok.
%%  Print module information.

m() -> c:m().

m(Ms) ->
    foreach(fun (M) -> c:m(M) end, Ms).

%% pid(A, B, C) -> Pid.

pid(A, B, C) -> c:pid(A, B, C).

%% regs() -> ok.

regs() -> c:regs().
