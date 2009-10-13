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

%%% File    : lfe_macro.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang macro expander.

%%% Expand macros and record definitions (into macros), also handles
%%% quasiquote/backquote in an R6RS compatible way.

-module(lfe_macro).

-export([expand_form/1,expand_form/2,expand_forms/1,expand_forms/2,
	 macro_forms/2,
	 default_exps/0,bq_expand/1]).

-export([mbe_syntax_rules_proc/4,mbe_syntax_rules_proc/5,
	 mbe_match_pat/3,mbe_get_bindings/3,mbe_expand_pattern/3]).

-export([expand_macro/2,expand_macro_1/2]).

%% -compile([export_all]).

-import(lfe_lib, [new_env/0,add_fbinding/4,is_fbound/3,
		  add_mbinding/3,is_mbound/2,get_mbinding/2,
		  is_symb_list/1,is_proper_list/1]).

-import(lists, [any/2,all/2,map/2,foldl/3,foldr/3,mapfoldl/3,
		reverse/1,reverse/2,member/2,concat/1]).
-import(orddict, [find/2,store/3]).
-import(ordsets, [add_element/2,is_element/2]).

-define(Q(E), [quote,E]).			%We do a lot of quoting!

-record(mac, {expand=true,			%Expand everything
	      vc=0,				%Variable counter
	      fc=0}).				%Function counter

default_exps() -> new_env().

%% expand_form(Form) -> Form.
%% expand_form(Form, Env) -> Form.
%%  The first one is for users who do not expect themselves to handle
%%  macros.

expand_form(F) ->
    expand_form(F, default_exps()).

expand_form(F, Env) ->
    {Ef,_} = expand(F, Env, #mac{expand=true}),
    Ef.

%% expand_forms(FileForms) -> FileForms.
%% expand_forms(FileForms, Env) -> {FileForms,Env}.
%%  Expand forms in "file format", {Form,LineNumber}. When we pass an
%%  environment in we get back an updated one.

expand_forms(Fs0) ->
    {Fs1,_} = expand_forms(Fs0, default_exps()),
    Fs1.

expand_forms(Fs0, Env0) ->
    {Fs1,Env1,_} = pass(Fs0, Env0, #mac{expand=true}),
    {Fs1,Env1}.

%% macro_forms(FileForms, Env) -> {FileForms,Env}.
%%  Collect, and remove, all macro definitions in a list of forms. All
%%  top level macro calls are also expanded and any new macro
%%  definitions are collected.

macro_forms(Fs0, Env0) ->
    {Fs1,Env1,_} = pass(Fs0, Env0, #mac{expand=false}),
    {Fs1,Env1}.

%% pass(FileForms, Env, State) -> {FileForms,Env,State}.
%%  Pass over a list of fileforms, {Form,Line}, collecting and
%%  removing all macro defintions. All forms must be expanded at
%%  top-level to check form, but all can be expanded to full depth.
%%  Nesting of forms by progn is preserved.

pass([{['progn'|Pfs0],L}|Fs0], Env0, St0) ->
    {Pfs1,Env1,St1} = pass_progn(Pfs0, L, Env0, St0),
    {Fs1,Env2,St2} = pass(Fs0, Env1, St1),
    {[{['progn'|Pfs1],L}|Fs1],Env2,St2};
pass([{['eval-when-compile'|Ewcs0],L}|Fs0], Env0, St0) ->
    {Ecws1,Env1,St1} = pass_ewc(Ewcs0, L, Env0, St0),
    {Fs1,Env2,St2} = pass(Fs0, Env1, St1),
    {[{['progn'|Ecws1],L}|Fs1],Env2,St2};
pass([{['define-macro'|Def]=F,L}|Fs0], Env0, St0) ->
    case def_macro(Def, Env0, St0) of
	{yes,Env1,St1} -> pass(Fs0, Env1, St1);
	no ->
	    %% Ignore it and pass it on to generate error later.
	    {Fs1,Env1,St1} = pass(Fs0, Env0, St0),
	    {[{F,L}|Fs1],Env1,St1}
    end;
pass([{F,L}|Fs0], Env0, St0) ->
    %% First expand enough to test top form, if so process again.
    case expand_macro(F, Env0) of
	{yes,Exp} -> pass([{Exp,L}|Fs0], Env0, St0);
	no -> 
	    %% Expand all if flag set.
	    {F1,St1} = if St0#mac.expand -> expand(F, Env0, St0);
			  true -> {F,St0}
		       end,
	    {Fs1,Env1,St2} = pass(Fs0, Env0, St1),
	    {[{F1,L}|Fs1],Env1,St2}
    end;
pass([], Env, St) -> {[],Env,St}.

%% pass_progn(Forms, Line, Env, State) -> {Forms,Env,State}.
%%  Pass over a list of forms collecting and removing all macro
%%  defintions. All forms must be expanded at top-level to check form,
%%  but all can be expanded to full depth. Nesting of forms by progn
%%  is preserved.

pass_progn([['progn'|Pfs0]|Fs0], L, Env0, St0) ->
    {Pfs1,Env1,St1} = pass_progn(Pfs0, L, Env0, St0),
    {Fs1,Env2,St2} = pass_progn(Fs0, L, Env1, St1),
    {[['progn'|Pfs1]|Fs1],Env2,St2};
pass_progn([['eval-when-compile'|Ewcs0]|Fs0], L, Env0, St0) ->
    {Ecws1,Env1,St1} = pass_ewc(Ewcs0, L, Env0, St0),
    {Fs1,Env2,St2} = pass_progn(Fs0, L, Env1, St1),
    {[['progn'|Ecws1]|Fs1],Env2,St2};
pass_progn([['define-macro'|Def]=F|Fs0], L, Env0, St0) ->
    case def_macro(Def, Env0, St0) of
	{yes,Env1,St1} -> pass_progn(Fs0, L, Env1, St1);
	no ->
	    %% Ignore it and pass it on to generate error later.
	    {Fs1,Env1,St1} = pass_progn(Fs0, L, Env0, St0),
	    {[F|Fs1],Env1,St1}
    end;
pass_progn([F|Fs0], L, Env0, St0) ->
    %% First expand enough to test top form, if so process again.
    case expand_macro(F, Env0) of
	{yes,Exp} -> pass_progn([Exp|Fs0], L, Env0, St0);
	no -> 
	    %% Expand all if flag set.
	    {F1,St1} = if St0#mac.expand -> expand(F, Env0, St0);
			  true -> {F,St0}
		       end,
	    {Fs1,Env1,St2} = pass_progn(Fs0, L, Env0, St1),
	    {[F1|Fs1],Env1,St2}
    end;
pass_progn([], _, Env, St) -> {[],Env,St}.

%% pass_ewc(Forms, Line, Env, State) -> {Forms,Env,State}.
%%  Pass over a list of forms collecting and removing all function
%%  defintions. All forms must be expanded at top-level to check
%%  form. Nesting of forms by progn is preserved. All functions are
%%  put into one letrec structure in environment so they are mutally
%%  recursive. They can call functions in previously defined
%%  eval-when-compile's.

pass_ewc(Fs0, L, Env0, St0) ->
    {Fs1,Fbs,Env1,St1} = pass_ewc(Fs0, L, [], Env0, St0),
    Env2 = lfe_eval:make_letrec_env(Fbs, Env1),
    {Fs1,Env2,St1}.

pass_ewc([['progn'|Pfs0]|Fs0], L, Fbs0, Env0, St0) ->
    {Pfs1,Fbs1,Env1,St1} = pass_ewc(Pfs0, L, Fbs0, Env0, St0),
    {Fs1,Fbs2,Env2,St2} = pass_ewc(Fs0, L, Fbs1, Env1, St1),
    {[['progn'|Pfs1]|Fs1],Fbs2,Env2,St2};
pass_ewc([['eval-when-compile'|Ewcs0]|Fs0], L, Fbs0, Env0, St0) ->
    {Ecws1,Fbs1,Env1,St1} = pass_ewc(Ewcs0, L, Fbs0, Env0, St0),
    {Fs1,Fbs2,Env2,St2} = pass_ewc(Fs0, L, Fbs1, Env1, St1),
    {[['progn'|Ecws1]|Fs1],Fbs2,Env2,St2};
%% Do we want macros here???
%% pass_ewc([['define-macro'|Def]=F|Fs0], L, Fbs0, Env0, St0) ->
%%     case def_macro(Def, Env0, St0) of
%% 	{yes,Env1,St1} -> pass_ewc(Fs0, L, Fbs0, Env1, St1);
%% 	no ->
%% 	    %% Ignore it and pass it on to generate error later.
%% 	    {Fs1,Fbs1,Env1,St1} = pass_ewc(Fs0, L, Fbs0, Env0, St0),
%% 	    {[F|Fs1],Fbs1,Env1,St1}
%%     end;
pass_ewc([['define-function',Name,Def]=F|Fs0], L, Fbs0, Env0, St0) ->
    case func_arity(Def) of
	{yes,Ar} ->				%Definition not too bad
	    Fb = {Name,Ar,Def},
	    %% Env1 = lfe_eval:add_expr_func(Name, Ar, Def, Env0),
	    pass_ewc(Fs0, L, [Fb|Fbs0], Env0, St0);
	no ->					%Definition really bad
	    %% Ignore it and pass it on to generate error later.
	    {Fs1,Fbs1,Env1,St1} = pass_ewc(Fs0, L, Fbs0, Env0, St0),
	    {[F|Fs1],Fbs1,Env1,St1}
    end;
pass_ewc([F|Fs0], L, Fbs0, Env0, St0) ->
    %% First expand enough to test top form, if so process again.
    case expand_macro(F, Env0) of
	{yes,Exp} -> pass_ewc([Exp|Fs0], L, Fbs0, Env0, St0);
	no -> 
	    %% Ignore it and pass it on to generate error later.
	    {Fs1,Fbs1,Env1,St1} = pass_ewc(Fs0, L, Fbs0, Env0, St0),
	    {[F|Fs1],Fbs1,Env1,St1}
    end;
pass_ewc([], _, Fbs, Env, St) -> {[],Fbs,Env,St}.

func_arity([lambda,Args|_]) ->
    case is_symb_list(Args) of
	true -> {yes,length(Args)};
	false -> no
    end;
func_arity(['match-lambda',[Pat|_]|_]) ->
    case is_proper_list(Pat) of
	true -> {yes,length(Pat)};
	false -> no
    end;
func_arity(_) -> no.

%% def_macro([Name,Def], Env, State) -> {yes,Env,State} | no.

def_macro([Name,Def], Env, St) ->
    def_macro(Name, Def, Env, St).

def_macro(Name, ['lambda'|_]=Def, Env, St) ->
    {yes,add_mbinding(Name, Def, Env),St};
def_macro(Name, ['match-lambda'|_]=Def, Env, St) ->
    {yes,add_mbinding(Name, Def, Env),St};
def_macro(_, _, _, _) -> no.

%% def_record([Name|FieldDefs], Env, State) -> {Funs,Macs,Env,State}.
%% def_record(Name, FieldDefs, Env, State) -> {Funs,Macs,Env,State}.
%%  Define a VERY simple record by generating macros for all accesses.
%%  (define-record point x y)
%%    => make-point, is-point, match-point, set-point,
%%       point-x, set-point-x, point-y, set-point-y.

def_record([Name|Fdefs], Env, St) -> def_record(Name, Fdefs, Env, St).

def_record(Name, Fdefs, Env, St0) ->
    %% Get field names, default values and indices.
    Fields = map(fun ([F,_])when is_atom(F) -> F;
		     (F) when is_atom(F) -> F
		 end, Fdefs),
    Defs = map(fun ([F,D])when is_atom(F) -> ?Q(D);
		   (F) when is_atom(F) -> ?Q(?Q(undefined))
	       end, Fdefs),
    Findex = def_rec_fields(Fields),
    %% Make names for helper functions.
    Fi = list_to_atom(concat([Name,'-',field,'-',index])),
    Fu = list_to_atom(concat([Name,'-',field,'-',update])),
    %% Build helper functions.
    Funs = [[defun,Fi|				%Get index of field
	     map(fun ({F,I}) -> [[?Q(F)],I] end, Findex) ++
	     [[[f],[':',erlang,error,[tuple,?Q(undefined_field),?Q(Name),f]]]]],
	    [defun,Fu,[is,def],			%Update field list
	     %% Convert default list to tuple to make setting easier.
	     [fletrec,[[l,
			[[[f,v|is],i],[l,is,[setelement,['-',[Fi,f],1],i,v]]],
			[[[f],'_'],
			 [':',erlang,error,
			  [tuple,?Q(missing_value),?Q(Name),f]]],
			[[[],i],i]]],
	      ['let',[[i,[l,is,[list_to_tuple,def]]]],
	       [tuple_to_list,i]]]]
	   ],
    %% Make names for record creator/tester/matcher/setter.
    Make = list_to_atom(concat(['make','-',Name])),
    Test = list_to_atom(concat(['is','-',Name])),
    Match = list_to_atom(concat(['match','-',Name])),
    Set = list_to_atom(concat(['set','-',Name])),
    %% Make access macros.
    {Fdef,St1} = def_rec_fields(Fields, Name, St0), %Name is element 1!
    Macs = [['defmacro',Make,
	     [fds,
	      ['let',[[def,[list|Defs]]],
	       [backquote,
		[tuple,?Q(Name),['unquote-splicing',[Fu,fds,def]]]]]]],
	    ['defsyntax',Test,
	     [[rec],['is_record',rec,[quote,Name],length(Fields)+1]]],
	    ['defmacro',Match,
	     [fds,
	      ['let',[[def,[list|lists:duplicate(length(Fields),?Q('_'))]]],
	       [backquote,
		[tuple,?Q(Name),['unquote-splicing',[Fu,fds,def]]]]]]],
	    ['defmacro',Set,
	     [[rec|fds],
	      [fletrec,[[l,
			 [[[f,v|is],r],
			  %% Force evaluation left-to-right.
			  [l,is,[list,[quote,setelement],[Fi,f],r,v]]],
			 [[[f],'_'],
			  [':',erlang,error,
			   [tuple,?Q(missing_value),?Q(Name),f]]],
			 [[[],i],i]]],
	       [l,fds,rec]]]]
	    |
	    Fdef],
    %% lfe_io:format("~p\n", [{Funs,Macs}]),
    {Funs,Macs,Env,St1}.

def_rec_fields([F|Fs]) ->
    def_rec_fields([F|Fs], 2).			%First element is record name

def_rec_fields([F|Fs], N) ->
    [{F,N}|def_rec_fields(Fs, N+1)];
def_rec_fields([], _) -> [].

def_rec_fields(Fs, Name, St) ->
    Fis = def_rec_fields(Fs),			%Calculate indexes
    {foldr(fun ({F,N}, Fas) ->
		   Get = list_to_atom(concat([Name,'-',F])),
		   Set = list_to_atom(concat(['set-',Name,'-',F])),
		   [['defsyntax',Get,[[rec],[element,N,rec]]],
		    ['defsyntax',Set,[[rec,new],[setelement,N,rec,new]]]|
		    Fas]
	   end, [], Fis), St}.

%% expand(Form, Env, State) -> {Form,State}.
%%  Completely expand a form using expansions in Env and pre-defined
%%  macros.  N.B. builtin core forms cannot be overidden and are
%%  handled here first. The core forms also are particular about how
%%  their bodies are to be expanded.

%% Known Core forms which cannot be overidden.
expand([quote,_]=Q, _, St) -> {Q,St};
expand([cons,H0,T0], Env, St0) ->
    {H1,St1} = expand(H0, Env, St0),
    {T1,St2} = expand(T0, Env, St1),
    {[cons,H1,T1],St2};
expand([car,E0], Env, St0) ->			%Catch these to prevent
    {E1,St1} = expand(E0, Env, St0),		%redefining them
    {[car,E1],St1};
expand([cdr,E0], Env, St0) ->
    {E1,St1} = expand(E0, Env, St0),
    {[cdr,E1],St1};
expand([list|As0], Env, St0) ->
    {As1,St1} = expand_tail(As0, Env, St0),
    {[list|As1],St1};
expand([tuple|As0], Env, St0) ->
    {As1,St1} = expand_tail(As0, Env, St0),
    {[tuple|As1],St1};
expand([binary|As0], Env, St0) ->
    {As1,St1} = expand_tail(As0, Env, St0),
    {[binary|As1],St1};
expand(['lambda',Head|B0], Env, St0) ->
    {B1,St1} = expand_tail(B0, Env, St0),
    {['lambda',Head|B1],St1};
expand(['match-lambda'|B0], Env, St0) ->
    {B1,St1} = expand_ml_clauses(B0, Env, St0),
    {['match-lambda'|B1],St1};
expand(['let',Vbs0|B0], Env, St0) ->
    %% We don't really have to syntax check very strongly here so we
    %% can use normal clause expansion. Lint will catch errors.
    {Vbs1,St1} = expand_clauses(Vbs0, Env, St0),
    {B1,St2} = expand_tail(B0, Env, St1),
    {['let',Vbs1|B1],St2};
expand(['let-function',Fbs|B], Env, St) ->
    expand_let_function(Fbs, B, Env, St);
expand(['letrec-function',Fbs|B], Env, St) ->
    expand_letrec_function(Fbs, B, Env, St);
expand(['let-macro',Mbs|B], Env, St) ->
    expand_let_macro(Mbs, B, Env, St);
expand(['progn'|B0], Env, St0)->
    {B1,St1} = expand_tail(B0, Env, St0),
    {['progn'|B1],St1};
expand(['if'|B0], Env, St0) ->
    {B1,St1} = expand_tail(B0, Env, St0),
    {['if'|B1],St1};
expand(['case',E0|Cls0], Env, St0) ->
    {E1,St1} = expand(E0, Env, St0),
    {Cls1,St2} = expand_clauses(Cls0, Env, St1),
    {['case',E1|Cls1],St2};
expand(['receive'|Cls0], Env, St0) ->
    {Cls1,St1} = expand_clauses(Cls0, Env, St0),
    {['receive'|Cls1],St1};
expand(['catch'|B0], Env, St0) ->
    {B1,St1} = expand_tail(B0, Env, St0),
    {['catch'|B1],St1};
expand(['try',E|B], Env, St) ->
    expand_try(E, B, Env, St);
expand(['funcall'|As0], Env, St0) ->
    {As1,St1} = expand_tail(As0, Env, St0),
    {['funcall'|As1],St1};
expand(['call'|As0], Env, St0) ->
    {As1,St1} = expand_tail(As0, Env, St0),
    {['call'|As1],St1};
expand(['define-function',Head|B0], Env, St0) ->
    %% Needs to be handled specially to protect Head.
    {B1,St1} = expand_tail(B0, Env, St0),
    {['define-function',Head|B1],St1};
%% Now the case where we can have macros.
expand([Fun|_]=Call, Env, St0) when is_atom(Fun) ->
    case get_mbinding(Fun, Env) of
	{yes,Def} ->
 	    {Exp,St1} = exp_macro(Call, Def, Env, St0),
	    expand(Exp, Env, St1);		%Expand macro expansion again
	no ->
	    %% Not there then use defaults.
	    case exp_predef(Call, Env, St0) of
		{yes,Exp,St1} -> expand(Exp, Env, St1);
		no -> expand_tail(Call, Env, St0)
	    end
    end;
expand([_|_]=Call, Env, St) -> expand_tail(Call, Env, St);
expand(Tup, _, St) when is_tuple(Tup) ->
    %% Should we expand this? We assume implicit quote here.
    {Tup,St};
%% Everything else is atomic.
expand(F, _, St) -> {F,St}.			%Atomic

%% expand_list(Exprs, Env, State) -> {Exps,State}.
%%  Expand a proper list of exprs.

expand_list(Es, Env, St) ->
    mapfoldl(fun (E, S) -> expand(E, Env, S) end, St, Es).

%% expand_tail(Tail, Env, State) -> {Etail,State}.
%% expand_tail(ExpFun, Tail, Env, State) -> {Etail,State}.
%%  Expand the tail of a list, need not be a proper list.

expand_tail(Tail, Env, St) ->
    expand_tail(fun expand/3, Tail, Env, St).

expand_tail(Fun, [E0|Es0], Env, St0) ->
    {E1,St1} = Fun(E0, Env, St0),
    {Es1,St2} = expand_tail(Fun, Es0, Env, St1),
    {[E1|Es1],St2};
expand_tail(_, [], _, St) -> {[],St};
expand_tail(Fun, E, Env, St) -> Fun(E, Env, St). %Same on improper tail.

%% expand_clauses(Clauses, Env, State) -> {ExpCls,State}.
%% expand_ml_clauses(Clauses, Env, State) -> {ExpCls,State}.
%%  Expand macros in clause patterns, guards and body. Must handle
%%  match-lambda clauses differently as pattern is an explicit list of
%%  patterns *NOT* a pattern which is a list. This will affect what is
%%  detected a macro call.

expand_clauses(Cls, Env, St) ->
    expand_tail(fun expand_clause/3, Cls, Env, St).

expand_clause([P0,['when',G0]|B0], Env, St0) ->
    {P1,St1} = expand(P0, Env, St0),
    {G1,St2} = expand(G0, Env, St1),
    {B1,St3} = expand_tail(B0, Env, St2),
    {[P1,['when',G1]|B1],St3};
expand_clause([P0|B0], Env, St0) ->
    {P1,St1} = expand(P0, Env, St0),
    {B1,St2} = expand_tail(B0, Env, St1),
    {[P1|B1],St2};
expand_clause(Other, Env, St) -> expand(Other, Env, St).

expand_ml_clauses(Cls, Env, St) ->
    expand_tail(fun expand_ml_clause/3, Cls, Env, St).

expand_ml_clause([Ps0,['when',G0]|B0], Env, St0) ->
    {Ps1,St1} = expand_tail(Ps0, Env, St0),
    {G1,St2} = expand(G0, Env, St1),
    {B1,St3} = expand_tail(B0, Env, St2),
    {[Ps1,['when',G1]|B1],St3};
expand_ml_clause([Ps0|B0], Env, St0) ->
    {Ps1,St1} = expand_tail(Ps0, Env, St0),
    {B1,St2} = expand_tail(B0, Env, St1),
    {[Ps1|B1],St2};
expand_ml_clause(Other, Env, St) -> expand(Other, Env, St).

%% expand_let_function(FuncBindings, Body, Env, State) -> {Expansion,State}.
%% expand_letrec_function(FuncBindings, Body, Env, State) -> {Expansion,State}.
%%  Expand a let/letrec-function. Here we are only interested in
%%  marking functions as bound in the env and not what they are bound
%%  to, we will not be calling them. We only want to shadow macros of
%%  the same name.

expand_let_function(Fbs0, B0, Env, St0) ->
    {Fbs1,B1,St1} = do_expand_let_function(Fbs0, B0, Env, St0),
    {['let-function',Fbs1|B1],St1}.

expand_letrec_function(Fbs0, B0, Env, St0) ->
    {Fbs1,B1,St1} = do_expand_let_function(Fbs0, B0, Env, St0),
    {['letrec-function',Fbs1|B1],St1}.

do_expand_let_function(Fbs0, B0, Env0, St0) ->
    %% Only very limited syntax checking here (see above).
    Env1 = foldl(fun ([V,['lambda',Args|_]], Env) when is_atom(V) ->
			 case is_proper_list(Args) of
			     true -> add_fbinding(V, length(Args), dummy, Env);
			     false -> Env
			 end;
		     ([V,['match-lambda',[Pats|_]|_]], Env) when is_atom(V) ->
			 case is_proper_list(Pats) of
			     true -> add_fbinding(V, length(Pats), dummy, Env);
			     false -> Env
			 end;
		     (_, Env) -> Env
		 end, Env0, Fbs0),
    {Fbs1,St1} = expand_clauses(Fbs0, Env1, St0),
    {B1,St2} = expand_tail(B0, Env1, St1),
    {Fbs1,B1,St2}.

%% expand_let_macro(MacroBindings, Body, Env, State) -> {Expansion,State}.
%%  Expand a let_syntax. We add the actual macro binding to the env as
%%  we may need them while expanding the body.

expand_let_macro(Mbs, B0, Env0, St0) ->
    %% Add the macro defs from expansion and return body in a progn.
    Env1 = foldl(fun ([Name,['lambda'|_]=Def], Env) when is_atom(Name) ->
			 add_mbinding(Name, Def, Env);
		     ([Name,['match-lambda'|_]=Def], Env) when is_atom(Name) ->
			 add_mbinding(Name, Def, Env);
		     (_, Env) -> Env		%Ignore mistakes
		 end, Env0, Mbs),
    {B1,St1} = expand_tail(B0, Env1, St0),	%Expand the body
    {['progn'|B1],St1}.

expand_try(E0, B0, Env, St0) ->
    {E1,St1} = expand(E0, Env, St0),
    {B1,St2} = expand_tail(fun (['case'|Cls0], E, Sta) ->
				   {Cls1,Stb} = expand_clauses(Cls0, E, Sta),
				   {['case'|Cls1],Stb};
			       (['catch'|Cls0], E, Sta) ->
				   {Cls1,Stb} = expand_clauses(Cls0, E, Sta),
				   {['catch'|Cls1],Stb};
			       (['after'|A0], E, Sta) ->
				   {A1,Stb} = expand_tail(A0, E, Sta),
				   {['after'|A1],Stb};
			       (Other, _, St) -> {Other,St}
			   end, B0, Env, St1),
    {['try',E1|B1],St2}.

%% exp_macro(Call, Def, Env, State) -> {Exp,State}.
%%  Evaluate the macro definition by applying it to the call args. The
%%  definition is either a lambda or match-lambda, expand it and apply
%%  it to argument list.

exp_macro([Mac|Args], Def0, Env, St0) ->
    %% lfe_io:format("macro: ~p\n", [Def0]),
    {Def1,St1} = expand(Def0, Env, St0),	%Expand definition
    try 
	Exp = lfe_eval:apply(Def1, [Args], Env),
	{Exp,St1}
    catch
	error:Error ->
	    erlang:error({expand_macro,Mac,Error})
    end.

%%     Exp = lfe_eval:apply(Def1, [Args], Env),
%%     {Exp,St1}.

%% exp_predef(Form, Env, State) -> {yes,Form,State} | no.
%%  Handle the builtin predefined macros but only one at top-level and
%%  only once.  Expand must be called on result to fully expand
%%  macro. This is basically doing exactly the same as if they were
%%  user defined.

%% Builtin default macro expansions.
exp_predef([backquote,Bq], _, St) ->
    {yes,bq_expand(Bq),St};
exp_predef(['++'|Abody], _, St) ->
    Exp = case Abody of
	      [E] -> E;
	      [E|Es] -> [call,?Q(erlang),?Q('++'),E,['++'|Es]];
	      [] -> []
	  end,
    {yes,Exp,St};
exp_predef([':',M,F|As], _, St) ->
    {yes,['call',?Q(M),?Q(F)|As], St};
exp_predef(['?'], _, St) ->
    {yes,['receive',['omega','omega']], St};
exp_predef(['list*'|As], _, St) ->
    Exp = case As of
	      [E] -> E;
	      [E|Es] -> [cons,E,['list*'|Es]];
	      [] -> []
	  end,
    {yes,Exp,St};
exp_predef(['let*'|Lbody], _, St) ->
    Exp = case Lbody of
	      [[Vb|Vbs]|B] -> ['let',[Vb],['let*',Vbs|B]];
	      [[]|B] -> ['progn'|B];
	      [Vb|B] -> ['let',Vb|B]		%Pass error to let for lint.
	  end,
    {yes,Exp,St};
exp_predef(['flet*'|Lbody], _, St) ->
    Exp = case Lbody of
	      [[Vb|Vbs]|B] -> ['flet',[Vb],['flet*',Vbs|B]];
	      [[]|B] -> ['progn'|B];
	      [Vb|B] -> ['flet',Vb|B]		%Pass error to flet for lint.
	  end,
    {yes,Exp,St};
exp_predef(['cond'|Cbody], _, St) ->
    Exp = case Cbody of
	      [['else'|B]] -> ['progn'|B];
	      [[['?=',P,E]|B]|Cond] ->
		  ['case',E,[P|B],['_',['cond'|Cond]]];
	      [[['?=',P,['when',_]=G,E]|B]|Cond] ->
		  ['case',E,[P,G|B],['_',['cond'|Cond]]];
	      [[Test|B]|Cond] ->
		  ['if',Test,['progn'|B],['cond'|Cond]];
	      [] -> ?Q(false)
	  end,
    {yes,Exp,St};
exp_predef(['do'|Dbody], _, St0) ->
    %% (do ((v i c) ...) (test val) . body) but of limited use as it
    %% stands as we have to everything in new values.
    [Pars,[Test,Ret]|B] = Dbody,		%Check syntax
    {Vs,Is,Cs} = foldr(fun ([V,I,C], {Vs,Is,Cs}) -> {[V|Vs],[I|Is],[C|Cs]} end,
		       {[],[],[]}, Pars),
    {Fun,St1} = new_fun_name("do", St0),
    Exp = ['letrec-function',
	   [[Fun,[lambda,Vs,
		  ['if',Test,Ret,
		   ['progn'] ++ B ++ [[Fun|Cs]]]]]],
	   [Fun|Is]],
    {yes,Exp,St1};
exp_predef([lc|Lbody], _, St0) ->
    %% (lc (qual ...) e ...)
    [Qs|E] = Lbody,
    {Exp,St1} = lc_te(E, Qs, St0),
    {yes,Exp,St1};
exp_predef([bc|Bbody], _, St0) ->
    %% (bc (qual ...) e ...)
    [Qs|E] = Bbody,
    {Exp,St1} = bc_te(E, Qs, St0),
    {yes,Exp,St1};
exp_predef(['andalso'|Abody], _, St) ->
    Exp = case Abody of
	      [E] -> E;				%Let user check last call
	      [E|Es] ->
		  ['case',E,[?Q(true),['andalso'|Es]],[?Q(false),?Q(false)]];
	      [] -> ?Q(true)
	  end,
    {yes,Exp,St};
exp_predef(['orelse'|Obody], _, St) ->
    Exp = case Obody of
	      [E] -> E;				%Let user check last call
	      [E|Es] ->
		  ['case',E,[?Q(true),?Q(true)],[?Q(false),['orelse'|Es]]];
	      [] -> ?Q(false)
	  end,
    {yes,Exp,St};
exp_predef(['fun',F,Ar], _, St0) when is_atom(F), is_integer(Ar), Ar >= 0 ->
    {Vs,St1} = new_symbs(Ar, St0),
    {yes,['lambda',Vs,[F|Vs]],St1};
exp_predef(['fun',M,F,Ar], _, St0)
  when is_atom(M), is_atom(F), is_integer(Ar), Ar >= 0 ->
    {Vs,St1} = new_symbs(Ar, St0),
    {yes,['lambda',Vs,['call',?Q(M),?Q(F)|Vs]],St1};
exp_predef(['defrecord'|Def], Env0, St0) ->
    {Funs,Macs,_,St1} = def_record(Def, Env0, St0),
    {yes,[progn,['eval-when-compile'|Funs]|Macs],St1};
exp_predef(['include-file'|Ibody], _, St) ->
    %% This is a VERY simple include file macro!
    [F] = Ibody,
    {ok,Fs} = lfe_io:read_file(F),		%No real error handling
    {yes,['progn'|Fs],St};
%% Compatibility macros for the older Scheme like syntax.
exp_predef(['begin'|Body], _, St) ->
    {yes,['progn'|Body],St};
exp_predef(['define',Head|Body], _, St) ->
    Exp = case is_symb_list(Head) of
	      true ->
		  ['define-function',hd(Head),[lambda,tl(Head)|Body]];
	      false ->
		  %% Let next step catch errors here.
		  ['define-function',Head|Body]
	  end,
    {yes,Exp,St};
exp_predef(['define-record'|Def], _, St) ->
    {yes,[defrecord|Def],St};
exp_predef(['define-syntax',Name,Def], _, St) ->
    %% N.B. New macro definition is function of 1 argument, whole
    %% argument list of macro call.
    Mdef = expand_syntax(Name, Def),
    {yes,['define-macro'|Mdef],St};
exp_predef(['let-syntax',Defs|Body], _, St) ->
    Mdefs = map(fun ([Name,Def]) -> expand_syntax(Name, Def) end, Defs),
    {yes,['let-macro',Mdefs|Body],St};
%% Common Lisp inspired macros.
exp_predef([defmodule|Mod], _, St) ->
    {yes,['define-module'|Mod],St};
exp_predef([defun,Name|Def], _, St) ->
    %% Educated guess whether traditional (defun name (a1 a2 ...) ...)
    %% or matching (defun name (patlist1 ...) (patlist2 ...))
    Fdef = expand_defun(Name, Def),
    {yes,['define-function'|Fdef],St};
exp_predef([defmacro,Name|Def], _, St) ->
    %% Educated guess whether traditional (defmacro name (a1 a2 ...) ...)
    %% or matching (defmacro name (patlist1 ...) (patlist2 ...))
    Mdef = expand_defmacro(Name, Def),
    {yes,['define-macro'|Mdef],St};
exp_predef([defsyntax,Name|Rules], _, St) ->
    %% Expand into call function which expands macro an invocation
    %% time, this saves much space and costs us nothing.
    {yes,['define-macro'|expand_rules(Name, [], Rules)],St};
exp_predef([flet,Defs|Body], _, St) ->
    Fdefs = map(fun ([Name|Def]) -> expand_defun(Name, Def) end, Defs),
    {yes,['let-function',Fdefs|Body], St};
exp_predef([fletrec,Defs|Body], _, St) ->
    Fdefs = map(fun ([Name|Def]) -> expand_defun(Name, Def) end, Defs),
    {yes,['letrec-function',Fdefs|Body], St};
exp_predef([macrolet,Defs|Body], _, St) ->
    Mdefs = map(fun ([Name|Def]) -> expand_defmacro(Name, Def) end, Defs),
    {yes,['let-macro',Mdefs|Body],St};
exp_predef([syntaxlet,Defs|Body], _, St) ->
    Mdefs = map(fun ([Name|Rules]) -> expand_rules(Name, [], Rules) end, Defs),
    {yes,['let-macro',Mdefs|Body],St};
%% This was not a call to a predefined macro.
exp_predef(_, _, _) -> no.

%% expand_defun(Name, Def) -> Lambda | Match-Lambda.
%% Educated guess whether traditional (defun name (a1 a2 ...) ...)
%% or matching (defun name (patlist1 ...) (patlist2 ...))

expand_defun(Name, [Args|Rest]) ->
    case is_symb_list(Args) of
	true -> [Name,[lambda,Args|Rest]];
	false -> [Name,['match-lambda',Args|Rest]]
    end.

%% expand_defmacro(Name, Def) -> Lambda | MatchLambda.
%%  Educated guess whether traditional (defmacro name (a1 a2 ...) ...)
%%  or matching (defmacro name (patlist1 ...) (patlist2 ...))
%%  N.B. New macro definition is function of 1 argument, whole
%%  argument list of macro call.

expand_defmacro(Name, [Args|Rest]=Cls) ->
    case is_symb_list(Args) of
	true -> [Name,['match-lambda',[[Args]|Rest]]];
	false ->
	    Mcls = map(fun ([Head|Body]) -> [[Head]|Body] end, Cls),
	    [Name,['match-lambda'|Mcls]]
    end.

%% expand_syntax(Name, Def) -> Lambda | MatchLambda.
%%  N.B. New macro definition is function of 1 argument, whole
%%  argument list of macro call.

expand_syntax(Name, Def) ->
    case Def of
	[macro|Cls] ->
	    Mcls = map(fun ([Pat|Body]) -> [[Pat]|Body] end, Cls),
	    [Name,['match-lambda'|Mcls]];
	['syntax-rules'|Rules] ->
	    expand_rules(Name, [], Rules)
    end.

%% expand_rules(Name, Keywords, Rules) -> Lambda.
%%  Expand into call function which expands macro an invocation time,
%%  this saves much space and costs us nothing.

expand_rules(Name, Keywords, Rules) ->
    [Name,[lambda,[args],
	   [':',lfe_macro,mbe_syntax_rules_proc,
	    [quote,Name],[quote,Keywords],[quote,Rules],args]]].

new_symb(St) ->
    C = St#mac.vc,
    {list_to_atom("|-" ++ integer_to_list(C) ++ "-|"),St#mac{vc=C+1}}.

new_symbs(N, St) -> new_symbs(N, St, []).

new_symbs(N, St0, Vs) when N > 0 ->
    {V,St1} = new_symb(St0),
    new_symbs(N-1, St1, [V|Vs]);
new_symbs(0, St, Vs) -> {Vs,St}.    

new_fun_name(Pre, St) ->
    C = St#mac.fc,
    {list_to_atom(Pre ++ "$^" ++ integer_to_list(C)),St#mac{fc=C+1}}.

%%  By André van Tonder
%%  Unoptimized.  See Dybvig source for optimized version.
%%  Resembles one by Richard Kelsey and Jonathan Rees.
%%   (define-syntax quasiquote
%%     (lambda (s)
%%       (define (qq-expand x level)
%%         (syntax-case x (quasiquote unquote unquote-splicing)
%%           (`x   (quasisyntax (list 'quasiquote
%%                                    #,(qq-expand (syntax x) (+ level 1)))))
%%           (,x (> level 0)
%%                 (quasisyntax (cons 'unquote
%%                                    #,(qq-expand (syntax x) (- level 1)))))
%%           (,@x (> level 0)
%%                 (quasisyntax (cons 'unquote-splicing
%%                                    #,(qq-expand (syntax x) (- level 1)))))
%%           (,x (= level 0)
%%                 (syntax x))
%%           (((unquote x ...) . y)
%%            (= level 0)
%%                 (quasisyntax (append (list x ...)
%%                                      #,(qq-expand (syntax y) 0))))
%%           (((unquote-splicing x ...) . y)
%%            (= level 0)
%%                 (quasisyntax (append (append x ...)
%%                                      #,(qq-expand (syntax y) 0))))
%%           ((x . y)
%%                 (quasisyntax (cons  #,(qq-expand (syntax x) level)
%%                                     #,(qq-expand (syntax y) level))))
%%           (#(x ...)
%%                 (quasisyntax (list->vector #,(qq-expand (syntax (x ...))    
%%                                                         level))))
%%           (x    (syntax 'x)))) 
%%       (syntax-case s ()
%%         ((_ x) (qq-expand (syntax x) 0)))))

%% bq_expand(Exp) -> Exp.
%%  Not very efficient quasiquote expander, but very compact code.  Is
%%  R6RS compliant and can handle unquote and unquote-splicing with
%%  more than one argument properly.  Actually with simple cons/append
%%  optimisers code now quite good.

bq_expand(Exp) -> bq_expand(Exp, 0).

bq_expand([backquote,X], N) ->
    [list,[quote,backquote],bq_expand(X, N+1)];
bq_expand([unquote|X], N) when N > 0 ->
    bq_cons([quote,unquote], bq_expand(X, N-1));
bq_expand([unquote,X], 0) -> X;
bq_expand(['unquote-splicing'|X], N) when N > 0 ->
    bq_cons([quote,'unquote-splicing'], bq_expand(X, N-1));
%% Next 2 handle case of splicing into a list.
bq_expand([[unquote|X]|Y], 0) ->
    bq_append([list|X], bq_expand(Y, 0));
bq_expand([['unquote-splicing'|X]|Y], 0) ->
    bq_append(['++'|X], bq_expand(Y, 0));
bq_expand([X|Y], N) ->
    bq_cons(bq_expand(X, N), bq_expand(Y, N));
bq_expand(X, N) when is_tuple(X) ->
%%     [list_to_tuple,bq_expand(tuple_to_list(X), N)];
    [tuple|tl(bq_expand(tuple_to_list(X), N))];
bq_expand(X, _) when is_atom(X) -> [quote,X];
bq_expand(X, _) -> X.

bq_append(['++',L], R) -> bq_append(L, R);	%Catch single unquote-splice
bq_append([], R) -> R;
bq_append(L, []) -> L;
%% Will these 2 cases move code errors illegally?
bq_append([list,L], [list|R]) -> [list,L|R];
bq_append([list,L], R) -> [cons,L,R];
%%bq_append(['++'|L], R) -> ['++'|L ++ [R]];
%%bq_append(L, ['++'|R]) -> ['++',L|R];
bq_append(L, R) -> ['++',L,R].

bq_cons([quote,L], [quote,R]) -> [quote,[L|R]];
bq_cons(L, [list|R]) -> [list,L|R];
bq_cons(L, []) -> [list,L];
bq_cons(L, R) -> [cons,L,R].

%% Macro by Example
%% Proper syntax-rules which can handle ... ellipsis by Dorai Sitaram.
%%
%% While we extend patterns to include tuples and binaries as in
%% normal LFE we leave the keyword handling in even though it is
%% subsumed by quotes and not really used.

%% To make it more lispy!
-define(car(L), hd(L)).
-define(cdr(L), tl(L)).
-define(cadr(L), hd(tl(L))).
-define(cddr(L), tl(tl(L))).

-define(mbe_ellipsis(Car, Cddr), [Car,'...'|Cddr]).

is_mbe_symbol(S) ->
    is_atom(S) andalso (S /= true) andalso (S /= false).

%% Tests if ellipsis pattern, (p ... . rest)
%% is_mbe_ellipsis(?mbe_ellipsis(_, _)) -> true;
%% is_mbe_ellipsis(_) -> false.

mbe_match_pat([quote,P], E, _) -> P =:= E;
mbe_match_pat([tuple|Ps], [tuple|Es], K) ->	%Match tuple constructor
    mbe_match_pat(Ps, Es, K);
mbe_match_pat([tuple|Ps], E, K) ->		%Match literal tuple
    case is_tuple(E) of
	true -> mbe_match_pat(Ps, tuple_to_list(E), K);
	false -> false
    end;
mbe_match_pat(?mbe_ellipsis(Pcar, _), E, K) ->
    case is_proper_list(E) of
	true ->
	    all(fun (X) -> mbe_match_pat(Pcar, X, K) end, E);
	false -> false
    end;
mbe_match_pat([Pcar|Pcdr], E, K) ->    
    case E of
	[Ecar|Ecdr] ->
	    mbe_match_pat(Pcar, Ecar, K) andalso
		mbe_match_pat(Pcdr, Ecdr, K);
	_ -> false
    end;
mbe_match_pat(Pat, E, K) ->    
    case is_mbe_symbol(Pat) of
	true ->
	    case member(Pat, K) of
		true -> Pat =:= E;
		false -> true
	    end;
	false -> Pat =:= E
    end.

mbe_get_ellipsis_nestings(Pat, K) ->
    m_g_e_n(Pat, K).

m_g_e_n([quote,_], _) -> [];
m_g_e_n([tuple|Ps], K) -> m_g_e_n(Ps, K);
m_g_e_n(?mbe_ellipsis(Pcar, Pcddr), K) ->
    [m_g_e_n(Pcar, K)|m_g_e_n(Pcddr, K)];
m_g_e_n([Pcar|Pcdr], K) ->
    m_g_e_n(Pcar, K) ++ m_g_e_n(Pcdr, K);
m_g_e_n(Pat, K) ->
    case is_mbe_symbol(Pat) of
	true ->
	    case member(Pat, K) of
		true -> [];
		false -> [Pat]
	    end;
	false -> []
    end.

mbe_ellipsis_sub_envs(Nestings, R) ->
    ormap(fun (C) ->
		  case mbe_intersect(Nestings, ?car(C)) of
		      true -> ?cdr(C);
		      false -> false
		  end end, R).

%% Return first value of F applied to elements in list which is not false.
ormap(F, [H|T]) ->
    case F(H) of
	false -> ormap(F, T);
	V -> V
    end;
ormap(_, []) -> false.
    
mbe_intersect(V, Y) ->
    case is_mbe_symbol(V) orelse is_mbe_symbol(Y) of
	true -> V =:= Y;
	false ->
	    any(fun (V0) ->
			any(fun (Y0) -> mbe_intersect(V0, Y0) end, Y)
		end, V)
    end.

%% mbe_get_bindings(Pattern, Expression, Keywords) -> Bindings.

mbe_get_bindings([quote,_], _, _) -> [];
mbe_get_bindings([tuple|Ps], [tuple|Es], K) ->	%Tuple constructor
    mbe_get_bindings(Ps, Es, K);
mbe_get_bindings([tuple|Ps], E, K) ->		%Literal tuple
    mbe_get_bindings(Ps, tuple_to_list(E), K);
mbe_get_bindings(?mbe_ellipsis(Pcar, _), E, K) ->
    [[mbe_get_ellipsis_nestings(Pcar, K) |
      map(fun (X) -> mbe_get_bindings(Pcar, X, K) end, E)]];
mbe_get_bindings([Pcar|Pcdr], [Ecar|Ecdr], K) ->
    mbe_get_bindings(Pcar, Ecar, K) ++
	mbe_get_bindings(Pcdr, Ecdr, K);
mbe_get_bindings(Pat, E, K) ->
    case is_mbe_symbol(Pat) of
	true ->
	    case member(Pat, K) of
		true -> [];
		false -> [[Pat|E]]
	    end;
	false -> []
    end.

%% mbe_expand_pattern(Pattern, Bindings, Keywords) -> Form.

mbe_expand_pattern([quote,P], R, K) ->
    [quote,mbe_expand_pattern(P, R, K)];
mbe_expand_pattern([tuple|Ps], R, K) ->
    [tuple|mbe_expand_pattern(Ps, R, K)];
mbe_expand_pattern(?mbe_ellipsis(Pcar, Pcddr), R, K) ->
    Nestings = mbe_get_ellipsis_nestings(Pcar, K),
    Rr = mbe_ellipsis_sub_envs(Nestings, R),
    map(fun (R0) -> mbe_expand_pattern(Pcar, R0 ++ R, K) end, Rr) ++
		mbe_expand_pattern(Pcddr, R, K);
mbe_expand_pattern([Pcar|Pcdr], R, K) ->
    [mbe_expand_pattern(Pcar, R, K)|
     mbe_expand_pattern(Pcdr, R, K)];
mbe_expand_pattern(Pat, R, K) ->
    case is_mbe_symbol(Pat) of
	true ->
	    case member(Pat, K) of
		true -> Pat;
		false ->
		    case lfe_lib:assoc(Pat, R) of
			[_|Cdr] -> Cdr;
			[] -> Pat
		    end
	    end;
	false -> Pat
    end.

%% mbe_syntax_rules_proc(Name, Keywords, Rules, Argsym, Keywordsym) ->
%%      Sexpr.
%%  Generate the sexpr to evaluate in a macro from Name and
%%  Rules. When the sexpr is applied to arguments (in Argsym) and
%%  evaluated then expansion is returned.

%% Return sexpr to evaluate.
mbe_syntax_rules_proc(Name, Ks0, Cls, Argsym, Ksym) ->
    Ks = [Name|Ks0],
    %% Don't prepend the macro name to the arguments!
    ['let',[[Ksym,[quote,Ks]]],
     ['cond'] ++
     map(fun (C) ->
		 Inpat = hd(C),
		 Outpat = hd(tl(C)),
		 [[':',lfe_macro,mbe_match_pat,[quote,Inpat], Argsym, Ksym],
		  ['let',
		   [[r,[':',lfe_macro,mbe_get_bindings,
			[quote,Inpat],Argsym,Ksym]]],
		   [':',lfe_macro,mbe_expand_pattern,[quote,Outpat],r,Ksym]]]
	 end, Cls) ++
    [[[quote,true],[exit,[tuple,[quote,macro_clause],[quote,Name]]]]]].

%% Do it all directly.
mbe_syntax_rules_proc(Name, Ks0, Cls, Args) ->
    Ks = [Name|Ks0],
    case ormap(fun ([Pat,Exp]) ->
		       case mbe_match_pat(Pat, Args, Ks) of
			   true ->
			       R = mbe_get_bindings(Pat, Args, Ks),
			       [mbe_expand_pattern(Exp, R, Ks)];
			   false -> false
		       end
	       end, Cls) of
	[Res] -> Res;
	false -> exit({macro_clause,Name})
    end.

%% lc_te(Exprs, Qualifiers, State) -> {Exp,State}.
%% bc_te(Exprs, Qualifiers, State) -> {Exp,State}.
%%  Expand a list/binary comprehension. Algorithm straight out of
%%  Simon PJs book.

%% lc_te(Es, Qs, St) -> lc_tq(Es, Qs, [], St).
lc_te(Es, Qs, St) ->
    c_tq(fun (L, S) -> {[cons,['progn'|Es],L],S} end, Qs, [], St).

%%bc_te(Es, Qs, St) -> bc_tq(Es, Qs, <<>>, St).
bc_te(Es, Qs, St) ->
    c_tq(fun (L, S) ->
		 case reverse(Es) of
		     [R] -> {[binary,R,[L,bitstring]],S};
		     [R|Rs] -> {['progn'|reverse(Rs)] ++
				[[binary,R,[L,bitstring]]],S};
		     [] -> {L,S}
		 end
	 end, Qs, <<>>, St).
    
c_tq(Exp, [['<-',P,G]|Qs], L, St0) ->		%List generator
    {H,St1} = new_fun_name("lc", St0),		%Function name
    {Us,St2} = new_symb(St1),			%Tail variable
    {Rest,St3} = c_tq(Exp, Qs, [H,Us], St2),	%Do rest of qualifiers
    {['letrec-function',
      [[H,['match-lambda',
	   [[[P|Us]],Rest],			%Matches pattern
	   [[['_'|Us]],[H,Us]],			%No match
	   [[[]],L]]]],				%End of list
      [H,G]],St3};
c_tq(Exp, [['<=',P,G]|Qs], L, St0) ->		%Bits generator
    {H,St1} = new_fun_name("bc", St0),		%Function name
    {B,St2} = new_symb(St1),			%Bin variable
    {Rest,St3} = c_tq(Exp, Qs, [H,B], St2),	%Do rest of qualifiers
    Brest = [B,bitstring,'big-endian',unsigned,[unit,1]], %,[size,all]
    {['letrec-function',
      [[H,['match-lambda',
	   [[[binary,P,Brest]],Rest],		%Matches pattern
%%	   [[[binary,Brest]],[H,B]]]]],		%No match
	   [[[binary,Brest]],L]]]],		%No match
      [H,G]],St3};
c_tq(Exp, [['?=',P,E]|Qs], L, St0) ->
    {Rest,St1} = c_tq(Exp, Qs, L, St0),
    {['case',E,[P,Rest],['_',L]],St1};
c_tq(Exp, [['?=',P,['when',_]=G,E]|Qs], L, St0) ->
    {Rest,St1} = c_tq(Exp, Qs, L, St0),
    {['case',E,[P,G,Rest],['_',L]],St1};
c_tq(Exp, [T|Qs], L, St0) ->
    {Rest,St1} = c_tq(Exp, Qs, L, St0),
    {['if',T,Rest,L],St1};
c_tq(Exp, [], L, St) ->
    Exp(L, St).

%% expand_macro(Form, Env) -> {yes,Exp} | no.
%% expand_macro_1(Form, Env) -> {yes,Exp} | no.
%%  User functions for testing macro expansions, either one expansion
%%  or as far as it can go.

expand_macro(Form, Env) ->
    case expand_macro_1(Form, Env) of
	{yes,Exp} -> {yes,expand_macro_loop(Exp, Env)};
	no -> no
    end.

expand_macro_loop(Form, Env) ->
    case expand_macro_1(Form, Env) of
	{yes,Exp} -> expand_macro_loop(Exp, Env);
	no -> Form
    end.

expand_macro_1([Name|_]=Call, Env) when is_atom(Name) ->
    case lfe_lib:is_core_form(Name) of
	true -> no;				%Don't expand core forms
	false ->
	    case get_mbinding(Name, Env) of
		{yes,Def} ->
		    %% User macro bindings.
		    {Exp,_} = exp_macro(Call, Def, Env, #mac{expand=false}),
		    {yes,Exp};
		no ->
		    %% Default macro bindings.
		    case exp_predef(Call, Env, #mac{}) of
			{yes,Exp,_} -> {yes,Exp};
			no -> no
		    end
	    end
	end;
expand_macro_1(_, _) -> no.
