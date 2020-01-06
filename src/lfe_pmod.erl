%% Copyright (c) 2008-2016 Robert Virding
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

%% File    : lfe_pmod.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang parameterised module transformer.

-module(lfe_pmod).

-export([module/2]).

%% -compile(export_all).

-import(lists, [member/2,keysearch/3,
                all/2,map/2,foldl/3,foldr/3,mapfoldl/3,mapfoldr/3, concat/1]).
-import(ordsets, [add_element/2,is_element/2,from_list/1,union/2]).
-import(orddict, [store/3,find/2]).

-import(lfe_env, [new/0,add_vbinding/3,add_vbindings/2,get_vbinding/2,
                  add_fbinding/4,add_fbindings/2,get_fbinding/3,
                  add_ibinding/5,get_gbinding/3]).

-include("lfe_comp.hrl").

-define(Q(E), [quote,E]).                       %For quoting

-record(param, {mod=[],                         %Module name
                pars=[],                        %Module parameters
                extd=[],                        %Extends
                this=[],                        %This match pattern
                env=[]}).                       %Environment

%% module(ModuleForms, CompInfo) -> {ModuleName,ModuleForms}.
%%  Expand the forms to handle parameterised modules if necessary,
%%  otherwise just pass forms straight through.

module([{['define-module',[Mod|_]|_],_}|_]=Fs, Ci) ->
    {Mod,expand_module(Fs, Ci#cinfo.opts)};
module([{['define-module',Mod|_],_}|_]=Fs, _) ->
    %% Normal module, do nothing.
    {Mod,Fs};
module(Fs, _) -> {[],Fs}.                       %Not a module, do nothing

expand_module(Fs0, Opts) ->
    St0 = #param{env=lfe_env:new()},
    {Acc,St1} = lists:foldl(fun exp_form/2, {[],St0}, Fs0),
    Fs1 = lists:reverse(Acc),
    ?DEBUG("#param: ~p\n", [{Fs1,St1}], Opts),
    %% {ok,_} = lfe_lint:module(Fs1, Opts),
    Fs1.

exp_form({['define-module',[Mod|Ps],Meta,Atts0],L}, {Acc,St0}) ->
    %% Save the good bits and define new/N and instance/N.
    St1 = St0#param{mod=Mod,pars=Ps},
    {Atts1,St2} = exp_attrs(Atts0, St1),
    {Nl,Il} = case St2#param.extd of
                  [] ->
                      {[lambda,Ps,[instance|Ps]],
                       [lambda,Ps,[tuple,?Q(Mod)|Ps]]};
                  Ex ->
                      {[lambda,Ps,[instance,[call,?Q(Ex),?Q(new)|Ps]|Ps]],
                       [lambda,[base|Ps],[tuple,?Q(Mod),base|Ps]]}
              end,
    New = ['define-function',new,[],Nl],
    Inst = ['define-function',instance,[],Il],
    %% Fix this match pattern depending on extends.
    St3 = case St2#param.extd of
              [] -> St2#param{this=['=',this,[tuple,'_'|Ps]]};
              _ -> St2#param{this=['=',this,[tuple,'_',base|Ps]]}
          end,
    {[{{New,L},{Inst,L},['define-module',Mod,Meta,Atts1],L}|Acc],St3};
exp_form({['define-function',F,Meta,Def0],L}, {Acc,St}) ->
    Def1 = exp_function(Def0, St),
    {[{['define-function',F,Meta,Def1],L}|Acc],St};
exp_form({F,L}, {Acc,St}) ->
    {[{F,L}|Acc],St}.

exp_attrs(Atts0, St0) ->
    %% Pre-scan to pick up 'extends'.
    St1 = foldl(fun ([extends,M], S) -> S#param{extd=M};
                    (_, S) -> S
                end, St0, Atts0),
    %% Now do "real" processing.
    {Atts1,St2} = mapfoldl(fun ([export,all], S) -> {[export,all],S};
                               ([export|Es0], S) ->
                                   %% Add 1 for this to each export.
                                   Es1 = map(fun ([F,A]) -> [F,A+1] end, Es0),
                                   {[export|Es1],S};
                               ([import|Is], S0) ->
                                   S1 = collect_imps(Is, S0),
                                   {[import|Is],S1};
                               (Md, S) -> {Md,S}
                           end, St1, Atts0 ++ [[abstract,true]]),
    %% Add export for new/N and instance/N.
    Nar = length(St2#param.pars),
    Iar = case St2#param.extd of
              [] -> Nar;
              _ -> Nar+1
          end,
    {[[export,[new,Nar],[instance,Iar]]|Atts1],St2}.

collect_imps(Is, St) ->
    foldl(fun (['from',M|Fs], S) ->
                  Env = foldl(fun ([F,Ar], E) ->
                                      add_ibinding(M, F, Ar, F, E) end,
                              S#param.env, Fs),
                  S#param{env=Env};
              (['rename',M|Fs], S) ->
                  Env = foldl(fun ([[F,Ar],R], E) ->
                                      add_ibinding(M, F, Ar, R, E) end,
                              S#param.env, Fs),
                  S#param{env=Env}
          end, St, Is).

%% exp_function(Lambda, State) -> Lambda.
%%  The resultant code matches the arguments in two steps: first the
%%  THIS arguemnt is matched and then the expanded function body
%%  ((match-)lambda) is funcalled. We KNOW that funcall of a
%%  (match-)lambda is inline expanded into a let or case so this is
%%  efficient.

exp_function(Lambda, #param{this=Th,env=Env}) ->
    As = new_args(lambda_arity(Lambda)),
    ['match-lambda',[As ++ [Th],[funcall,exp_expr(Lambda, Env)|As]]].

%% exp_function(['match-lambda'|Cls0], #param{this=Th,env=Env}) ->
%%     Cls1 = map(fun ([As|Body]) ->
%%                exp_clause([As ++ [Th]|Body], Env)
%%            end, Cls0),
%%     ['match-lambda'|Cls1];
%% exp_function([lambda,As|Body0], #param{this=Th,env=Env}) ->
%%     Body1 = exp_list(Body0, Env),
%%     ['match-lambda',[As ++ [Th]|Body1]].

new_args(N) when N > 0 ->
    [list_to_atom("{{-" ++ [$a+N-1] ++ "-}}")|new_args(N-1)];
new_args(0) -> [].

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
exp_expr([tref|[_,_]=Es], Env) -> [tref|exp_list(Es, Env)];
exp_expr([tset|[_,_,_]=Es], Env) -> [tset|exp_list(Es, Env)];
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
        {yes,_,_} -> [Fun|exp_list(Es, Env)];   %Imported or Bif
        {yes,local} -> [Fun|exp_list(Es, Env)]; %Local function
        _ -> [Fun|exp_list(Es, Env) ++ [this]]
    end;
exp_expr(E, _) when is_atom(E) -> E;
exp_expr(E, _) -> E.                            %Atoms expand to themselves.

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
             (['after'|B]) -> ['after'|exp_body(B, Env)]
         end, Body)].

exp_call([M,F|As], Env) ->
    [exp_expr(M, Env),exp_expr(F, Env)|exp_list(As, Env)].

lambda_arity([lambda,As|_]) -> length(As);
lambda_arity(['match-lambda',[As|_]|_]) -> length(As).
