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

%% File    : lfe_trans.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang translator.

%%% Translate LFE code to/from vanilla Erlang AST.
%%%
%%% Note that we don't really check code here as such, we assume the
%%% input is correct. If there is an error in the input we just fail.
%%% This allows us to accept forms which are actually illegal but we
%%% may special case, for example functions call in patterns which
%%% will become macro expansions.

-module(lfe_trans).

-export([from_expr/1,from_expr/2,from_body/1,from_body/2,from_lit/1,
     to_expr/2,to_lit/2]).

-import(lists, [map/2,foldl/3,mapfoldl/3,foldr/3,splitwith/2]).

-define(Q(E), [quote,E]).           %We do a lot of quoting

-record(from, {vc=0                 %Variable counter
          }).

%% from_expr(AST) -> Sexpr.
%% from_expr(AST, Variables) -> {Sexpr,Variables}.
%% from_body([AST]) -> Sexpr.
%% from_body([AST], Variables) -> {Sexpr,Variables}.
%%  Translate a vanilla Erlang expression into LFE. The main
%%  difficulty is in the handling of variables. The implicit matching
%%  of known variables in vanilla must be translated into explicit
%%  equality tests in guards (which is what the compiler does
%%  internally). For this we need to keep track of visible variables
%%  and detect when they reused in patterns.

from_expr(E) ->
    {S,_,_} = from_expr(E, ordsets:new(), #from{}),
    S.

from_expr(E, Vs0) ->
    Vt0 = ordsets:from_list(Vs0),                   %We are clean
    {S,Vt1,_} = from_expr(E, Vt0, #from{}),
    {S,ordsets:to_list(Vt1)}.

from_body(Es) ->
    {Les,_,_} = from_body(Es, ordsets:new(), #from{}),
    [progn|Les].

from_body(Es, Vs0) ->
    Vt0 = ordsets:from_list(Vs0),                   %We are clean
    {Les,Vt1,_} = from_body(Es, Vt0, #from{}),
    {[progn|Les],ordsets:to_list(Vt1)}.

%% from_expr(AST, VarTable, State) -> {Sexpr,VarTable,State}.

from_expr({var,_,V}, Vt, St) -> {V,Vt,St};          %Unquoted atom
from_expr({nil,_}, Vt, St) -> {[],Vt,St};
from_expr({integer,_,I}, Vt, St) -> {I,Vt,St};
from_expr({float,_,F}, Vt, St) -> {F,Vt,St};
from_expr({atom,_,A}, Vt, St) -> {?Q(A),Vt,St};     %Quoted atom
from_expr({string,_,S}, Vt, St) -> {?Q(S),Vt,St};   %Quoted string
from_expr({cons,_,H,T}, Vt0, St0) ->
    {Car,Vt1,St1} = from_expr(H, Vt0, St0),
    {Cdr,Vt2,St2} = from_expr(T, Vt1, St1),
    {from_cons(Car, Cdr),Vt2,St2};
%%    {[cons,Car,Cdr],Vt2,St2};
from_expr({tuple,_,Es}, Vt0, St0) ->
    {Ss,Vt1,St1} = from_expr_list(Es, Vt0, St0),
    {[tuple|Ss],Vt1,St1};
from_expr({bin,_,Segs}, Vt0, St0) ->
    {Ss,Vt1,St1} = from_bitsegs(Segs, Vt0, St0),
    {[binary|Ss],Vt1,St1};
%% Core closure special forms.
from_expr({'fun',_,{clauses,Cls}}, Vt, St0) ->
    {Lcls,St1} = from_fun_cls(Cls, Vt, St0),
    {['match-lambda'|Lcls],Vt,St1};        %Don't bother using lambda
from_expr({'fun',_,{function,F,A}}, Vt, St) ->
    {['fun',F,A],Vt,St};                    %Return macros here?
from_expr({'fun',_,{function,M,F,A}}, Vt, St) ->
    {['fun',M,F,A],Vt,St};
%% Core control special forms.
from_expr({block,_,Es}, Vt0, St0) ->
    {Les,Vt1,St1} = from_body(Es, Vt0, St0),
    {[progn|Les],Vt1,St1};
from_expr({'case',_,E,Cls}, Vt0, St0) ->
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {Lcls,Vt2,St2} = from_icrt_cls(Cls, Vt1, St1),
    {['case',Le|Lcls],Vt2,St2};
from_expr({'if',_,Cls}, Vt0, St0) ->
    {Lcls,Vt1,St1} = from_icrt_cls(Cls, Vt0, St0),
    {['case',[]|Lcls],Vt1,St1};
from_expr({'receive',_,Cls}, Vt0, St0) ->
    {Lcls,Vt1,St1} = from_icrt_cls(Cls, Vt0, St0),
    {['receive'|Lcls],Vt1,St1};
from_expr({'receive',_,Cls,Timeout,Body}, Vt0, St0) ->
    {Lcls,Vt1,St1} = from_icrt_cls(Cls, Vt0, St0),
    {Lt,Vt2,St2} = from_expr(Timeout, Vt1, St1),
    {Lb,Vt3,St3} = from_body(Body, Vt2, St2),
    {['receive'|Lcls ++ [['after',Lt|Lb]]],Vt3,St3};
%% More complex special forms. These become LFE macros.
from_expr({lc,_,E,Qs}, Vt0, St0) ->
    {Lqs,Vt1,St1} = from_lc_quals(Qs, Vt0, St0),
    {Le,Vt2,St2} = from_expr(E, Vt1, St1),
    {[lc,Lqs,Le],Vt2,St2};
from_expr({record,_,R,Fs}, Vt0, St0) ->             %Create a record
    MR = list_to_atom("make-" ++ atom_to_list(R)),
    {Lfs,Vt1,St1} = from_rec_fields(Fs, Vt0, St0),
    {[MR|Lfs],Vt1,St1};
from_expr({record,_,E,R,Fs}, Vt0, St0) ->           %Set fields in record
    SR = list_to_atom("set-" ++ atom_to_list(R)),
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {Lfs,Vt2,St2} = from_rec_fields(Fs, Vt1, St1),
    {[SR,Le|Lfs],Vt2,St2};
from_expr({record_field,_,E,R,{atom,_,F}}, Vt0, St0) -> %We KNOW!
    RF = list_to_atom(atom_to_list(R) ++ "-" ++ atom_to_list(F)),
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {[RF,Le],Vt1,St1};
from_expr({record_field,_,_,_}=M, Vt, St) ->        %Pre R16 packages
    from_package_module(M, Vt, St);
%% Function calls.
from_expr({call,_,{remote,_,M,F},As}, Vt0, St0) ->  %Remote function call
    {Lm,Vt1,St1} = from_expr(M, Vt0, St0),
    {Lf,Vt2,St2} = from_expr(F, Vt1, St1),
    {Las,Vt3,St3} = from_expr_list(As, Vt2, St2),
    {[call,Lm,Lf|Las],Vt3,St3};
from_expr({call,_,{atom,_,F},As}, Vt0, St0) ->      %Local function call
    {Las,Vt1,St1} = from_expr_list(As, Vt0, St0),
    {[F|Las],Vt1,St1};
from_expr({call,_,F,As}, Vt0, St0) ->               %F not an atom or remote
    {Lf,Vt1,St1} = from_expr(F, Vt0, St0),
    {Las,Vt2,St2} = from_expr_list(As, Vt1, St1),
    {[funcall,Lf|Las],Vt2,St2};
from_expr({'try',_,Es,Scs,Ccs,As}, Vt, St0) ->
    %% Currently erl_lint does not allow any exports!
    {Les,_,St1} = from_body(Es, Vt, St0),
    %% These maybe empty.
    {Lscs,_,St2} = if Scs =:= [] -> {[],[],St1};
              true -> from_icrt_cls(Scs, Vt, St1)
           end,
    {Lccs,_,St3} = if Ccs =:= [] -> {[],[],St2};
              true -> from_icrt_cls(Ccs, Vt, St2)
           end,
    {Las,_,St4} = from_body(As, Vt, St3),
    {['try',[progn|Les]|
      from_maybe('case', Lscs) ++
      from_maybe('catch', Lccs) ++
      from_maybe('after', Las)],Vt,St4};
from_expr({'catch',_,E}, Vt0, St0) ->
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {['catch',Le],Vt1,St1};
from_expr({match,_,P,E}, Vt0, St0) ->
    {Lp,Eqt,Vt1,St1} = from_pat(P, Vt0, St0),   %The pattern
    {Le,Vt2,St2} = from_expr(E, Vt1, St1),      %The expression
    {Lb,Vt3,St3} = from_expr(P, Vt2, St2),      %Pattern as value expression
    Leg = from_eq_tests(Eqt),                   %Implicit guard tests
    {['let',[[Lp,['when'|Leg],Le]],Lb],Vt3,St3};
from_expr({op,_,Op,A}, Vt0, St0) ->
    {La,Vt1,St1} = from_expr(A, Vt0, St0),
    {[Op,La],Vt1,St1};
from_expr({op,_,Op,L,R}, Vt0, St0) ->
    {Ll,Vt1,St1} = from_expr(L, Vt0, St0),
    {Lr,Vt2,St2} = from_expr(R, Vt1, St1),
    {[Op,Ll,Lr],Vt2,St2}.

%% from_package_module(Module, VarTable, State) -> {Module,VarTable,State}.
%%  We must handle the special case where in pre-R16 you could have
%%  packages with a dotted module path. It used a special record_field
%%  tuple. This does not work in R16 and later!

from_package_module({record_field,_,_,_}=M, Vt, St) ->
    Segs = erl_parse:package_segments(M),
    A = list_to_atom(packages:concat(Segs)),
    {?Q(A),Vt,St}.

from_maybe(_, []) -> [];
from_maybe(Tag, Es) -> [[Tag|Es]].

from_cons(Car, [list|Es]) -> [list,Car|Es];
from_cons(Car, []) -> [list,Car];
from_cons(Car, Cdr) -> [cons,Car,Cdr].

%% from_body(Expressions, VarTable, State) -> {Body,VarTable,State}.
%%  Handle '=' specially here and translate into let containing rest
%%  of body.

from_body([{match,_,_,_}=Match], Vt0,St0) ->    %Last match
    {Lm,Vt1,St1} = from_expr(Match, Vt0, St0),  %Must return pattern as value
    {[Lm],Vt1,St1};
from_body([{match,_,P,E}|Es], Vt0, St0) ->
    {Lp,Eqt,Vt1,St1} = from_pat(P, Vt0, St0),
    {Le,Vt2,St2} = from_expr(E, Vt1, St1),
    {Les,Vt3,St4} = from_body(Es, Vt2, St2),
    Leg = from_eq_tests(Eqt),                   %Implicit guard tests
    {[['let',[[Lp,['when'|Leg],Le]]|Les]],Vt3,St4};
from_body([E|Es], Vt0, St0) ->
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {Les,Vt2,St2} = from_body(Es, Vt1, St1),
    {[Le|Les],Vt2,St2};
from_body([], Vt, St) -> {[],Vt,St}.

from_expr_list(Es, Vt, St) -> mapfoldl2(fun from_expr/3, Vt, St, Es).

%% from_icrt_cls(Clauses, VarTable, State) -> {Clauses,VarTable,State}.
%% from_icrt_cl(Clause, VarTable, State) -> {Clause,VarTable,State}.
%%  If/case/receive/try clauses.
%%  No ; in guards, so no guard sequence only one list of guard tests.

from_icrt_cls(Cls, Vt, St) -> from_cls(fun from_icrt_cl/3, Vt, St, Cls).

from_icrt_cl({clause,_,[],[G],B}, Vt0, St0) ->          %If clause
    {Lg,Vt1,St1} = from_body(G, Vt0, St0),
    {Lb,Vt2,St2} = from_body(B, Vt1, St1),
    {['_',['when'|Lg]|Lb],Vt2,St2};
from_icrt_cl({clause,_,H,[],B}, Vt0, St0) ->
    {[Lh],Eqt,Vt1,St1} = from_pat_list(H, Vt0, St0),    %List of one
    {Lb,Vt2,St2} = from_body(B, Vt1, St1),
    Leg = from_eq_tests(Eqt),
    {[Lh,['when'|Leg]|Lb],Vt2,St2};
from_icrt_cl({clause,_,H,[G],B}, Vt0, St0) ->
    {[Lh],Eqt,Vt1,St1} = from_pat_list(H, Vt0, St0),    %List of one
    {Lg,Vt2,St2} = from_body(G, Vt1, St1),
    {Lb,Vt3,St3} = from_body(B, Vt2, St2),
    Leg = from_eq_tests(Eqt),
    {[Lh,['when'|Leg ++ Lg]|Lb],Vt3,St3}.

%% from_fun_cls(Clauses, VarTable, State) -> {Clauses,State}.
%% from_fun_cl(Clause, VarTable, State) -> {Clause,VarTable,State}.
%%  Function clauses, all variables in the patterns are new variables
%%  which shadow existing variables without equality tests.

from_fun_cls(Cls, Vt, St0) ->
    {Lcls,_,St1} = from_cls(fun from_fun_cl/3, Vt, St0, Cls),
    {Lcls,St1}.

from_fun_cl({clause,_,H,[],B}, Vt0, St0) ->
    {Lh,Eqt,Vtp,St1} = from_pat_list(H, [], St0),
    Vt1 = ordsets:union(Vtp, Vt0),              %All variables so far
    {Lb,Vt2,St2} = from_body(B, Vt1, St1),
    Leg = from_eq_tests(Eqt),
    {[Lh,['when'|Leg]|Lb],Vt2,St2};
from_fun_cl({clause,_,H,[G],B}, Vt0, St0) ->
    {Lh,Eqt,Vtp,St1} = from_pat_list(H, [], St0),
    Vt1 = ordsets:union(Vtp, Vt0),              %All variables so far
    {Lg,Vt2,St2} = from_body(G, Vt1, St1),
    {Lb,Vt3,St3} = from_body(B, Vt2, St2),
    Leg = from_eq_tests(Eqt),
    {[Lh,['when'|Leg ++ Lg]|Lb],Vt3,St3}.

from_cls(Fun, Vt0, St0, [C]) ->
    {Lc,Vt1,St1} = Fun(C, Vt0, St0),
    {[Lc],Vt1,St1};
from_cls(Fun, Vt0, St0, [C|Cs]) ->
    {Lc,Vtc,St1} = Fun(C, Vt0, St0),
    {Lcs,Vtcs,St2} = from_cls(Fun, Vt0, St1, Cs),
    {[Lc|Lcs],ordsets:intersection(Vtc, Vtcs),St2}.

from_eq_tests(Gs) -> [ ['=:=',V,V1] || {V,V1} <- Gs ].

%% from_lc_quals(Qualifiers, VarTable, State) -> {Qualifiers,VarTable,State}.

from_lc_quals([{generate,_,P,E}|Qs], Vt0, St0) ->
    {Lp,Eqt,Vt1,St1} = from_pat(P, Vt0, St0),
    {Le,Vt2,St2} = from_expr(E, Vt1, St1),
    {Lqs,Vt3,St3} = from_lc_quals(Qs, Vt2, St2),
    Leg = from_eq_tests(Eqt),
    {[['<-',Lp,['when'|Leg]|Le]|Lqs],Vt3,St3};
from_lc_quals([T|Qs], Vt0, St0) ->
    {Lt,Vt1,St1} = from_expr(T, Vt0, St0),
    {Lqs,Vt2,St2} = from_lc_quals(Qs, Vt1, St1),
    {[Lt|Lqs],Vt2,St2};
from_lc_quals([], Vt, St) -> {[],Vt,St}.

%% from_rec_fields(Recfields, VarTable, State) -> {Recfields,VarTable,State}.

from_rec_fields([{record_field,_,{atom,_,F},E}|Fs], Vt0, St0) ->
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {Lfs,Vt2,St2} = from_rec_fields(Fs, Vt1, St1),
    {[F,Le|Lfs],Vt2,St2};
from_rec_fields([{record_field,_,{var,_,F},E}|Fs], Vt0, St0) -> %special case!!
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {Lfs,Vt2,St2} = from_rec_fields(Fs, Vt1, St1),
    {[F,Le|Lfs],Vt2,St2};
from_rec_fields([], Vt, St) -> {[],Vt,St}.

%% from_bitsegs(Segs, VarTable, State) -> {Segs,VarTable,State}.

from_bitsegs([{bin_element,_,Seg,Size,Type}|Segs], Vt0, St0) ->
    {S,Vt1,St1} = from_bitseg(Seg, Size, Type, Vt0, St0),
    {Ss,Vt2,St2} = from_bitsegs(Segs, Vt1, St1),
    {[S|Ss],Vt2,St2};
from_bitsegs([], Vt, St) -> {[],Vt,St}.

%% So it won't get confused with strings.
from_bitseg({integer,_,I}, default, default, Vt, St) -> {I,Vt,St};
from_bitseg({integer,_,I}, Size, Type, Vt0, St0) ->
    {Lsize,Vt1,St1} = from_bitseg_size(Size, Vt0, St0),
    {[I|from_bitseg_type(Type) ++ Lsize],Vt1,St1};
from_bitseg({float,_,F}, Size, Type, Vt0, St0) ->
    {Lsize,Vt1,St1} = from_bitseg_size(Size, Vt0, St0),
    {[F|from_bitseg_type(Type) ++ Lsize],Vt1,St1};
from_bitseg({string,_,S}, Size, Type, Vt0, St0) ->
    {Lsize,Vt1,St1} = from_bitseg_size(Size, Vt0, St0),
    {[S|from_bitseg_type(Type) ++ Lsize],Vt1,St1};
from_bitseg(E, Size, Type, Vt0, St0) ->
    {Le,Vt1,St1} = from_expr(E, Vt0, St0),
    {Lsize,Vt2,St2} = from_bitseg_size(Size, Vt1, St1),
    {[Le|from_bitseg_type(Type) ++ Lsize],Vt2,St2}.

from_bitseg_size(default, Vt, St) -> {[],Vt,St};
from_bitseg_size(Size, Vt0, St0) ->
    {Ssize,Vt1,St1} = from_expr(Size, Vt0, St0),
    {[[size,Ssize]],Vt1,St1}.

from_bitseg_type(default) -> [];
from_bitseg_type(Ts) ->
    map(fun ({unit,U}) -> [unit,U]; (T) -> T end, Ts).

new_from_var(#from{vc=C}=St) ->
    V = list_to_atom(lists:concat(["---",C,"---"])),
    {V,St#from{vc=C+1}}.

from_pat({var,_,'_'}, Vt, St) -> {'_',[],Vt,St};    %Special case _
from_pat({var,_,V}, Vt, St0) ->                     %Unquoted atom
    case ordsets:is_element(V, Vt) of               %Is variable bound?
        true ->
            {V1,St1} = new_from_var(St0),       %New var for pattern
            {V1,[{V,V1}],Vt,St1};               %Add to guard tests
        false ->
            {V,[],ordsets:add_element(V, Vt),St0}
    end;
from_pat({nil,_}, Vt, St) -> {[],[],Vt,St};
from_pat({integer,_,I}, Vt, St) -> {I,[],Vt,St};
from_pat({float,_,F}, Vt, St) -> {F,[],Vt,St};
from_pat({atom,_,A}, Vt, St) -> {?Q(A),[],Vt,St};   %Quoted atom
from_pat({string,_,S}, Vt, St) -> {?Q(S),[],Vt,St}; %Quoted string
from_pat({cons,_,H,T}, Vt0, St0) ->
    {Car,Eqt1,Vt1,St1} = from_pat(H, Vt0, St0),
    {Cdr,Eqt2,Vt2,St2} = from_pat(T, Vt1, St1),
    {from_cons(Car, Cdr),Eqt1++Eqt2,Vt2,St2};
from_pat({tuple,_,Es}, Vt0, St0) ->
    {Ss,Eqt,Vt1,St1} = from_pat_list(Es, Vt0, St0),
    {[tuple|Ss],Eqt,Vt1,St1};
from_pat({bin,_,Segs}, Vt0, St0) ->
    {Ss,Eqt,Vt1,St1} = from_pat_bitsegs(Segs, Vt0, St0),
    {[binary|Ss],Eqt,Vt1,St1};
from_pat({record,_,R,Fs}, Vt0, St0) ->        %Match a record
    MR = list_to_atom("match-" ++ atom_to_list(R)),
    {Sfs,Eqt,Vt1,St1} = from_rec_fields(Fs, Vt0, St0),
    {[MR|Sfs],Eqt,Vt1,St1};
from_pat({match,_,P1,P2}, Vt0, St0) ->        %Aliases
    {Lp1,Eqt1,Vt1,St1} = from_pat(P1, Vt0, St0),
    {Lp2,Eqt2,Vt2,St2} = from_pat(P2, Vt1, St1),
    {['=',Lp1,Lp2],Eqt1++Eqt2,Vt2,St2};
%% Basically illegal syntax which maybe generated by internal tools.
from_pat({call,_,{atom,_,F},As}, Vt0, St0) ->
    %% This will never occur in real code but for macro expansions.
    {Las,Eqt,Vt1,St1} = from_pat_list(As, Vt0, St0),
    {[F|Las],Eqt,Vt1,St1}.

from_pat_list([P|Ps], Vt0, St0) ->
    {Lp,Eqt,Vt1,St1} = from_pat(P, Vt0, St0),
    {Lps,Eqts,Vt2,St2} = from_pat_list(Ps, Vt1, St1),
    {[Lp|Lps],Eqt++Eqts,Vt2,St2};
from_pat_list([], Vt, St) -> {[],[],Vt,St}.

%% from_pat_bitsegs(Segs, VarTable, State) -> {Segs,EqTable,VarTable,State}.

from_pat_bitsegs([{bin_element,_,Seg,Size,Type}|Segs], Vt0, St0) ->
    {S,Eqt,Vt1,St1} = from_pat_bitseg(Seg, Size, Type, Vt0, St0),
    {Ss,Eqts,Vt2,St2} = from_pat_bitsegs(Segs, Vt1, St1),
    {[S|Ss],Eqt++Eqts,Vt2,St2};
from_pat_bitsegs([], Vt, St) -> {[],[],Vt,St}.

from_pat_bitseg({string,_,S}, Size, Type, Vt0, St0) ->
    {Lsize,Vt1,St1} = from_pat_bitseg_size(Size, Vt0, St0),
    {[S|from_bitseg_type(Type) ++ Lsize],[],Vt1,St1};
from_pat_bitseg(P, Size, Type, Vt0, St0) ->
    {Lp,Eqt,Vt1,St1} = from_pat(P, Vt0, St0),
    {Lsize,Vt2,St2} = from_pat_bitseg_size(Size, Vt1, St1),
    {[Lp|from_bitseg_type(Type) ++ Lsize],Eqt,Vt2,St2}.

from_pat_bitseg_size(default, Vt, St) -> {[],Vt,St};
from_pat_bitseg_size({var,_,V}, Vt, St) ->  %Size vars never match
    {[[size,V]],Vt,St};
from_pat_bitseg_size(Size, Vt0, St0) ->
    {Ssize,_,Vt1,St1} = from_pat(Size, Vt0, St0),
    {[[size,Ssize]],Vt1,St1}.

from_lit({nil,_}) -> [];
from_lit({integer,_,I}) -> I;
from_lit({float,_,F}) -> F;
from_lit({atom,_,A}) -> A;                  %Quoted atom
from_lit({string,_,S}) -> S;
from_lit({cons,_,H,T}) ->
    [cons,from_lit(H),from_lit(T)];
from_lit({tuple,_,Es}) ->
    [tuple|from_lit_list(Es)].

from_lit_list(Es) -> [ from_lit(E) || E <- Es ].

-record(to, {vc=0                           %Variable counter
        }).

to_expr(E, L) ->
    {Le,_} = to_expr(E, L, orddict:new(), #to{}),
    Le.

%% to_expr(Expr, LineNumber, VarTable, State) -> {Expr,State}.

to_expr([], L, _, St) -> {{nil,L},St};
to_expr(I, L, _, St) when is_integer(I) -> {{integer,L,I},St};
to_expr(F, L, _, St) when is_float(F) -> {{float,L,F},St};
to_expr(V, L, Vt, St) when is_atom(V) ->    %Unquoted atom
    to_expr_var(V, L, Vt, St);
to_expr(T, L, Vt, St0) when is_tuple(T) ->
    {Es,St1} = to_expr_list(tuple_to_list(T), L, Vt, St0),
    {{tuple,L,Es},St1};
to_expr(?Q(V), L, _, St) -> {to_lit(V, L),St};
to_expr([cons,H,T], L, Vt, St0) ->
    {Eh,St1} = to_expr(H, L, Vt, St0),
    {Et,St2} = to_expr(T, L, Vt, St1),
    {{cons,L,Eh,Et},St2};
to_expr([car,E], L, Vt, St0) ->
    {Ee,St1} = to_expr(E, L, Vt, St0),
    {{call,L,{atom,L,hd},[Ee]},St1};
to_expr([cdr,E], L, Vt, St0) ->
    {Ee,St1} = to_expr(E, L, Vt, St0),
    {{call,L,{atom,L,tl},[Ee]},St1};
to_expr([list|Es], L, Vt, St) ->
    Fun = fun (E, {Tail,St0}) ->
                  {Ee,St1} = to_expr(E, L, Vt, St0),
                  {{cons,L,Ee,Tail},St1}
          end,
    foldr(Fun, {{nil,L},St}, Es);
to_expr(['list*'|Es], L, Vt, St) ->        %Macro
    to_expr_list_s(fun to_expr/4, L, Vt, St, Es);
to_expr([tuple|Es], L, Vt, St0) ->
    {Ees,St1} = to_expr_list(Es, L, Vt, St0),
    {{tuple,L,Ees},St1};
to_expr([binary|Segs], L, Vt, St0) ->
    {Esegs,St1} = to_bitsegs(Segs, L, Vt, St0),
    {{bin,L,Esegs},St1};
%% Core closure special forms.
to_expr([lambda,As|B], L, Vt, St0) ->
    {Ecl,St1} = to_fun_cl([As|B], L, Vt, St0),
    {{'fun',L,{clauses,[Ecl]}},St1};
to_expr(['match-lambda'|Cls], L, Vt, St0) ->
    {Ecls,St1} = to_fun_cls(Cls, L, Vt, St0),
    {{'fun',L,{clauses,Ecls}},St1};
to_expr(['fun',F,A], L, _, St) -> {{'fun',L,{function,F,A}},St};
to_expr(['fun',M,F,A], L, _, St) -> {{'fun',L,{function,M,F,A}},St};
to_expr(['let',Lbs|B], L, Vt0, St0) ->
    {Ebs,Vt1,St1} = to_let_bindings(Lbs, L, Vt0, St0),
    {Eb,St2} = to_body(B, L, Vt1, St1),
    {{block,L,Ebs ++ Eb},St2};
%% Core control special forms.
to_expr([progn|B], L, Vt, St) -> to_block(B, L, Vt, St);
to_expr(['if',Test,True], L, Vt, St) ->
    to_expr(['if',Test,True,?Q(false)], L, Vt, St);
to_expr(['if',Test,True,False], L, Vt, St0) ->
    {Etest,St1} = to_expr(Test, L, Vt, St0),
    {Ecls,St2} = to_icrt_cls([[?Q(true),True],[?Q(false),False]], L, Vt, St1),
    {{'case',L,Etest,Ecls},St2};
to_expr(['case',E|Cls], L, Vt, St0) ->
    {Ee,St1} = to_expr(E, L, Vt, St0),
    {Ecls,St2} = to_icrt_cls(Cls, L, Vt, St1),
    {{'case',L,Ee,Ecls},St2};
to_expr(['receive'|Cls0], L, Vt, St0) ->
    %% Get the right receive form depending on whether there is an after.
    {Cls1,A} = splitwith(fun (['after'|_]) -> false; (_) -> true end, Cls0),
    {Ecls,St1} = to_icrt_cls(Cls1, L, Vt, St0),
    case A of
        [['after',T|B]] ->
            {Et,St2} = to_expr(T, L, Vt, St1),
            {Eb,St3} = to_body(B, L, Vt, St2),
            {{'receive',L,Ecls,Et,Eb},St3};
        [] ->
            {{'receive',L,Ecls},St1}
    end;
%% Special known macros.
%% No record stuff here as they are macros which have been expanded.
to_expr([lc,Qs|Es], L, Vt0, St0) ->
    {Eqs,Vt1,St1} = to_lc_quals(Qs, L, Vt0, St0),
    {Ees,St2} = to_block(Es, L, Vt1, St1),
    {{lc,L,Ees,Eqs},St2};
%% General function calls.
to_expr([call,?Q(erlang),?Q(F)|As], L, Vt, St0) ->
    %% This is semantically the same but some tools behave differently
    %% (qlc_pt).
    {Eas,St1} = to_expr_list(As, L, Vt, St0),
    case is_erl_op(F, length(As)) of
        true -> {list_to_tuple([op,L,F|Eas]),St1};
        false ->
            {{call,L,{remote,{atom,L,erlang},{atom,L,F}},Eas},St1}
    end;
to_expr([call,M,F|As], L, Vt, St0) ->
    {Em,St1} = to_expr(M, L, Vt, St0),
    {Ef,St2} = to_expr(F, L, Vt, St1),
    {Eas,St3} = to_expr_list(As, L, Vt, St2),
    {{call,L,{remote,L,Em,Ef},Eas},St3};
to_expr([F|As], L, Vt, St0) when is_atom(F) ->  %General function call
    {Eas,St1} = to_expr_list(As, L, Vt, St0),
    case is_erl_op(F, length(As)) of
        true -> {list_to_tuple([op,L,F|Eas]),St1};
        false -> {{call,L,{atom,L,F},Eas},St1}
    end;
to_expr(List, L, _, St) ->
    case is_integer_list(List) of
        true -> {{string,L,List},St};
        false ->
            io:format("BOOM:~p\n", [List]),
            {integer,L,4711}                    %Not right!
    end.

to_expr_var(V, L, Vt, St) ->
    Var = case orddict:find(V, Vt) of
              {ok,V1} -> V1;
              error -> V
          end,
    {{var,L,Var},St}.

%% is_erl_op(Op, Arity) -> bool().
%% Is Op/Arity one of the known Erlang operators?

is_erl_op(Op, Ar) ->
    erl_internal:arith_op(Op, Ar)
    orelse erl_internal:bool_op(Op, Ar)
    orelse erl_internal:comp_op(Op, Ar)
    orelse erl_internal:list_op(Op, Ar)
    orelse erl_internal:send_op(Op, Ar).

to_body(Es, L, Vt, St) ->
    Fun = fun (E, St0) -> to_expr(E, L, Vt, St0) end,
    mapfoldl(Fun, St, Es).

to_expr_list(Es, L, Vt, St) ->
    Fun = fun (E, St0) -> to_expr(E, L, Vt, St0) end,
    mapfoldl(Fun, St, Es).

to_expr_list_s(Fun, L, Vt, St, [E]) -> Fun(E, L, Vt, St);
to_expr_list_s(Fun, L, Vt, St0, [E|Es]) ->
    {Les,St1} = to_expr_list_s(Fun, L, Vt, St0, Es),
    {Le,St2} = Fun(E, L, Vt, St1),
    {{cons,L,Le,Les},St2};
to_expr_list_s(_, L, _, St, []) -> {{nil,L},St}.

to_pat_list_s(Fun, L, Vt, St, [E]) -> Fun(E, L, Vt, St);
to_pat_list_s(Fun, L, Vt0, St0, [E|Es]) ->
    {Les,Vt1,St1} = to_pat_list_s(Fun, L, Vt0, St0, Es),
    {Le,Vt2,St2} = Fun(E, L, Vt1, St1),
    {{cons,L,Le,Les},Vt2,St2};
to_pat_list_s(_, L, Vt, St, []) -> {{nil,L},Vt,St}.

%% to_block(Expressions, LineNumber, VarTable, State) -> {Block,State}.
%% Don't generate {block,...} if only one expression, though
%% semantically the same some tools can't handle it (qlc_pt).

to_block(Es, L, Vt, St0) ->
    case to_expr_list(Es, L, Vt, St0) of
        {[Ee],St1} -> {Ee,St1};                 %No need to wrap
        {Ees,St1} -> {{block,L,Ees},St1}        %Must wrap
    end.

%% to_let_bindings(Bindings, LineNumber, VarTable, State) ->
%%     {Block,VarTable,State}.
%%  When we have a guard translate into a case but special case where
%%  we have an empty guard.

to_let_bindings(Lbs, L, Vt, St) ->
    Fun = fun ([P,E], Vt0, St0) ->
                  {Ep,Vt1,St1} = to_pat(P, L, Vt0, St0),
                  {Ee,St2} = to_expr(E, L, Vt0, St1),
                  {{match,L,Ep,Ee},Vt1,St2};
              ([P,['when'],E], Vt0, St0) ->     %Just to keep it short
                  {Ep,Vt1,St1} = to_pat(P, L, Vt0, St0),
                  {Ee,St2} = to_expr(E, L, Vt0, St1),
                  {{match,L,Ep,Ee},Vt1,St2};
              ([P,['when'|G],E], Vt0, St0) ->
                  {Ee,St1} = to_expr(E, L, Vt0, St0),
                  {Ep,Vt1,St2} = to_pat(P, L, Vt0, St1),
                  {Eg,St3} = to_body(G, L, Vt1, St2),
                  {{'case',L,Ee,[{clause,L,[Ep],Eg,[Ep]}]},Vt1,St3}
      end,
    mapfoldl2(Fun, Vt, St, Lbs).

%% to_icrt_cls(Clauses, LineNumber, VarTable, State) -> {Clauses,State}.
%% to_icrt_cl(Clause, LineNumber, VarTable, State) -> {Clause,State}.
%%  If/case/receive/try clauses.

to_icrt_cls(Cls, L, Vt, St) ->
    Fun = fun (Cl, St0) -> to_icrt_cl(Cl, L, Vt, St0) end, 
    mapfoldl(Fun, St, Cls).

to_icrt_cl([P,['when'|G]|B], L, Vt0, St0) ->
    {Ep,Vt1,St1} = to_pat(P, L, Vt0, St0),
    {Eg,St2} = to_body(G, L, Vt1, St1),
    {Eb,St3} = to_body(B, L, Vt1, St2),
    {{clause,L,[Ep],Eg,Eb},St3};
to_icrt_cl([P|B], L, Vt0, St0) ->
    {Ep,Vt1,St1} = to_pat(P, L, Vt0, St0),
    {Eb,St2} = to_body(B, L, Vt1, St1),
    {{clause,L,[Ep],[],Eb},St2}.

%% to_fun_cls(Clauses, LineNumber) -> Clauses.
%% to_fun_cl(Clause, LineNumber) -> Clause.
%%  Function clauses.

to_fun_cls(Cls, L, Vt, St) ->
    Fun = fun (Cl, St0) -> to_fun_cl(Cl, L, Vt, St0) end,
    mapfoldl(Fun, St, Cls).

to_fun_cl([As,['when'|G]|B], L, Vt0, St0) ->
    {Eas,Vt1,St1} = to_pat_list(As, L, Vt0, St0),
    {Eg,St2} = to_body(G, L, Vt1, St1),
    {Eb,St3} = to_body(B, L, Vt1, St2),
    {{clause,L,Eas,Eg,Eb},St3};
to_fun_cl([As|B], L, Vt0, St0) ->
    {Eas,Vt1,St1} = to_pat_list(As, L, Vt0, St0),
    {Eb,St2} = to_body(B, L, Vt1, St1),
    {{clause,L,Eas,[],Eb},St2}.

%% to_lc_quals(Qualifiers, LineNumber, VarTable, State) ->
%%     {Qualifiers,VarTable,State}.
%%  Can't use mapfoldl2 as guard habling modifies Qualifiers.

to_lc_quals([['<-',P,E]|Qs], L, Vt0, St0) ->
    {Ep,Vt1,St1} = to_pat(P, L, Vt0, St0),
    {Ee,St2} = to_expr(E, L, Vt1, St1),
    {Eqs,Vt2,St3} = to_lc_quals(Qs, L, Vt1, St2),
    {[{generate,L,Ep,Ee}|Eqs],Vt2,St3};
to_lc_quals([['<-',P,['when'|G],E]|Qs], L, Vt, St) ->
    to_lc_quals([['<-',P,E]|G ++ Qs], L, Vt, St);    %Move guards to tests
to_lc_quals([T|Qs], L, Vt0, St0) ->
    {Et,St1} = to_expr(T, L, Vt0, St0),
    {Eqs,Vt1,St2} = to_lc_quals(Qs, L, Vt0, St1),
    {[Et|Eqs],Vt1,St2};
to_lc_quals([], _, Vt, St) -> {[],Vt,St}.

%% to_bitsegs(Segs, LineNumber, VarTable, State) -> {Segs,State}.
%% This gives a verbose value, but it is correct.

to_bitsegs(Ss, L, Vt, St) ->
    Fun = fun (S, St0) -> to_bitseg(S, L, Vt, St0) end,
    mapfoldl(Fun, St, Ss).

to_bitseg([Val|Specs]=F, L, Vt, St) ->
    case is_integer_list(F) of
        true ->
            {Size,Type} = to_bitspecs([]),
            to_bin_element(F, Size, Type, L, Vt, St);
        false ->
            {Size,Type} = to_bitspecs(Specs),
            to_bin_element(Val, Size, Type, L, Vt, St)
    end;
to_bitseg(Val, L, Vt, St) ->
    {Size,Type} = to_bitspecs([]),
    to_bin_element(Val, Size, Type, L, Vt, St).

to_bin_element(Val, Size, {Type,Unit,Sign,End}, L, Vt, St0) ->
    {Eval,St1} = to_expr(Val, L, Vt, St0),
    {Esiz,St2} = to_bin_size(Size, L, Vt, St1),
    {{bin_element,L,Eval,Esiz,[Type,to_bin_unit(Unit),Sign,End]},St2}.

to_bin_size(default, _, _, St) -> {default,St};
to_bin_size(undefined, _, _, St) -> {default,St};
to_bin_size(Size, L, Vt, St) -> to_expr(Size, L, Vt, St).

to_bin_unit(default) -> default;
to_bin_unit(Unit) -> {unit,Unit}.

%% to_bitspec(Specs) -> {Size,Type}.
%%  Get the error handling as we want it.

to_bitspecs(Ss) ->
    case lfe_bits:get_bitspecs(Ss) of
        {ok,Sz,Ty} -> {Sz,Ty};
        {error,Error} -> erlang:error(Error)
    end.

new_to_var(#to{vc=C}=St) ->
    V = list_to_atom(lists:concat(["___",C,"___"])),
    {V,St#to{vc=C+1}}.

%% to_pat(Pattern, LineNumber, VarTable, State) -> {Pattern,VarTable,State}.

to_pat([], L, Vt, St) -> {{nil,L},Vt,St};
to_pat(I, L, Vt, St) when is_integer(I) -> {{integer,L,I},Vt,St};
to_pat(F, L, Vt, St) when is_float(F) -> {{float,L,F},Vt,St};
to_pat(V, L, Vt, St) when is_atom(V) ->         %Unquoted atom
    to_pat_var(V, L, Vt, St);
to_pat(T, L, Vt, St) when is_tuple(T) ->        %Tuple literal
    Es = to_lit_list(tuple_to_list(T), L),
    {{tuple,L,Es},Vt,St};
to_pat(?Q(P), L, Vt, St) ->                     %Everything quoted here
    {to_lit(P, L),Vt,St};
to_pat([cons,H,T], L, Vt0, St0) ->
    {[Eh,Et],Vt1,St1} = to_pat_list([H,T], L, Vt0, St0),
    {{cons,L,Eh,Et},Vt1,St1};
to_pat([list|Es], L, Vt, St) ->
    Fun = fun (E, {Tail,Vt0,St0}) ->
          {Ee,Vt1,St1} = to_pat(E, L, Vt0, St0),
          {{cons,L,Ee,Tail},Vt1,St1}
      end,
    foldr(Fun, {{nil,L},Vt,St}, Es);
to_pat(['list*'|Es], L, Vt, St) ->
    to_pat_list_s(fun to_pat/4, L, Vt, St, Es);
to_pat([tuple|Es], L, Vt0, St0) ->
    {Ees,Vt1,St1} = to_pat_list(Es, L, Vt0, St0),
    {{tuple,L,Ees},Vt1,St1};
to_pat([binary|Segs], L, Vt0, St0) ->
    {Esegs,Vt1,St1} = to_pat_bitsegs(Segs, L, Vt0, St0),
    {{bin,L,Esegs},Vt1,St1};
to_pat(['=',P1,P2], L, Vt0, St0) ->        %Alias
    {Ep1,Vt1,St1} = to_pat(P1, L, Vt0, St0),
    {Ep2,Vt2,St2} = to_pat(P2, L, Vt1, St1),
    {{match,L,Ep1,Ep2},Vt2,St2}.

to_pat_list(Ps, L, Vt, St) ->
    Fun = fun (P, Vt0, St0) -> to_pat(P, L, Vt0, St0) end,
    mapfoldl2(Fun, Vt, St, Ps).

to_pat_var(V, L, Vt, St0) ->
    case orddict:is_key(V, Vt) of
    true ->
        {V1,St1} = new_to_var(St0),
        {{var,L,V1},orddict:store(V, V1, Vt),St1};
    false ->
        {{var,L,V},orddict:store(V, V, Vt),St0}
    end.

%% to_pat_bitsegs(Segs, LineNumber, VarTable, State) -> {Segs,State}.
%% This gives a verbose value, but it is correct.

to_pat_bitsegs(Ss, L, Vt, St) ->
    Fun = fun (S, Vt0, St0) -> to_pat_bitseg(S, L, Vt0, St0) end,
    mapfoldl2(Fun, Vt, St, Ss).

to_pat_bitseg([Val|Specs]=F, L, Vt, St) ->
    case is_integer_list(F) of
        true ->
            {Size,Type} = to_bitspecs([]),
            to_pat_bin_element(F, Size, Type, L, Vt, St);
        false ->
            {Size,Type} = to_bitspecs(Specs),
            to_pat_bin_element(Val, Size, Type, L, Vt, St)
    end;
to_pat_bitseg(Val, L, Vt, St) ->
    {Size,Type} = to_bitspecs([]),
    to_pat_bin_element(Val, Size, Type, L, Vt, St).

to_pat_bin_element(Val, Size, {Type,Unit,Sign,End}, L, Vt0, St0) ->
    {Eval,Vt1,St1} = to_pat(Val, L, Vt0, St0),
    {Esiz,Vt2,St2} = to_pat_bin_size(Size, L, Vt1, St1),
    {{bin_element,L,Eval,Esiz,[Type,to_bin_unit(Unit),Sign,End]},Vt2,St2}.

to_pat_bin_size(default, _, Vt, St) -> {default,Vt,St};
to_pat_bin_size(undefined, _, Vt, St) -> {default,Vt,St};
to_pat_bin_size(Size, L, Vt, St) -> to_pat(Size, L, Vt, St).

to_lit([], L) -> {nil,L};
to_lit(I, L) when is_integer(I) -> {integer,L,I};
to_lit(F, L) when is_float(F) -> {float,L,F};
to_lit(V, L) when is_atom(V) -> {atom,L,V};    %Quoted atom here!
to_lit([H|T], L) ->
    {cons,L,to_lit(H, L),to_lit(T, L)};
to_lit(T, L) when is_tuple(T) ->
    {tuple,L,to_lit_list(tuple_to_list(T), L)}.

to_lit_list(Ps, L) -> [ to_lit(P, L) || P <- Ps ].

is_integer_list([I|Is]) when is_integer(I) ->
    is_integer_list(Is);
is_integer_list([]) -> true;
is_integer_list(_) -> false.

%% mapfoldl2(Fun, Acc1, Acc2, List) -> {List,Acc1,Acc2}.
%%  Like normal mapfoldl but with 2 accumulators.

mapfoldl2(Fun, A0, B0, [E0|Es0]) ->
    {E1,A1,B1} = Fun(E0, A0, B0),
    {Es1,A2,B2} = mapfoldl2(Fun, A1, B1, Es0),
    {[E1|Es1],A2,B2};
mapfoldl2(_, A, B, []) -> {[],A,B}.
