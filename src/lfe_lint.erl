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

%% File    : lfe_lint.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang syntax checker.

-module(lfe_lint).

-export([module/1,module/2,form/1,expr/1,expr/2,pattern/1,pattern/2,
         format_error/1]).

-import(lfe_env, [new/0,is_vbound/2,is_fbound/3,is_gbound/3,
                  add_vbinding/3,add_fbinding/4,add_ibinding/5]).

-import(lfe_lib, [is_erl_bif/2,is_guard_bif/2,
                  is_symb_list/1,is_proper_list/1]).

%% -compile(export_all).

-import(lists, [member/2,sort/1,all/2,foldl/3,foldr/3,foreach/2,mapfoldl/3]).
-import(ordsets, [add_element/2,from_list/1,is_element/2,
                  union/1,union/2,intersection/2,subtract/2]).
-import(orddict, [store/3,find/2]).

-record(lint, {module=[],            %Module name
               pars=none,            %Module parameters
               extd=[],              %Extends
               exps=[],              %Exports
               imps=[],              %Imports
               pref=[],              %Prefixes
               funcs=[],             %Defined functions
               env=[],               %Top-level environment
               errors=[],            %Errors
               warnings=[],          %Warnings
               line=[],              %Current line
               func=[]}).            %Current function

%% Errors.
format_error({bad_mdef,D}) ->
    lfe_io:format1("bad module definition: ~w", [D]);
format_error(bad_extends) -> "bad extends";
format_error(bad_funcs) -> "bad function list";
format_error(bad_body) -> "bad body";
format_error(bad_clause) -> "bad clause";
format_error(bad_args) -> "bad arguments";
format_error(bad_gargs) -> "bad guard arguments";
format_error(bad_alias) -> "bad alias";
format_error(bad_arity) -> "head arity mismatch";
format_error({bad_attribute,A}) ->
    lfe_io:format1("bad attribute: ~w", [A]);
format_error({bad_form,Type}) ->
    lfe_io:format1("bad form: ~w", [Type]);
format_error({bad_gform,Type}) ->
    lfe_io:format1("bad guard form: ~w", [Type]);
format_error({bad_pat,Type}) ->
    lfe_io:format1("bad pattern: ~w", [Type]);
format_error({unbound_symb,S}) ->
    lfe_io:format1("unbound symbol: ~w", [S]);
format_error({unbound_func,F}) ->
    lfe_io:format1("unbound function: ~w", [F]);
format_error({multi_var,S}) ->
    lfe_io:format1("multiple variable: ~w", [S]);
format_error({redef_fun,F}) ->
    lfe_io:format1("redefining function: ~w", [F]);
format_error(illegal_literal) -> "illegal literal";
format_error(illegal_pattern) -> "illegal pattern";
format_error(illegal_guard) -> "illegal guard";
format_error(illegal_bitseg) -> "illegal bit segment";
format_error(illegal_mapkey) -> "illegal map key";
format_error({undefined_bittype,S}) ->
    lfe_io:format1("bit type ~w undefined", [S]);
format_error(bittype_unit) ->
    "bit unit size can only be specified together with size";
format_error(illegal_bitsize) -> "illegal bit size";
format_error({deprecated,What}) ->
    lfe_io:format1("deprecated ~s", [What]);
format_error(unknown_form) -> "unknown form".

%% expr(Expr) -> {ok,[Warning]} | {error,[Error],[Warning]}.
%% expr(Expr, Env) -> {ok,[Warning]} | {error,[Error],[Warning]}.

expr(E) -> expr(E, lfe_env:new()).

expr(E, Env) ->
    St0 = #lint{},
    St1 = check_expr(E, Env, 1, St0),
    return_status(St1).

%% pattern(Pattern) -> {ok,[Warning]} | {error,[Error],[Warning]}.
%% pattern(Pattern, Env) -> {ok,[Warning]} | {error,[Error],[Warning]}.

pattern(P) -> pattern(P, lfe_env:new()).

pattern(P, Env) ->
    St0 = #lint{},
    {_,St1} = pattern(P, Env, 1, St0),
    return_status(St1).

%% form(Form) -> {ok,[Warning]} | {error,[Error],[Warning]}.

form(F) ->
    module([{['define-module',dummy],1},
            {F,2}]).

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
    case is_symb_list(Mod) of                   %Parameterised module
        true ->
            {Vs,St1} = check_lambda_args(tl(Mod), L, St0),
            %% Everything into State.
            {[],check_mdef(Mdef, L, St1#lint{module=hd(Mod),pars=Vs})};
        false when is_atom(Mod) ->              %Normal module
            %% Everything into State.
            {[],check_mdef(Mdef, L, St0#lint{module=Mod,pars=none})};
        false ->                                %Bad module name
            {[],bad_mdef_error(L, name, St0)}
    end;
collect_form(_, L, #lint{module=[]}=St) ->
    %% Set module name so this only triggers once.
    {[],bad_mdef_error(L, name, St#lint{module='-no-module-'})};
collect_form(['extend-module'|Mdef], L, St) ->
    {[],check_mdef(Mdef, L, St)};
collect_form(['define-function',Func,Body], L, St) ->
    case Body of
        [lambda|_] when is_atom(Func) ->
            {[{Func,Body,L}],St};
        ['match-lambda'|_] when is_atom(Func) ->
            {[{Func,Body,L}],St};
        _ -> {[],bad_form_error(L, 'define-function', St)}
    end;
collect_form(_, L, St) ->
    {[],add_error(L, unknown_form, St)}.

check_mdef([[export,all]|Mdef], L, St) ->       %Pass 'all' along
    check_mdef(Mdef, L, St#lint{exps=all});
check_mdef([[export|Es]|Mdef], L, St) ->
    case is_flist(Es) of
        {yes,Fs} ->
            Exps = add_exports(St#lint.exps, Fs),
            check_mdef(Mdef, L, St#lint{exps=Exps});
        no ->
            check_mdef(Mdef, L, bad_mdef_error(L, export, St))
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
check_mdef(_, L, St) -> bad_mdef_error(L, form, St).

check_imports(Is, L, St) ->
    check_foreach(fun (I, S) -> check_import(I, L, S) end,
                  fun (S) -> import_error(L, S) end, St, Is).

check_import([from,Mod|Fs], L, St) when is_atom(Mod) ->
    Check = fun ([F,A], Imps, S) when is_atom(F), is_integer(A) ->
                    {store({F,A}, F, Imps),S};
                (_, Imps, S) -> {Imps,bad_mdef_error(L, from, S)}
            end,
    check_import(Check, Mod, L, St, Fs);
check_import([rename,Mod|Rs], L, St) when is_atom(Mod) ->
    Check = fun ([[F,A],R], Imps, S) when is_atom(F),
                                          is_integer(A),
                                          is_atom(R) ->
                    {store({F,A}, R, Imps),S};
                (_, Imps, S) -> {Imps,bad_mdef_error(L, rename, S)}
            end,
    check_import(Check, Mod, L, St, Rs);
check_import([prefix,Mod,Pre], L, St) when is_atom(Mod), is_atom(Pre) ->
    Pstr = atom_to_list(Pre),
    case find(Pstr, St#lint.pref) of
        {ok,_} -> bad_mdef_error(L, prefix, St);
        error ->
            Pref = store(Pstr, Mod, St#lint.pref),
            St#lint{pref=Pref}
    end;
check_import(_, L, St) -> import_error(L, St).

check_import(Check, Mod, L, St0, Fs) ->
    Imps0 = safe_fetch(Mod, St0#lint.imps, []),
    {Imps1,St1} = foldl_form(Check, import, L, Imps0, St0, Fs),
    St1#lint{imps=store(Mod, Imps1, St1#lint.imps)}.

import_error(L, St) -> bad_mdef_error(L, import, St).

is_flist(Fs) -> is_flist(Fs, []).

is_flist([[F,Ar]|Fs], Funcs) when is_atom(F), is_integer(Ar), Ar >= 0 ->
    is_flist(Fs, add_element({F,Ar}, Funcs));
is_flist([], Funcs) -> {yes,Funcs};
is_flist(_, _) -> no.

%% check_module(FuncBindings, State) -> State.
%%  Do all the actual work checking a module.

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
                 end, lfe_env:new(), St#lint.imps),
    %% Basic predefines
    Predefs0 = [{module_info,[lambda,[],[quote,dummy]],1},
                {module_info,[lambda,[x],[quote,dummy]],1}],
    Exps0 = [{module_info,0},{module_info,1}],
    %% Now handle parameterised module.
    case St#lint.pars of
        none ->                                 %Normal module
            {Predefs0,Env0,
             St#lint{exps=add_exports(St#lint.exps, Exps0)}};
        Ps0 ->                                  %Parameterised module
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

check_exports(all, _, St) -> St;                %All is all
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
check_expr([quote,Lit], Env, L, St) -> literal(Lit, Env, L, St);
check_expr([cons|[_,_]=As], Env, L, St) -> check_args(As, Env, L, St);
check_expr([car,E], Env, L, St) -> check_expr(E, Env, L, St);
check_expr([cdr,E], Env, L, St) -> check_expr(E, Env, L, St);
check_expr([list|As], Env, L, St) -> check_args(As, Env, L, St);
check_expr([tuple|As], Env, L, St) -> check_args(As, Env, L, St);
check_expr([binary|Segs], Env, L, St) -> expr_bitsegs(Segs, Env, L, St);
check_expr([map|As], Env, L, St) -> expr_map(As, Env, L, St);
check_expr(['mref',Map,K], Env, L, St) ->
    expr_get_map(Map, K, Env, L, St);
check_expr(['mset',Map|As], Env, L, St) ->
    expr_set_map(Map, As, Env, L, St);
check_expr(['mupd',Map|As], Env, L, St) ->
    expr_update_map(Map, As, Env, L, St);
check_expr(['map-get',Map,K], Env, L, St) ->
    check_expr(['mref',Map,K], Env, L, St);
check_expr(['map-set',Map|As], Env, L, St) ->
    check_expr(['mset',Map|As], Env, L, St);
check_expr(['map-update',Map|As], Env, L, St) ->
    check_expr(['mupd',Map|As], Env, L, St);
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
check_expr(['progn'|B], Env, L, St) ->
    check_body(B, Env, L, St);
check_expr(['if'|B], Env, L, St) ->
    check_if(B, Env, L, St);
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
check_expr([Fun|As], Env, L, St0) when is_atom(Fun) ->
    St1 = check_args(As, Env, L, St0),          %Check arguments first
    %% Here we are not interested in HOW fun is associated to a
    %% function, just that it is.
    case is_fbound(Fun, safe_length(As), Env) of
        true -> St1;
        false -> add_error(L, {unbound_func,{Fun,safe_length(As)}}, St1)
    end;
check_expr([_|As]=S, Env, L, St0) ->            %Test if literal string
    case is_posint_list(S) of
        true -> St0;
        false ->
            %% Function here is an expression, report error and check args.
            St1 = bad_form_error(L, application, St0),
            check_args(As, Env, L, St1)
    end;
check_expr(Symb, Env, L, St) when is_atom(Symb) ->
    check_symb(Symb, Env, L, St);
check_expr(Lit, Env, L, St) ->                  %Everything else is a literal
    literal(Lit, Env, L, St).

%% check_symb(Symbol, Env, Line, State) -> State.
%%  Check if Symbol is bound.

check_symb(Symb, Env, L, St) ->
    case is_vbound(Symb, Env) of
        true -> St;
        false -> add_error(L, {unbound_symb,Symb}, St)
    end.

%% check_body(Body, Env, Line, State) -> State.
%% Check the calls in a body. A body is a proper list of calls. Env is
%% the set of known bound variables.

check_body(Body, Env, L, St) ->
    check_foreach(fun (E, S) -> check_expr(E, Env, L, S) end,
                  fun (S) -> add_error(L, bad_body, S) end,
                  St, Body).

%% check_body(Body, Env, L, St) ->
%%     %% check_body(fun check_exprs/4, Env, L, St, Body).
%%     case is_proper_list(Body) of
%%         true -> check_exprs(Body, Env, L, St);
%%         false -> add_error(L, bad_body, St)
%%     end.

%% check_args(Args, Env, Line, State) -> State.
%% Check the expressions in an argument list.

check_args(Args, Env, L, St) ->
    check_foreach(fun (A, S) -> check_expr(A, Env, L, S) end,
                  fun (S) -> add_error(L, bad_args, S) end,
                  St, Args).

%% check_args(Args, Env, L, St) ->
%%     case is_proper_list(Args) of
%%         true -> check_exprs(Args, Env, L, St);
%%         false -> add_error(L, bad_args, St)
%%     end.

%% check_exprs(Exprs, Env, Line, State) -> State.
%% Check a list of expressions. We know it's a proper list.

check_exprs(Es, Env, L, St) ->
    foldl(fun (E, S) -> check_expr(E, Env, L, S) end, St, Es).

%% expr_bitsegs(BitSegs, Env, Line, State) -> State.

expr_bitsegs(Segs, Env, L, St0) ->
    foreach_form(fun (S, St) -> bitseg(S, Env, L, St, fun check_expr/4) end,
         binary, L, St0, Segs).

%% bitseg(BitSeg, Env, Line, State) -> State.
%% bitspecs(BitSpecs, Env, Line, State) -> State.
%% bit_size(Size, Type, Env, Line, State) -> State.
%% Functions for checking expression bitsegments.

bitseg([Val|Specs]=Seg, Env, L, St0, Check) ->
    case is_posint_list(Seg) of                 %Is bitseg a string?
        true -> St0;                            %A string
        false ->                                %A value and spec
            St1 = bitspecs(Specs, Env, L, St0, Check),
            case is_posint_list(Val) of         %Is Val a string?
                true -> St1;
                false -> Check(Val, Env, L, St1)
            end
    end;
bitseg(Val, Env, L, St, Check) ->
    Check(Val, Env, L, St).

bitspecs(Specs, Env, L, St, Check) ->
    case lfe_bits:get_bitspecs(Specs) of
        {ok,Sz,Ty} -> bit_size(Sz, Ty, Env, L, St, Check);
        {error,E} -> add_error(L, E, St)
    end.

%% Catch the case where size was explicitly given as 'undefined' or
%% 'all' for the wrong type.

bit_size(all, {Ty,_,_,_}, _, L, St, _) ->
    if Ty =:= binary -> St;
       true -> add_error(L, illegal_bitsize, St)
    end;
bit_size(undefined, {Ty,_,_,_}, _, L, St, _) ->
    if Ty =:= utf8; Ty =:= utf16; Ty =:= utf32 -> St;
       true -> add_error(L, illegal_bitsize, St)
    end;
bit_size(Sz, _, Env, L, St, Check) -> Check(Sz, Env, L, St).

is_posint_list([I|Is]) when is_integer(I), I >= 0 ->
    is_posint_list(Is);
is_posint_list([]) -> true;
is_posint_list(_) -> false.

%% expr_map(Pairs, Env, Line, State) -> State.
%% expr_get_map(Map, Key, Env, Line, State) -> State.
%% expr_set_map(Map, Pairs, Line, State) -> State.
%% expr_update_map(Args, Pairs, Line, State) -> State.
%%  Functions for checking maps, these always return errors if system
%%  does not support maps.

-ifdef(HAS_MAPS).
expr_map(Pairs, Env, L, St) ->
    expr_map_pairs(Pairs, Env, L, St).

expr_get_map(Map, Key, Env, L, St0) ->
    St1 = check_expr(Map, Env, L, St0),
    map_key(Key, Env, L, St1).

expr_set_map(Map, Pairs, Env, L, St0) ->
    St1 = check_expr(Map, Env, L, St0),
    expr_map_pairs(Pairs, Env, L, St1).

expr_update_map(Map, Pairs, Env, L, St0) ->
    St1 = check_expr(Map, Env, L, St0),
    expr_map_pairs(Pairs, Env, L, St1).

expr_map_pairs([K,V|As], Env, L, St0) ->
    St1 = expr_map_assoc(K, V, Env, L, St0),
    expr_map_pairs(As, Env, L, St1);
expr_map_pairs([],  _, _, St) -> St;
expr_map_pairs(_, _, L, St) ->
    bad_form_error(L, map, St).

expr_map_assoc(K, V, Env, L, St0) ->
    St1 = map_key(K, Env, L, St0),
    check_expr(V, Env, L, St1).

%% map_key(Key, Env, L, State) -> State.
%%  A map key can currently only be a literal.

map_key(Key, _, L, St) ->
    case is_map_key(Key) of
        true -> St;
        false -> add_error(L, illegal_mapkey, St)
    end.

is_map_key([quote,Lit]) -> is_literal(Lit);
is_map_key([_|_]=L) -> is_posint_list(L);       %Literal strings only
is_map_key(E) when is_atom(E) -> false;
is_map_key(Lit) -> is_literal(Lit).
-else.
expr_map(Ps, _, L, St) ->
    add_error(L, {unbound_func,{map,safe_length(Ps)}}, St).

expr_get_map(_, _, _, L, St) ->
    add_error(L, {unbound_func,{'map-get',2}}, St).

expr_set_map(_, Ps, _, L, St) ->
    add_error(L, {unbound_func,{'map-set',safe_length(Ps)+1}}, St).

expr_update_map(_, Ps, _, L, St) ->
    add_error(L, {unbound_func,{'map-update',safe_length(Ps)+1}}, St).
-endif.

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
check_match_lambda(_, _, L, St) ->              %Totally wrong
    bad_form_error(L, 'match-lambda', St).

check_ml_arity([[Pat|_]|Cls], Ar, L, St) ->
    case is_proper_list(Pat) andalso length(Pat) == Ar of
        true -> check_ml_arity(Cls, Ar, L, St);
        false -> add_error(L, bad_arity, St)
    end;
check_ml_arity([], _, _, St) -> St.

check_ml_clauses(Cls, Env, L, St) ->
    %% Sneaky! m-l args a list of patterns so wrap with list and pass
    %% in as one pattern. Have already checked a proper list.
    foreach_form(fun ([As|B], S) -> check_clause([[list|As]|B], Env, L, S) end,
                 'match-lambda', L, St, Cls).

%% check_ml_clauses(Cls, Env, L, St) ->
%%     %% Sneaky! m-l args list of patterns so just pass in as one pattern.
%%     foreach_form(fun (Cl, S) -> check_clause(Cl, Env, L, S) end,
%%           'match-lambda', L, St, Cls).

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
    {Pvs,St1} = foldl_form(Check, 'let', L, [], St0, Vbs),
    check_body(Body, add_vbindings(Pvs, Env), L, St1);
check_let(_, _, L, St) ->
    bad_form_error(L, 'let', St).

%% check_let_vb(VarBind, Env, Line, State) -> {Env,State}.
%% Check a variable binding of form [Pat,[when,Guard],Val] or [Pat,Val].

check_let_vb(Vb, Env, L, St0) ->
    %% Get the environments right here!
    case pattern_guard(Vb, Env, L, St0) of
        {[Val],Pvs,_,St1} ->                    %One value expression only
            {Pvs,check_expr(Val, Env, L, St1)};
        {_,_,_,St1} -> {[],bad_form_error(L, 'let', St1)}
    end.

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
    foldr_form(Check, Type, L, [], St0, Fbs0).  %Preserve order

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
                    end;
                (_, Acc) -> Acc                 %Error here flagged later
            end,
    foldl(Check, {[],St0}, Fbs0).

%% check_if(IfBody, Env, Line, State) -> State.
%% Check form (if Test True [False]).

check_if([Test,True,False], Env, L, St) ->
    check_exprs([Test,True,False], Env, L, St);
check_if([Test,True], Env, L, St) ->
    check_exprs([Test,True], Env, L, St);
check_if(_, _, L, St) ->
    bad_form_error(L, 'if', St).

%% check_case(CaseBody, Env, Line, State) -> State.
%% Check form (case Expr Clause ...), must be at least one clause.

check_case([E|[_|_]=Cls], Env, L, St0) ->
    St1 = check_expr(E, Env, L, St0),
    check_case_clauses(Cls, Env, L, St1);
check_case(_, _, L, St) ->
    bad_form_error(L, 'case', St).

check_case_clauses(Cls, Env, L, St) ->
    foreach_form(fun (Cl, S) -> check_clause(Cl, Env, L, S) end,
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
    {B,_,Env1,St1} = pattern_guard(Cl, Env0, L, St0),
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

%% pattern_guard([Pat{,Guard}|Body], Env, L, State) ->
%%      {Body,PatVars,Env,State}.
%%  Check pattern and guard in a clause. We know there is at least pattern!

pattern_guard([Pat,['when'|G]|Body], Env0, L, St0) ->
    {Pvs,St1} = pattern(Pat, Env0, L, St0),
    Env1 = add_vbindings(Pvs, Env0),
    St2 = check_guard(G, Env1, L, St1),
    {Body,Pvs,Env1,St2};
pattern_guard([Pat|Body], Env0, L, St0) ->
    {Pvs,St1} = pattern(Pat, Env0, L, St0),
    Env1 = add_vbindings(Pvs, Env0),
    {Body,Pvs,Env1,St1}.

%% check_guard(GuardTests, Env, Line, State) -> State.
%% Check a guard.

check_guard(G, Env, L, St) -> check_gbody(G, Env, L, St).

%% check_gbody(Body, Env, Line, State) -> State.
%% Check guard expressions in a body

check_gbody([E|Es], Env, L, St0) ->
    St1 = check_gexpr(E, Env, L, St0),
    check_gbody(Es, Env, L, St1);
check_gbody([], _, _, St) -> St;
check_gbody(_, _, L, St) -> illegal_guard_error(L, St).

%% check_gexpr(Call, Env, Line, State) -> State.
%% Check a guard expression. This is a restricted body expression.

%% Check the Core data special cases.
check_gexpr([quote,Lit], Env, L, St) -> literal(Lit, Env, L, St);
check_gexpr([cons|[_,_]=As], Env, L, St) -> check_gargs(As, Env, L, St);
check_gexpr([car,E], Env, L, St) -> check_gexpr(E, Env, L, St);
check_gexpr([cdr,E], Env, L, St) -> check_gexpr(E, Env, L, St);
check_gexpr([list|As], Env, L, St) -> check_gargs(As, Env, L, St);
check_gexpr([tuple|As], Env, L, St) -> check_gargs(As, Env, L, St);
check_gexpr([binary|Segs], Env, L, St) -> gexpr_bitsegs(Segs, Env, L, St);
check_gexpr([map|As], Env, L, St) -> gexpr_map(As, Env, L, St);
%% check_gexpr(['mref',Map,K], Env, L, St) ->
%%     gexpr_get_map(Map, K, Env, L, St);
check_gexpr(['mset',Map|As], Env, L, St) ->
    gexpr_set_map(Map, As, Env, L, St);
check_gexpr(['mupd',Map|As], Env, L, St) ->
    gexpr_update_map(Map, As, Env, L, St);
check_gexpr(['map-get',Map,K], Env, L, St) ->
    check_gexpr(['mref',Map,K], Env, L, St);
check_gexpr(['map-set',Map|As], Env, L, St) ->
    check_expr(['mset',Map|As], Env, L, St);
check_gexpr(['map-update',Map|As], Env, L, St) ->
    check_gexpr(['mupd',Map|As], Env, L, St);
%% Check the Core closure special forms.
%% Check the Core control special forms.
check_gexpr(['progn'|B], Env, L, St) -> check_gbody(B, Env, L, St);
check_gexpr(['if'|B], Env, L, St) -> check_gif(B, Env, L, St);
check_gexpr([call,[quote,erlang],[quote,Fun]|As], Env, L, St) ->
    check_gexpr([Fun|As], Env, L, St);          %Pass the buck
check_gexpr([call|_], _, L, St) ->              %Other calls not allowed
    illegal_guard_error(L, St);
%% Finally the general case.
check_gexpr([Fun|As], Env, L, St0) when is_atom(Fun) ->
    St1 = check_gargs(As, Env, L, St0),
    %% Here we are not interested in HOW fun is associated to a
    %% function, just that it is.
    case is_gbound(Fun, safe_length(As), Env) of
        true -> St1;
        false -> illegal_guard_error(L, St1)
    end;
check_gexpr([_|As]=S, Env, L, St0) ->            %Test if literal string
    case is_posint_list(S) of
        true -> St0;
        false ->
            %% Function here is an expression, report error and check args.
            St1 = bad_gform_error(L, application, St0),
            check_gargs(As, Env, L, St1)
    end;
check_gexpr(Symb, Env, L, St) when is_atom(Symb) ->
    check_symb(Symb, Env, L, St);
check_gexpr(Lit, Env, L, St) ->                 %Everything else is a literal
    literal(Lit, Env, L, St).

%% check_gargs(Args, Env, Line, State) -> State.
%% check_gexprs(Exprs, Env, Line, State) -> State.
%% The guard counter parts. Check_gexprs assumes a proper list.

check_gargs(Args, Env, L, St) ->
    case is_proper_list(Args) of
        true -> check_gexprs(Args, Env, L, St);
        false -> add_error(L, bad_gargs, St)
    end.

check_gexprs(Es, Env, L, St) ->
    foldl(fun (E, S) -> check_gexpr(E, Env, L, S) end, St, Es).

%% check_gif(IfBody, Env, Line, State) -> State.
%% Check guard form (if Test True [False]).

check_gif([Test,True,False], Env, L, St) ->
    check_gexprs([Test,True,False], Env, L, St);
check_gif([Test,True], Env, L, St) ->
    check_gexprs([Test,True], Env, L, St);
check_gif(_, _, L, St) ->
    bad_gform_error(L, 'if', St).               %Signal as guard error.

%% gexpr_bitsegs(BitSegs, Env, Line, State) -> State.

gexpr_bitsegs(Segs, Env, L, St0) ->
    check_foreach(fun (S, St) -> bitseg(S, Env, L, St, fun check_gexpr/4) end,
                  fun (St) -> bad_gform_error(L, binary, St) end, St0, Segs).

%% gexpr_map(Pairs, Env, Line, State) -> State.
%% gexpr_set_map(Map, Pairs, Env, Line, State) -> State.
%% gexpr_update_map(Map, Pairs, Env, Line, State) -> State.
%%  Functions for checking maps, these always return errors if system
%%  does not support maps.

-ifdef(HAS_MAPS).
gexpr_map(Pairs, Env, L, St) ->
    gexpr_map_pairs(Pairs, Env, L, St).

gexpr_set_map(Map, Pairs, Env, L, St0) ->
    St1 = check_gexpr(Map, Env, L, St0),
    gexpr_map_pairs(Pairs, Env, L, St1).

gexpr_update_map(Map, Pairs, Env, L, St0) ->
    St1 = check_gexpr(Map, Env, L, St0),
    gexpr_map_pairs(Pairs, Env, L, St1).

gexpr_map_pairs([K,V|As], Env, L, St0) ->
    St1 = gexpr_map_assoc(K, V, Env, L, St0),
    gexpr_map_pairs(As, Env, L, St1);
gexpr_map_pairs([],  _, _, St) -> St;
gexpr_map_pairs(_, _, L, St) ->
    bad_form_error(L, map, St).

gexpr_map_assoc(K, V, Env, L, St0) ->
    St1 = map_key(K, Env, L, St0),
    check_gexpr(V, Env, L, St1).
-else.
gexpr_map(Ps, _, L, St) ->
    add_error(L, {unbound_func,{map,safe_length(Ps)}}, St).

gexpr_set_map(_, Ps, _, L, St) ->
    add_error(L, {unbound_func,{'map-set',safe_length(Ps)+1}}, St).

gexpr_update_map(_, Ps, _, L, St) ->
    add_error(L, {unbound_func,{'map-update',safe_length(Ps)+1}}, St).
-endif.

%% pattern(Pattern, Env, L, State) -> {PatVars,State}.
%% pattern(Pattern, PatVars, Env, L, State) -> {PatVars,State}.
%%  Return the *set* of Variables in Pattern.  Patterns are
%%  complicated by the fact that we don't allow multiple occurrence of
%%  variables, this is an error. Explicit guards tests are
%%  necessary. So we need to carry around the current pattern
%%  variables as well as the environment.

pattern(Pat, Env, L, St) ->
    %% io:fwrite("pat: ~p\n", [Pat]),
    %% pattern/5 should never fail!
    pattern(Pat, [], Env, L, St).
%%     try
%%     pattern(Pat, [], Env, L, St)
%%     catch
%%     _:_ -> {[],add_error(L, illegal_pattern, St)}
%%     end.

pattern([quote,Lit], Pvs, Env, L, St) ->
    {Pvs,literal(Lit, Env, L, St)};
pattern(['=',P1,P2], Pvs0, Env, L, St0) ->
    %% Must check patterns together as same variable can occur
    %% in both branches.
    {Pvs1,St1} = pattern(P1, Pvs0, Env, L, St0),
    {Pvs2,St2} = pattern(P2, Pvs1, Env, L, St1),
    St3 = case is_pat_alias(P1, P2) of
              true -> St2;                      %Union of variables now visible
              false -> add_error(L, bad_alias, St2)
          end,
    {Pvs2,St3};
pattern([cons,H,T], Pvs0, Env, L, St0) ->       %Explicit cons constructor
    {Pvs1,St1} = pattern(H, Pvs0, Env, L, St0),
    pattern(T, Pvs1, Env, L, St1);
pattern([list|Ps], Pvs, Env, L, St) ->          %Explicit list constructor
    pat_list(Ps, Pvs, Env, L, St);
pattern([tuple|Ps], Pvs, Env, L, St) ->         %Tuple elements
    pat_list(Ps, Pvs, Env, L, St);
pattern([binary|Segs], Pvs, Env, L, St) ->
    pat_binary(Segs, Pvs, Env, L, St);
pattern([map|As], Pvs, Env, L, St) ->
    pat_map(As, Pvs, Env, L, St);
%% Check old no contructor list forms.
pattern([_|_]=List, Pvs0, Env, L, St0) ->
    St1 = add_warning(L, {deprecated,"pattern"}, St0),
    pat_list(List, Pvs0, Env, L, St1);
%% pattern([_|_], Pvs, _, L, St) ->
%%     {Pvs,add_error(L, illegal_pattern, St)};
pattern([], Pvs, _, _, St) -> {Pvs,St};
pattern(Symb, Pvs, _, L, St) when is_atom(Symb) ->
    pat_symb(Symb, Pvs, L, St);
pattern(Lit, Pvs, Env, L, St) ->
    {Pvs,literal(Lit, Env, L, St)}.             %Everything else is a literal

pat_list([P|Ps], Pvs0, Env, L, St0) ->
    {Pvs1,St1} = pattern(P, Pvs0, Env, L, St0),
    pat_list(Ps, Pvs1, Env, L, St1);
pat_list([], Pvs, _, _, St) -> {Pvs,St};
pat_list(_, Pvs, _, L, St) ->
    {Pvs,add_error(L, illegal_pattern, St)}.

pat_symb('_', Pvs, _, St) -> {Pvs,St};          %Don't care variable
pat_symb(Symb, Pvs, L, St) ->
    case is_element(Symb, Pvs) of
        true -> {Pvs,multi_var_error(L, Symb, St)};
        false -> {add_element(Symb, Pvs),St}
    end.

%% is_pat_alias(Pattern, Pattern) -> true | false.
%%  Check if two aliases are compatible. Note that binaries can never
%%  be aliased, this is from erlang.

is_pat_alias([quote,P1], [quote,P2]) -> P1 =:= P2;
is_pat_alias([tuple|Ps1], [tuple|Ps2]) ->
    is_pat_alias_list(Ps1, Ps2);
%% is_pat_alias([tuple|Ps1], P2) when is_tuple(P2) ->
%%     is_pat_alias_list(Ps1, tuple_to_list(P2));
%% is_pat_alias(P1, [tuple|Ps2]) when is_tuple(P1) ->
%%     is_pat_alias_list(tuple_to_list(P1), Ps2);
is_pat_alias([binary|_], [binary|_]) -> false;
is_pat_alias([cons,H1,T1], [cons,H2,T2]) ->
    is_pat_alias(H1, H2) andalso is_pat_alias(T1, T2);
is_pat_alias([cons,H1,T1], [list,H2|T2]) ->
    is_pat_alias(H1, H2) andalso is_pat_alias(T1, [list|T2]);
is_pat_alias([list|Ps1], [list|Ps2]) ->
    is_pat_alias_list(Ps1, Ps2);
is_pat_alias([list,H1|T1], [cons,H2,T2]) ->
    is_pat_alias(H1, H2) andalso is_pat_alias([list|T1], T2);
%% Check against old no contructor list forms.
is_pat_alias([list|_]=P1, P2) when is_list(P2) ->
    is_pat_alias(P1, [list|P2]);
is_pat_alias([cons,_,_]=P1, [H2|T2]) ->
    is_pat_alias(P1, [cons,H2,T2]);
is_pat_alias(P1, [list|_]=P2) when is_list(P1) ->
    is_pat_alias([list|P1], P2);
is_pat_alias([H1|T1], [cons,_,_]=P2) ->
    is_pat_alias([cons,H1,T1], P2);
%% Check old against old no constructor list forms.
is_pat_alias([P1|Ps1], [P2|Ps2]) ->
    is_pat_alias(P1, P2) andalso is_pat_alias(Ps1, Ps2);
is_pat_alias(P1, _) when is_atom(P1) -> true;   %Variable
is_pat_alias(_, P2) when is_atom(P2) -> true;
is_pat_alias(P1, P2) -> P1 =:= P2.              %Atomic

is_pat_alias_list([P1|Ps1], [P2|Ps2]) ->
    is_pat_alias(P1, P2) andalso is_pat_alias_list(Ps1, Ps2);
is_pat_alias_list([], []) -> true;
is_pat_alias_list(_, _) -> false.

%% pat_binary(BitSegs, PatVars, Env, Line, State) -> {PatVars,State}.
%% pat_bitsegs(BitSegs, BitVars, PatVars, Env, Line, State) ->
%%     {BitVars,PatVars,State}.
%% pat_bitseg(BitSeg, BitVars, PatVars, Env, Line, State) ->
%%     {BitVars,PatVars,State}.
%% pat_bitspecs(BitSpecs, BitVars, PatVars, Env, Line, State) -> State.
%% pat_bit_size(Size, Type, BitVars, PatVars, Env, Line, State) -> State.
%% pat_bit_expr(BitElement, BitVars, PatVars, Env, Line, State) ->
%%     {BitVars,PatVars,State}.
%%  Functions for checking pattern bitsegments. This gets a bit
%%  complex as we allow using values from left but only as sizes, no
%%  implicit equality checks so multiple pattern variables are an
%%  error. We only update BitVars during the match.

pat_binary(Segs, Pvs, Env, L, St) ->
    pat_bitsegs(Segs, [], Pvs, Env, L, St).

pat_bitsegs(Segs, Bvs0, Pvs, Env, L, St0) ->
    {Bvs1,St1} =
        check_foldl(fun (Seg, Bvs, St) ->
                            pat_bitseg(Seg, Bvs, Pvs, Env, L, St)
                    end,
                    fun (St) -> bad_pat_error(L, binary, St) end,
                    Bvs0, St0, Segs),
    {union(Bvs1, Pvs),St1}.                     %Add bitvars to patvars

pat_bitseg([Pat|Specs]=Seg, Bvs, Pvs, Env, L, St0) ->
    case is_posint_list(Seg) of                 %Is bitseg a string?
        true -> {Bvs,St0};                      %A string
        false ->                                %A pattern and spec
            St1 = pat_bitspecs(Specs, Bvs, Pvs, Env, L, St0),
            case is_posint_list(Pat) of         %Is Pat a string?
                true -> {Bvs,St1};
                false -> pat_bit_expr(Pat, Bvs, Pvs, Env, L, St1)
            end
    end;
pat_bitseg(Pat, Bvs, Pvs, Env, L, St) ->
    pat_bit_expr(Pat, Bvs, Pvs, Env, L, St).

pat_bitspecs(Specs, Bvs, Pvs, Env, L, St) ->
    case lfe_bits:get_bitspecs(Specs) of
        {ok,Sz,Ty} -> pat_bit_size(Sz, Ty, Bvs, Pvs, Env, L, St);
        {error,E} -> add_error(L, E, St)
    end.

%% Catch the case where size was explicitly given as 'undefined' or
%% 'all' for the wrong type.

pat_bit_size(all, {Ty,_,_,_}, _, _, _, L, St) ->
    if Ty =:= binary -> St;
       true -> add_error(L, illegal_bitsize, St)
    end;
pat_bit_size(undefined, {Ty,_,_,_}, _, _, _, L, St) ->
    if Ty =:= utf8; Ty =:= utf16; Ty =:= utf32 -> St;
       true -> add_error(L, illegal_bitsize, St)
    end;
pat_bit_size(N, _, _, _, _, _, St) when is_integer(N), N > 0 -> St;
pat_bit_size(S, _, Bvs, _, Env, L, St) when is_atom(S) ->
    %% Size must be bound here or occur earlier in binary pattern.
    case is_element(S, Bvs) or is_vbound(S, Env) of
        true -> St;
        false -> add_error(L, {unbound_symb,S}, St)
    end;
pat_bit_size(_, _, _, _, _, L, St) -> add_error(L, illegal_bitsize, St).

pat_bit_expr(N, Bvs, _, _, _, St) when is_number(N) -> {Bvs,St};
pat_bit_expr('_', Bvs, _, _, _, St) -> {Bvs,St};
pat_bit_expr(S, Bvs, Pvs, _, L, St) when is_atom(S) ->
    case is_element(S, Bvs) or is_element(S, Pvs) of
        true -> {Bvs,multi_var_error(L, S, St)};
        false -> {add_element(S, Bvs),St}
    end;
pat_bit_expr(_, Bvs, _, _, L, St) ->
    {Bvs,add_error(L, illegal_bitseg, St)}.

%% pat_map(Args, PatVars, Env, Line, State) -> {PatVars,State}.

-ifdef(HAS_MAPS).
pat_map([K,V|As], Pvs0, Env, L, St0) ->
    {Pvs1,St1} = pat_map_assoc(K, V, Pvs0, Env, L, St0),
    pat_map(As, Pvs1, Env, L, St1);
pat_map([], Pvs, _, _, St) -> {Pvs,St};
pat_map(_, Pvs, _, L, St) ->
    {Pvs,bad_form_error(L, map, St)}.

pat_map_assoc(K, V, Pvs, Env, L, St0) ->
    St1 = map_key(K, Env, L, St0),
    pattern(V, Pvs, Env, L, St1).
-else.
pat_map(_, Pvs, _, L, St) ->
    {Pvs,add_error(L, illegal_pattern, St)}.
-endif.

%% is_literal(Literal) -> true | false.
%% literal(Literal, Env, Line, State) -> State.
%%  Check for legal literals. We have to be extra careful here as the
%%  input can be any forms. We assume that special cases at the top
%%  level, for example atoms as variables and lists as calls, are
%%  handled before the call to literal/4.

literal(Lit, _, L, St) ->
    case is_literal(Lit) of
        true -> St;
        false -> add_error(L, illegal_literal, St)
    end.

is_literal(A) when is_atom(A) -> true;
is_literal(N) when is_number(N) -> true;
is_literal(B) when is_bitstring(B) -> true;
is_literal(List) when is_list(List) ->
    is_lit_list(List);
is_literal(Tup) when is_tuple(Tup) ->
    is_lit_list(tuple_to_list(Tup));
is_literal(Map) ->                              %Handles maps and non-maps
    is_lit_map(Map).

is_lit_list([Lit|Lits]) ->
    is_literal(Lit) andalso is_lit_list(Lits);
is_lit_list([]) -> true;
is_lit_list(Lit) ->
    is_literal(Lit).

-ifdef(HAS_MAPS).
is_lit_map(Map) when is_map(Map) ->
    is_lit_list(maps:to_list(Map));
is_lit_map(_) -> false.
-else.
is_lit_map(_) -> false.
-endif.

%% Functions for checking lists of forms, generate bad_form error if
%% not proper list.

foreach_form(Check, T, L, St, Fs) ->
    check_foreach(Check, fun (S) -> bad_form_error(L, T, S) end, St, Fs).

%% map_form(Check, T, L, St, Fs) ->
%%     check_map(Check, fun (S) -> bad_form_error(L, T, S) end, St, Fs).

foldl_form(Fun, T, L, Acc, St, Fs) ->
    check_foldl(Fun, fun (S) -> bad_form_error(L, T, S) end, Acc, St, Fs).

foldr_form(Fun, T, L, Acc, St, Fs) ->
    check_foldr(Fun, fun (S) -> bad_form_error(L, T, S) end, Acc, St, Fs).

%% check_foreach(Check, Err, State, Forms) -> State.
%% check_map(Check, Err, State, Forms) -> {Results,State}.
%% check_foldl(Check, Err, Acc, State, Forms) -> {Acc,State}.
%% check_foldr(Check, Err, Acc, State, Forms) -> {Acc,State}.
%%  These functions automatically manage a state variable and check for
%%  proper top list. Could easily and clearly be done with a Lisp
%%  macro.

%% Versions which only check for proper top list.
check_foreach(Check, Err, St0, [F|Fs]) ->
    St1 = Check(F, St0),
    check_foreach(Check, Err, St1, Fs);
check_foreach(_, _, St, []) -> St;
check_foreach(_, Err, St, _) -> Err(St).

%% check_map(Check, Err, St0, [F|Fs]) ->
%%     {R,St1} = Check(F, St0),
%%     {Rs,St2} = check_map(Check, Err, St1, Fs),
%%     {[R|Rs],St2};
%% check_map(_, _, St, []) -> {[],St};
%% check_map(_, Err, St, _) -> {[],Err(St)}.

check_foldl(Check, Err, Acc0, St0, [F|Fs]) ->
    {Acc1,St1} = Check(F, Acc0, St0),
    check_foldl(Check, Err, Acc1, St1, Fs);
check_foldl(_, _, Acc, St, []) -> {Acc,St};
check_foldl(_, Err, Acc, St, _) -> {Acc,Err(St)}.

check_foldr(Check, Err, Acc0, St0, [F|Fs]) ->
    {Acc1,St1} = check_foldr(Check, Err, Acc0, St0, Fs),
    Check(F, Acc1, St1);
check_foldr(_, _, Acc, St, []) -> {Acc,St};
check_foldr(_, Err, Acc, St, _) -> {Acc,Err(St)}.

%% Versions which completely wrap with a try. These may catch too much!
%% check_foreach(Fun, Err, St, Fs) ->
%%     try
%%     foldl(Fun, St, Fs)
%%     catch
%%     _:_ -> Err(St)
%%     end.

%% check_map(Fun, Err, St, Fs) ->
%%     try
%%     mapfoldl(Fun, St, Fs)
%%     catch
%%     _:_ -> {[],Err(St)}
%%     end.

%% check_foldl(Fun, Err, Acc, St, Fs) ->
%%     try
%%     foldl(fun (F, {A,S}) -> Fun(F, A, S) end, {Acc,St}, Fs)
%%     catch
%%     _:_ -> {Acc,Err(St)}
%%     end.

%% check_foldr(Fun, Err, St, Acc, Fs) ->
%%     try
%%     foldr(fun (F, {A,S}) -> Fun(F, A, S) end, {Acc,St}, Fs)
%%     catch
%%     _:_ -> {Acc,Err(St)}
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

add_warning(L, W, St) ->
    St#lint{warnings=St#lint.warnings ++ [{L,?MODULE,W}]}.

bad_form_error(L, F, St) ->
    add_error(L, {bad_form,F}, St).

bad_gform_error(L, F, St) ->
    add_error(L, {bad_gform,F}, St).

bad_pat_error(L, F, St) ->
    add_error(L, {bad_pat,F}, St).

bad_mdef_error(L, D, St) ->
    add_error(L, {bad_mdef,D}, St).

multi_var_error(L, V, St) ->
    add_error(L, {multi_var,V}, St).

illegal_guard_error(L, St) ->
    add_error(L, illegal_guard, St).

%% Interface to the binding functions in lfe_lib.
%% These just add arity as a dummy values as we are not interested in
%% value but it might be useful.

add_fbinding(N, A, Env) -> lfe_env:add_fbinding(N, A, A, Env).

add_vbindings(Vs, Env) ->
    foldl(fun (V, E) -> lfe_env:add_vbinding(V, dummy, E) end, Env, Vs).

%% safe_fetch(Key, Dict, Default) -> Value.

safe_fetch(Key, D, Def) ->
    case find(Key, D) of
        {ok,Val} -> Val;
        error -> Def
    end.
