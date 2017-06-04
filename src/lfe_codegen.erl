%% Copyright (c) 2008-2017 Robert Virding
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

%%% File    : lfe_codegen.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang code generator (to core Erlang).

%%% We have to be very careful to generate annotations in exactly the
%%% same way as the erlang compiler does and in the same places.
%%% Dialyzer is very finnicky about this and seriously fails if things
%%% are not as it expects them to be. Note that now the whole
%%% annotation is passed into the constructor functions, not just the
%%% line number.
%%%
%%% We make temporary variables of the form ' <num> ', which while
%%% they are not guaranteed to be unique are pretty unlikely.

-module(lfe_codegen).

-export([module/2]).

-compile(export_all).

-import(lists, [member/2,keysearch/3,reverse/1,
                all/2,map/2,foldl/3,foldr/3,mapfoldl/3,mapfoldr/3,
                concat/1,zipwith/3]).
-import(ordsets, [add_element/2,is_element/2,from_list/1,union/2]).
-import(orddict, [store/3,find/2]).

-import(lfe_env, [new/0,add_env/2,
                  add_vbinding/3,get_vbinding/2,add_fbinding/4,
                  add_ibinding/5,get_gbinding/3]).

-include("lfe_comp.hrl").

%% Define IS_MAP/1 macro for is_map/1 bif.
-ifdef(HAS_MAPS).
-define(IS_MAP(T), is_map(T)).
-else.
-define(IS_MAP(T), false).
-endif.

-define(Q(E), [quote,E]).                       %We do a lot of quoting!

-record(cg, {module=[],                         %Module name
             exps=[],                           %Exports (ordsets)
             imps=[],                           %Imports (orddict)
             pref=[],                           %Prefixes
             atts=[],                           %Attrubutes
             mets=[],                           %Metadata
             defs=[],                           %Function definitions.
             env=[],                            %Environment
             anno=[],                           %Current annotation
             opts=[],                           %Options
             file=[],                           %File name
             func=[],                           %Current function
             line=0,                            %Current line
             vc=0,                              %Variable counter
             fc=0                               %Function counter
            }).

%% module(ModuleForms, CompInfo) -> {ModuleName,CoreModule}

module(Mfs, #cinfo{opts=Opts,file=File}) ->
    St0 = #cg{opts=Opts,file=File},
    {Core,St1} = compile_module(Mfs, St0),
    {St1#cg.module,Core}.

%% compile_module(ModuleForms, State) -> {CoreModule,State}.

compile_module(Mfs, St0) ->
    {Fbs,St1} = collect_module(Mfs, St0),
    Core = c_module(c_atom(none), [], []),
    compile_forms(Fbs, St1, Core).

%% collect_module(ModuleForms, State) -> {Fbs,State}.
%%  Collect forms and module data. Returns function bindings and puts
%%  module data into state.

collect_module(Mfs, St0) ->
    {Fds,St1} = lists:foldl(fun collect_form/2, {[],St0}, Mfs),
    {lists:reverse(Fds),St1}.

%% collect_form(Form, Line, State} -> {FuncDefs,State}.
%%  Collect valid forms and module data. Returns forms and put module
%%  data into state.

collect_form({['define-module',Mod,Metas,Atts],L}, {Fds,St0}) ->
    St1 = collect_metas(Metas, L, St0#cg{module=Mod,anno=[L]}),
    {Fds,collect_attrs(Atts, L, St1)};
collect_form({['extend-module',Meta,Atts],L}, {Fds,St0}) ->
    St1 = collect_metas(Meta, L, St0#cg{anno=[L]}),
    {Fds,collect_attrs(Atts, L, St1)};
collect_form({['define-type',Type,Def],L}, {Fds,St}) ->
    {Fds,collect_meta([type,[Type,Def]], L, St#cg{anno=[L]})};
collect_form({['define-opaque-type',Type,Def],L}, {Fds,St}) ->
    {Fds,collect_meta([opaque,[Type,Def]], L, St#cg{anno=[L]})};
collect_form({['define-function-spec',Func,Spec],L}, {Fds,St}) ->
    {Fds,collect_meta([spec,[Func,Spec]], L, St#cg{anno=[L]})};
collect_form({['define-function',Name,_Meta,Def],L}, {Fds,St}) ->
    %% Ignore the meta data.
    {[{Name,Def,L}|Fds],St};
%% Ignore macro definitions and eval-when-compile forms.
collect_form({['define-macro'|_],_}, {Fds,St}) -> {Fds,St};
collect_form({['eval-when-compile'|_],_}, {Fds,St}) -> {Fds,St}.

%% collect_metas(Metas, Line, State) -> State.
%%  Collect module metadata which is to be compiled. Only type
%%  information is to be kept.

collect_metas(Ms, L, St) ->
    foldl(fun (M, S) -> collect_meta(M, L, S) end, St, Ms).

collect_meta([type|Tds], L, #cg{mets=Ms}=St) ->
    St#cg{mets=Ms ++ [{type,Tds,L}]};
collect_meta([opaque|Tds], L, #cg{mets=Ms}=St) ->
    St#cg{mets=Ms ++ [{opaque,Tds,L}]};
collect_meta([spec|Sps], L, #cg{mets=Ms}=St) ->
    St#cg{mets=Ms ++ [{spec,Sps,L}]};
collect_meta([record|Rds], L, #cg{mets=Ms}=St) ->
    St#cg{mets=Ms ++ [{record,Rds,L}]};
collect_meta(_M, _L, St) -> St.                 %Ignore the rest

%% collect_attrs(Attributes, Line, State) -> State.
%%  Collect module attributes and fill in the #cg state record. Need
%%  to ignore all eventual doc attributes.

collect_attrs(As, L, St) ->
    %% io:format("ca: ~p\n", [As]),
    foldl(fun (A, S) -> collect_attr(A, L, S) end, St, As).

collect_attr([export|Es], _, St) -> collect_exps(Es, St);
collect_attr([import|Is], _, St) -> collect_imps(Is, St);
collect_attr([doc|_], _, St) -> St;             %Don't save doc attribute!
collect_attr([N|Vs], L, #cg{atts=As}=St) ->
    St#cg{atts=As ++ [{N,Vs,L}]}.               %Probably not many

collect_exps([all], St) -> St#cg{exps=all};     %Propagate all
collect_exps(_, #cg{exps=all}=St) -> St;
collect_exps(Es, #cg{exps=Exps0}=St) ->
    %% Add exports to export set.
    Exps1 = foldl(fun ([F,A], E) -> add_element({F,A}, E) end,
                  Exps0, Es),
    St#cg{exps=Exps1}.

collect_imps(Is, St) ->
    foldl(fun (I, S) -> collect_imp(I, S) end, St, Is).

collect_imp(['from',Mod|Fs], St) ->
    collect_imp(fun ([F,A], Imps) -> store({F,A}, F, Imps) end,
                Mod, St, Fs);
collect_imp(['rename',Mod|Rs], St) ->
    collect_imp(fun ([[F,A],R], Imps) -> store({F,A}, R, Imps) end,
                Mod, St, Rs);
collect_imp(['prefix',Mod,Pre], St) ->
    Pstr = atom_to_list(Pre),            %Store prefix as string
    St#cg{pref=store(Pstr, Mod, St#cg.pref)}.

collect_imp(Fun, Mod, St, Fs) ->
    Imps0 = safe_fetch(Mod, St#cg.imps, []),
    Imps1 = foldl(Fun, Imps0, Fs),
    St#cg{imps=store(Mod, Imps1, St#cg.imps)}.

%% compile_forms(Forms, State, CoreModule) -> {CoreModule,State}.
%%  Compile the forms from the file as stored in the state record.

compile_forms(Fbs0, St0, Core0) ->
    %% Add predefined functions and definitions, these are in line 0.
    Predefs = [{module_info,0},{module_info,1}],
    Mibs = [{module_info,
             [lambda,[],
              [call,?Q(erlang),?Q(get_module_info),?Q(St0#cg.module)]],0},
            {module_info,
             [lambda,[x],
              [call,?Q(erlang),?Q(get_module_info),?Q(St0#cg.module),x]],0}],
    %% The sum of all functions.
    Fbs1 = Fbs0 ++ Mibs,
    %% Make initial environment and set state.
    Env = forms_env(Fbs1, St0),
    St1 = St0#cg{exps=add_exports(St0#cg.exps, Predefs),
                 defs=Fbs1,env=Env},
    Exps = make_exports(St1#cg.exps, Fbs1),
    Atts = map(fun (Attr) ->
                       %% io:format("ca: ~p\n", [Attr]),
                       comp_attribute(Attr)
               end, St1#cg.atts),
    Mets = map(fun (Meta) ->
                       %% io:format("cm: ~p\n", [Meta]),
                       comp_metadata(Meta)
               end, St1#cg.mets),
    %% Both the attributes and saved metadata end up in the attributes.
    Catts = Mets ++ Atts,
    %% Compile the functions.
    {Cdefs,St2} = mapfoldl(fun (D, St) -> comp_define(D, Env, St) end,
                           St1, St1#cg.defs),
    %% Build the final core module structure.
    Core1 = update_c_module(Core0, c_atom(St2#cg.module), Exps, Catts, Cdefs),
    %% Maybe print lots of debug info.
    ?DEBUG("#cg: ~p\n", [St2], St2#cg.opts),
    ?DEBUG("core_lint: ~p\n", [(catch core_lint:module(Core1))], St2#cg.opts),
    ?DEBUG("#core: ~p\n", [Core1], St2#cg.opts),
    %% ?DEBUG("core_pp: ~p\n",
    %%        [(catch io:put_chars([core_pp:format(Core1),$\n]))], St2#cg.opts),
    {Core1,St2}.

forms_env(Fbs, St) ->
    %% Make initial environment with imports and local functions.
    Env = foldl(fun ({M,Fs}, Env) ->
                        foldl(fun ({{F,A},R}, E) ->
                                      add_ibinding(M, F, A, R, E)
                              end, Env, Fs)
                end, lfe_env:new(), St#cg.imps),
    foldl(fun ({Name,Def,_}, E) ->
                  add_fbinding(Name, func_arity(Def), Name, E)
          end, Env, Fbs).

add_exports(all, _) -> all;
add_exports(Old, More) -> union(Old, More).

make_exports(all, Fbs) ->
    map(fun ({F,Def,_}) -> c_fname(F, func_arity(Def)) end, Fbs);
make_exports(Exps, _) ->
    map(fun ({F,A}) -> c_fname(F, A) end, Exps).

%% comp_attribute(Attribute) -> CoreAttr.
%%  Compile attributes.

comp_attribute({N,V,Line}) ->
    Ann = [Line],
    {ann_c_lit(Ann, N),ann_c_lit(Ann, V)}.

%% comp_metadata(Metadata) -> CoreAttr.
%%  Compile metadata handling the special cases.

comp_metadata({type,Types,Line}) ->
    comp_type_metadata(type, Types, Line);
comp_metadata({opaque,Types,Line}) ->
    comp_type_metadata(opaque, Types, Line);
comp_metadata({spec,Specs,Line}) ->
    comp_spec_metadata(Specs, Line);
comp_metadata({record,Records,Line}) ->
    comp_record_metadata(Records, Line);
comp_metadata({N,V,Line}) ->
    Ann = [Line],
    {ann_c_lit(Ann, N),ann_c_lit(Ann, V)}.

comp_type_metadata(Attr, Types, Line) ->
    Ann = [Line],
    Tfun = fun ([[Type|Args],Def]) ->
                   {Type,
                    lfe_types:to_type_def(Def, Ann),
                    lfe_types:to_type_defs(Args, Ann)}
           end,
    Tdefs = [ Tfun(Type) || Type <- Types ],
    {ann_c_lit(Ann, Attr),ann_c_lit(Ann, Tdefs)}.

comp_spec_metadata(Specs, Line) ->
    Ann = [Line],
    Sfun = fun ([[N,Ar],Spec]) ->
                   {{N,Ar},lfe_types:to_func_spec_list(Spec, Ann)}
           end,
    Fspecs = [ Sfun(Spec) || Spec <- Specs ],
    {ann_c_lit(Ann, spec),ann_c_lit(Ann, Fspecs)}.

%% comp_record_metadata(Records, Line) -> Metadata.
%%  Format depends on whether 18 and older or newer.

-ifdef(NEW_REC_CORE).
comp_record_metadata(Recs, Line) ->
    Ann = [Line],
    Rfun = fun ([Name|Fields]) ->
                   {Name,[ comp_record_field(Fdef, Ann) || Fdef <- Fields ]}
           end,
    Rs = [ Rfun(Rec) || Rec <- Recs ],
    {ann_c_lit(Ann, record),ann_c_lit(Ann, Rs)}.
-else.
comp_record_metadata(Recs, Line) ->
    Ann = [Line],
    Rfun = fun ([Name|Fields]) ->
                   {{record,Name},
                    [ comp_record_field(Fdef, Ann) || Fdef <- Fields ]}
           end,
    Rs = [ Rfun(Rec) || Rec <- Recs ],
    {ann_c_lit(Ann, type),ann_c_lit(Ann, Rs)}.
-endif.

comp_record_field([F,D,T], Ann) ->
    {typed_record_field,
     comp_untyped_field([F,D], Ann),
     lfe_types:to_type_def(T, Ann)};
comp_record_field(Fd, Ann) ->
    comp_untyped_field(Fd, Ann).

comp_untyped_field([F,?Q(undefined)], Ann) ->   %No need for undefined default
    {record_field,Ann,{atom,Ann,F}};
comp_untyped_field([F,D], Ann) ->
    {record_field,Ann,{atom,Ann,F},lfe_trans:to_expr(D, Ann)};
comp_untyped_field(F, Ann) ->
    {record_field,Ann,{atom,Ann,F}}.

%% comp_define(DefForm, Env, State) -> {Corefunc,State}.
%%  Compile a top-level define. Sets current function name. Be careful
%%  with annotations as dialyzer then sometimes goes crazy.

comp_define({Name,Def,L}, Env, St) ->
    Ann = [L],
    Cf = {Name,func_arity(Def)},                %Is useful
    comp_func(Name, Def, Env, L, St#cg{func=Cf,line=L,vc=0,fc=0,anno=Ann}).

%% comp_body(BodyList, Env, Line, State) -> {CoreBody,State}.
%%  Compile a body list of expressions.

comp_body([E], Env, L, St) -> comp_expr(E, Env, L, St);
comp_body([E|Es], Env, L, St0) ->
    {Ce,St1} = comp_expr(E, Env, L, St0),
    {Cb,St2} = comp_body(Es, Env, L, St1),
    {append_c_seq(Ce, Cb, L),St2};              %Flatten nested sequences
comp_body([], _, _, St) -> {c_nil(),St}.        %Empty body returns []

%% append_c_seq(Expr, Body, Line) -> {CoreBody}.
%%  Create a c_seq with Expr and Body by appending Body to the end of
%%  Expr c_seq chain if there is one. We get flat sequence.

append_c_seq(Ce, Cb, L) ->
    case is_c_seq(Ce) of
        true ->
            update_c_seq(Ce, seq_arg(Ce), append_c_seq(seq_body(Ce), Cb, L));
        false -> ann_c_seq([L], Ce, Cb)
    end.

%% comp_expr(Expr, Env, Line, State) -> {CoreExpr,State}.
%%  Compile an expression.

%% Handle the Core data special forms.
comp_expr([quote,E], _, _, St) -> {comp_lit(E),St};
comp_expr([cons,H,T], Env, L, St) ->
    Cons = fun ([Ch,Ct], _, _, Sta) -> {c_cons(Ch, Ct),Sta} end,
    comp_args([H,T], Cons, Env, L, St);
comp_expr([car,E], Env, L, St) ->               %Provide lisp names
    comp_bif_call(hd, [E], Env, L, St);
comp_expr([cdr,E], Env, L, St) ->
    comp_bif_call(tl, [E], Env, L, St);
comp_expr([list|Es], Env, L, St) ->
    List = fun (Ces, _, _, Sta) ->
                   {foldr(fun (E, T) -> c_cons(E, T) end, c_nil(), Ces),Sta}
           end,
    comp_args(Es, List, Env, L, St);
comp_expr([tuple|As], Env, L, St) ->
    Args = fun (Args, _, _, Sta) -> {c_tuple(Args),Sta} end,
    comp_args(As, Args, Env, L, St);
comp_expr([tref,Tup,I], Env, L, St) ->
    comp_bif_call(element, [I,Tup], Env, L, St);
comp_expr([tset,Tup,I,V], Env, L, St) ->
    comp_bif_call(setelement, [I,Tup,V], Env, L, St);
comp_expr([binary|Segs], Env, L, St) ->
    comp_binary(Segs, Env, L, St);              %And bitstring as well
comp_expr([map|As], Env, L, St) ->
    comp_map(As, Env, L, St);
comp_expr(['mref',Map,K], Env, L, St) ->
    %% Sneaky, but no other real option for now.
    comp_expr([call,?Q(maps),?Q(get),K,Map], Env, L, St);
comp_expr(['mset',Map|As], Env, L, St) ->
    comp_set_map(Map, As, Env, L, St);
comp_expr(['mupd',Map|As], Env, L, St) ->
    comp_upd_map(Map, As, Env, L, St);
comp_expr(['map-get',Map,K], Env, L, St) ->
    comp_expr(['mref',Map,K], Env, L, St);
comp_expr(['map-set',Map|As], Env, L, St) ->
    comp_expr(['mset',Map|As], Env, L, St);
comp_expr(['map-update',Map|As], Env, L, St) ->
    comp_expr(['mupd',Map|As], Env, L, St);
comp_expr([function,F,Ar], Env, L, St) ->
    %% In general case create a lambda.
    Args = new_vars(Ar),
    Body = [[F|Args]],
    comp_lambda(Args, Body, Env, L, St);
comp_expr([function,M,F,Ar], Env, L, St) ->
    %% The arguments are all literals.
    comp_bif_call(make_fun, [?Q(M),?Q(F),Ar], Env, L, St);
%% Handle the Core closure special forms.
comp_expr([lambda,Args|Body], Env, L, St) ->
    comp_lambda(Args, Body, Env, L, St);
comp_expr(['match-lambda'|Cls], Env, L, St) ->
    comp_match_lambda(Cls, Env, L, St);
comp_expr(['let',Vbs|Body], Env, L, St) ->
    comp_let(Vbs, Body, Env, L, St);
comp_expr(['let-function',Fbs|Body], Env, L, St) ->
    comp_let_function(Fbs, Body, Env, L, St);
comp_expr(['letrec-function',Fbs|Body], Env, L, St) ->
    comp_letrec_function(Fbs, Body, Env, L, St);
%% (let-syntax ...) should never be seen here!
%% Handle the Core control special forms.
comp_expr(['progn'|Body], Env, L, St) ->
    comp_body(Body, Env, L, St);
comp_expr(['if'|Body], Env, L, St) ->
    comp_if(Body, Env, L, St);
comp_expr(['case',Expr|Cls], Env, L, St) ->
    comp_case(Expr, Cls, Env, L, St);
comp_expr(['receive'|Cls], Env, L, St0) ->
    {Ccs,Ct,Ca,St1} = rec_clauses(Cls, Env, L, St0),
    {ann_c_receive([L], Ccs, Ct, Ca),St1};
comp_expr(['catch'|Body], Env, L, St0) ->
    {Cb,St1} = comp_body(Body, Env, L, St0),
    {ann_c_catch([L], Cb),St1};
comp_expr(['try'|B], Env, L, St) ->
    comp_try(B, Env, L, St);
comp_expr(['funcall',F|As], Env, L, St) ->
    comp_funcall(F, As, Env, L, St);
%%comp_expr([call,[quote,erlang],[quote,primop]|As], Env, L, St) ->
%% An interesting thought to open up system.
comp_expr([call,M,N|As], Env, L, St) ->
    %% Call a function in another module.
    Call = fun ([Cm,Cn|Cas], _, Li, Sta) ->
                   Ann = line_file_anno(Li, Sta),
                   {ann_c_call(Ann, Cm, Cn, Cas),Sta}
           end,
    comp_args([M,N|As], Call, Env, L, St);
%% General function calls.
comp_expr([Fun|As], Env, L, St) when is_atom(Fun) ->
    %% Fun is a symbol which is either a known BIF or function.
    Call = fun (Cas, En, Li, Sta) ->
                   Ar = length(Cas),
                   Ann = line_file_anno(Li, Sta),
                   case get_fbinding(Fun, Ar, En) of
                       {yes,M,F} ->             %BIF or import
                           {ann_c_call(Ann, c_atom(M), c_atom(F), Cas),Sta};
                       {yes,Name} ->
                           %% Might have been renamed, use real function name.
                           {ann_c_apply(Ann, c_fname(Name, Ar), Cas),Sta};
                       no ->
                           %% io:format("ce: ~p\n", [{{Fun,Ar},En}]),
                           error(foo)
                   end
           end,
    comp_args(As, Call, Env, L, St);
comp_expr(Symb, _, _, St) when is_atom(Symb) ->
    {c_var(Symb),St};
%% Everything is a literal constant (nil, tuples, numbers, binaries, maps).
comp_expr(Const, _, _, St) ->
    {comp_lit(Const),St}.

%% get_fbinding(NAme, Arity, Env) ->
%%     {yes,Module,Fun} | {yes,Binding} | no.
%%  Get the function binding. Locally bound function takes precedence
%%  over auto-imported BIFs.

get_fbinding(Name, Ar, Env) ->
    case lfe_env:get_fbinding(Name, Ar, Env) of
        {yes,_,_}=Yes -> Yes;                   %Imported function
        {yes,_}=Yes -> Yes;                     %Bound function
        no ->
            case lfe_internal:is_lfe_bif(Name, Ar) of
                true -> {yes,lfe,Name};         %Auto-imported LFE BIF
                false ->
                    case lfe_internal:is_erl_bif(Name, Ar) of
                        true ->                 %Auto-imported Erlang BIF
                            {yes,erlang,Name};
                        false -> no
                    end
            end
    end.

%% comp_bif_call(Bif, Args, Env, Line, State) -> {Call,State}.
%%  Call a BIF in the erlang module.

comp_bif_call(Bif, As, Env, L, St) ->
    Call = fun(Cas, _, Li, Sta) ->
                   Ann = line_file_anno(Li, Sta),
                   {ann_c_call(Ann, c_atom(erlang), c_atom(Bif), Cas),Sta}
           end,
    comp_args(As, Call, Env, L, St).

%% comp_args(Args, CallFun, Env, Line, State) -> {Call,State}.
%%  Sequentialise the evaluation of Args building the Call at the
%%  bottom. For non-simple arguments use let to break the arg
%%  evaluation out from the main call.

comp_args(As, Call, Env, L, St0) ->
    {Cas,St1} = mapfoldl(fun (A, St) -> comp_expr(A, Env, L, St) end, St0, As),
    simple_seq(Cas, Call, Env, L, St1).

%% simple_seq(CoreExps, Then, Env, Line, State) -> {Cepxr,State}.
%%  Sequentialise the evaluation of a sequence of core expressions
%%  using let's for non-simple exprs, and call Then with the simple
%%  core sequence. Cannot use a simple foldr as we pass data both in
%%  and out.

simple_seq(Ces, Then, Env, L, St) -> simple_seq(Ces, Then, [], Env, L, St).

simple_seq([Ce|Ces], Then, Ses, Env, L, St0) ->
    %% Use erlang core compiler lib which does what we want.
    case is_simple(Ce) of
        true -> simple_seq(Ces, Then, [Ce|Ses], Env, L, St0);
        false ->
            {Cv,St1} = new_c_var(L, St0),
            {Rest,St2} = simple_seq(Ces, Then, [Cv|Ses], Env, L, St1),
            {ann_c_let([L], [Cv], Ce, Rest),St2}
    end;
simple_seq([], Then, Ses, Env, L, St) ->
    Then(reverse(Ses), Env, L, St).

%% comp_lambda(Args, Body, Env, Line, State) -> {c_fun(),State}.
%%  Compile a (lambda (...) ...).

comp_lambda(Args, Body0, Env, L, St0) ->
    {Cvs,Pvs,Ts,St1} = comp_lambda_args(Args, L, St0),
    Body1 = add_guard_tests(Ts, Body0),
    {Cb,St2} = comp_body(Body1, add_vbindings(Pvs, Env), L, St1),
    Ann = line_file_anno(L, St2),
    {ann_c_fun(Ann, Cvs, Cb),St2}.

comp_lambda_args(Args, L, St) ->
    foldr(fun (A, {Cvs,Pvs0,Ts0,St0}) ->
                  {Cv,Pvs1,Ts1,St1} = pat_symb(A, L, Pvs0, Ts0, St0),
                  {[Cv|Cvs],Pvs1,Ts1,St1}
          end, {[],[],[],St}, Args).

%% lambda_arity([Args|_]) -> length(Args).

%% comp_match_lambda(Clauses, Env, Line, State) -> {c_fun(),State}.
%%  (match-lambda (Pat ...) ...).

comp_match_lambda(Cls, Env, L, St0) ->
    Ar = match_lambda_arity(Cls),
    {Cvs,St1} = new_c_vars(Ar, L, St0),
    {Ccs,St2} = comp_match_clauses(Cls, Env, L, St1),
    {Fvs,St3} = new_c_vars(Ar, L, St2),
    Cf = func_fail(Fvs, L, St3),
    Ann = line_file_anno(L, St3),
    Cb = ann_c_case(Ann, ann_c_values(Ann, Cvs), Ccs ++ [Cf]),
    {ann_c_fun(Ann, Cvs, Cb),St3}.

func_fail(Fvs, L, #cg{func=F}=St) ->
    %% We need function_name anno to generate function_clause error.
    fail_clause(Fvs, c_tuple([c_atom(function_clause)|Fvs]),
                [{function_name,F}], L, St).

%% match_lambda_arity(MatchClauses) -> int().

match_lambda_arity([[Pats|_]|_]) -> length(Pats).

comp_match_clauses(Cls, Env, L, St) ->
    mapfoldl(fun (Cl, Sta) -> comp_match_clause(Cl, Env, L, Sta) end,
             St, Cls).

%% comp_match_clause(Clause, Env, L, State) -> {c_clause(),State}.
%%  (Pats [(when Guard)] . Body)
%%  Pats is here a list of patterns which are the function clause
%%  arguments. This must be compiled to a list of patterns not a
%%  pattern with a list!

comp_match_clause([Pats|Body0], Env0, L, St0) ->
    Pfun = fun (P, {Pvsa,Vtsa,Sta}) ->
                   {Cp,Pvsb,Vtsb,Stb} = pattern(P, L, Pvsa, Vtsa, Sta),
                   {Cp,{Pvsb,Vtsb,Stb}}
           end,
    {Cps,{Pvs,Vts,St1}} = mapfoldl(Pfun, {[],[],St0}, Pats),
    %% io:format("~p\n", [{Cps,Vts}]),
    Env1 = add_vbindings(Pvs, Env0),
    Body1 = add_guard_tests(Vts, Body0),
    {Cg,Cb,St2} = comp_clause_body(Body1, Env1, L, St1),
    Ann = line_file_anno(L, St2),
    {ann_c_clause(Ann, Cps, Cg, Cb),St2}.

add_guard_tests([], Body) -> Body;
add_guard_tests(Ts, [['when'|Guard]|Body]) ->
    [['when'|Ts ++ Guard]|Body];
add_guard_tests(Ts, Body) ->
    [['when'|Ts]|Body].

%% comp_let(VarBindings, Body, Env, L, State) -> {c_let()|c_case(),State}.
%%  Compile a let expr. First evaluate all the value expressions in
%%  parallel so they don't inherit variables, then build nested cases
%%  to do matching optimising case where value bound to variable. Use
%%  nested cases so match fail only give one value. Probably not worth
%%  the effort as optimiser would do it.

comp_let(Vbs, B, Env, L, St0) ->
    {Cvs,Ces,Cms,St1} = comp_let_vbs(Vbs, Env, L, St0),
    {Cb,St2} = comp_let_body(Cms, B, add_vbindings(Cvs, Env), L, St1),
    %% Build nesting let which evaluates expressions first.
    {ann_c_let([L], Cvs, ann_c_values([L], Ces), Cb),St2}.

comp_let_vbs(Vbs, Env, L, St) ->
    Fun = fun ([V,E], {Cvs,Ces,Cms,St0}) when is_atom(V) ->
                  {Ce,St1} = comp_expr(E, Env, L, St0),
                  {[c_var(V)|Cvs],[Ce|Ces],Cms,St1};
              ([P,E], {Cvs,Ces,Cms,St0}) ->
                  {V,St1} = new_var(St0),
                  {Ce,St2} = comp_expr(E, Env, L, St1),
                  {[c_var(V)|Cvs],[Ce|Ces],[{P,[],V}|Cms],St2};
              ([P,['when'|G],E], {Cvs,Ces,Cms,St0}) ->
                  {V,St1} = new_var(St0),
                  {Ce,St2} = comp_expr(E, Env, L, St1),
                  {[c_var(V)|Cvs],[Ce|Ces],[{P,G,V}|Cms],St2}
          end,
    lists:foldr(Fun, {[],[],[],St}, Vbs).

comp_let_body([{P,G,V}|Cms], B, Env0, L, St0) ->
    Cv = c_var(V),
    {Cp,Pvs,Vts,St1} = pattern(P, L, St0),
    Env1 = add_vbindings(Pvs, Env0),
    {Cg,St2} = comp_guard(Vts ++ G, Env1, L, St1),
    {Cb,St3} = comp_let_body(Cms, B, Env1, L, St2),
    Cf = let_fail(Cv, L, St3),
    {ann_c_case([L], Cv, [ann_c_clause([L], [Cp], Cg, Cb),Cf]),St3};
comp_let_body([], B, Env, L, St0) ->
    {Cb,St1} = comp_body(B, Env, L, St0),
    {Cb,St1}.

let_fail(Cv, L, St) ->
    fail_clause([Cv], c_tuple([c_atom(badmatch),Cv]), [], L, St).

%% comp_let_function(FuncBindngs, Body, Env, Line, State) ->
%%      {c_letrec(),State}.
%%  Compile an flet. This is complicated by the fact that Core only
%%  has letrec so we have to some name munging of the functions to
%%  avoid recursive definitions.

comp_let_function(Fbs0, B, Env0, L, St0) ->
    %% Munge names of functions. Don't use new_symb as we want to link
    %% new names to original.
    Nfun = fun ([Old,Def], S0) ->
                   {New,S1} = new_fun_name(atom_to_list(Old), S0),
                   {{Old,New,Def},S1}
           end,
    {Nfbs,St1} = mapfoldl(Nfun, St0, Fbs0),
    %% Now compile functions in old environment.
    Ffun = fun ({_,New,Def}, St) -> comp_func(New, Def, Env0, L, St) end,
    {Cfs,St2} = mapfoldl(Ffun, St1, Nfbs),
    %% Add local functions Env mapping old name to new.
    Efun = fun ({Old,New,Def}, E) ->
                   add_fbinding(Old, func_arity(Def), New, E)
           end,
    Env1 = foldl(Efun, Env0, Nfbs),
    {Cb,St3} = comp_body(B, Env1, L, St2),
    {ann_c_letrec([L], Cfs, Cb),St3}.

%% comp_letrec_function(FuncBindngs, Body, Env, Line, State) ->
%%      {c_letrec(),State}.

comp_letrec_function(Fbs, B, Env0, L, St0) ->
    %% Add local functions Env.
    Efun = fun ([Name,Def], E) ->
                   add_fbinding(Name, func_arity(Def), Name, E)
           end,
    Env1 = foldl(Efun, Env0, Fbs),
    %% Now compile functions in new environment.
    Ffun = fun ([Name,Def], St) -> comp_func(Name, Def, Env1, L, St) end,
    {Cfs,St1} = mapfoldl(Ffun, St0, Fbs),
    {Cb,St2} = comp_body(B, Env1, L, St1),
    {ann_c_letrec([L], Cfs, Cb),St2}.

%% func_arity(FuncDef) -> Arity.
%%  Return the arity of a function definition.

func_arity([lambda,Args|_]) -> length(Args);
func_arity(['match-lambda'|Cls]) ->
    match_lambda_arity(Cls).

%% comp_func(FuncName, FuncDef, Env, L, State) -> {{Fname,Cfun},State}.
%%  NEVER annotate the Fname c-form, dialyzer goes crazy then for some
%%  strange reason!

comp_func(Name, [lambda,Args|Body], Env, L, St0) ->
    Cf = c_fname(Name, length(Args)),
    {Cfun,St1} = comp_lambda(Args, Body, Env, L, St0),
    {{Cf,Cfun},St1};
comp_func(Name, ['match-lambda'|Cls], Env, L, St0) ->
    Cf = c_fname(Name, match_lambda_arity(Cls)),
    {Cfun,St1} = comp_match_lambda(Cls, Env, L, St0),
    {{Cf,Cfun},St1}.

%% comp_if(IfBody, Env, Line, State) -> {c_case(),State}.
%%  Compile in if form to a case testing the Test expression.

comp_if([Test,True], Env, L, St) ->
    comp_if(Test, True, ?Q(false), Env, L, St);
comp_if([Test,True,False], Env, L, St) ->
    comp_if(Test, True, False, Env, L, St).

comp_if(Te, Tr, Fa, Env, L, St0) ->
    {Cte,St1} = comp_expr(Te, Env, L, St0),     %Test expression
    {Ctr,St2} = comp_expr(Tr, Env, L, St1),     %True expression
    {Cfa,St3} = comp_expr(Fa, Env, L, St2),     %False expression
    True = c_atom(true),
    False = c_atom(false),
    Ctrue = ann_c_clause([L], [True], Ctr),
    Cfalse = ann_c_clause([L], [False], Cfa),
    Cfail = if_fail(L, St3),
    {ann_c_case([L], Cte, [Ctrue,Cfalse,Cfail]),St3}.

%% This produces code which is harder to optimise, strangely enough.
%% comp_if(Te, Tr, Fa, Env, L, St0) ->
%%     {Cte,St1} = comp_expr(Te, Env, L, St0),     %Test expression
%%     {Ctr,St2} = comp_expr(Tr, Env, L, St1),     %True expression
%%     {Cfa,St3} = comp_expr(Fa, Env, L, St2),     %False expression
%%     If = fun ([Ctest], _, _, St) ->
%%                  True = c_atom(true),
%%                  False = c_atom(false),
%%                  Ctrue = ann_c_clause([L], [True], Ctr),
%%                  Cfalse = ann_c_clause([L], [Fail], Cfa),
%%                  Cfail = if_fail(L, St),
%%                  {ann_c_case([L], Ctest, [Ctrue,Cfalse,Cfail]),St}
%%          end,
%%     simple_seq([Cte], If, Env, L, St3).

if_fail(L, St) ->
    Cv = c_var(omega),
    fail_clause([Cv], c_atom(if_clause), [], L, St).

%% fail_clause(Pats, Arg, FailAnno, Line, State) -> Clause.
%%  Build a general failure clause. No line number in the clause, but
%%  append the line number and file name to the annotation.

fail_clause(Pats, Arg, Fann, L, St) ->
    Ann = line_file_anno(L, St),
    ann_c_clause(comp_gen_anno(L, St),          %It is compiler generated
                 Pats, ann_c_primop(Fann ++ Ann, c_atom(match_fail), [Arg])).

%% comp_case(Expr, Clauses, Env, Line, State) -> {c_case(),State}.
%%  Compile a case.

comp_case(E, Cls, Env, L, St0) ->
    {Ce,St1} = comp_expr(E, Env, L, St0),
    {Ccs,St2} = case_clauses(Cls, Env, L, St1),
    Cf = case_fail(L, St2),
    {ann_c_case([L], Ce, Ccs ++ [Cf]),St2}.

case_clauses(Cls, Env, L, St) ->
    mapfoldl(fun (Cl, Sta) -> comp_clause(Cl, Env, L, Sta) end,
             St, Cls).

case_fail(L, St) ->
    Cv = c_var(omega),
    fail_clause([Cv], c_tuple([c_atom(case_clause),Cv]), [], L, St).

%% rec_clauses(RecClauses, Env, Line, State) -> {Clause,Timeout,After,State}.

rec_clauses([['after',T|B]], Env, L, St0) ->
    {Ct,St1} = comp_expr(T, Env, L, St0),
    {Ca,St2} = comp_body(B, Env, L, St1),
    {[],Ct,Ca,St2};
rec_clauses([Cl|Cls], Env, L, St0) ->
    {Cc,St1} = comp_clause(Cl, Env, L, St0),
    {Ccs,Ct,Ca,St2} = rec_clauses(Cls, Env, L, St1),
    {[Cc|Ccs],Ct,Ca,St2};
rec_clauses([], _, _, St) ->
    {[],c_atom(infinity),c_atom(true),St}.

%% comp_clause(Clause, Env, Line, State) -> {c_clause(),State}.
%%  This is a case/receive clause where the is only one pattern.

comp_clause([Pat|Body0], Env0, L, St0) ->
    {Cp,Pvs,Vts,St1} = pattern(Pat, L, St0),
    Env1 = add_vbindings(Pvs, Env0),
    Body1 = add_guard_tests(Vts, Body0),
    {Cg,Cb,St2} = comp_clause_body(Body1, Env1, L, St1),
    {ann_c_clause([L], [Cp], Cg, Cb),St2}.

comp_clause_body([['when'|Guard]|Body], Env, L, St0) ->
    {Cg,St1} = comp_guard(Guard, Env, L, St0),
    {Cb,St2} = comp_body(Body, Env, L, St1),
    {Cg,Cb,St2};
comp_clause_body(Body, Env, L, St0) ->
    {Cb,St1} = comp_body(Body, Env, L, St0),
    {c_atom(true),Cb,St1}.

%% comp_try(Body, Env, Line, State) -> {c_try(),State}.
%%  Compile a try. We know that case is optional but must have at
%%  least one of catch or after. Complicated by the behaviour of the
%%  after which means we split try with all parts into two try's.

comp_try([E|Body], Env, L, St) ->
    %% Separate try body into separate bits, none if not there.
    Case = tag_tail(Body, 'case'),
    Catch = tag_tail(Body, 'catch'),
    After = tag_tail(Body, 'after'),
    comp_try(E, Case, Catch, After, Env, L, St). %Now build the bugger

%% comp_try(Exp, Case, Catch, After, Env, L, St) -> {c_try(),State}.

comp_try(E, Case, [], [], Env, L, St0) ->
    %% No catch or after - (try E [(case ...)])
    %% This is compiler generated.
    {Ce,St1} = comp_expr(E, Env, L, St0),
    {Cv,Cc,St2} = try_case(Case, Env, L, St1),
    {[_,Val,Info]=Evs,St3} = new_c_vars(3, L, St2), %Tag, Value, Info
    After = raise_primop([Info,Val], L, St2),
    Ann = line_file_anno(L, St3),
    {ann_c_try(Ann, Ce, [Cv], Cc, Evs, After),St3};
comp_try(E, Case, Catch, [], Env, L, St0) ->
    %% No after - (try E [(case ...)] (catch ...))
    {Ce,St1} = comp_expr(E, Env, L, St0),
    {Cv,Cc,St2} = try_case(Case, Env, L, St1),
    {Evs,Ecs,St3} = try_exception(Catch, Env, L, St2),
    Ann = line_file_anno(L, St3),
    {ann_c_try(Ann, Ce, [Cv], Cc, Evs, Ecs),St3};
comp_try(E, [], [], After, Env, L, St0) ->
    %% Just after - (try E (after ...))
    {Ce,St1} = comp_expr(E, Env, L, St0),
    {Cv,St2} = new_c_var(L, St1),
    {Ca,St3} = comp_body(After, Env, L, St2),
    Cb = ann_c_seq([L], Ca, Cv),
    {Evs,Ecs,St4} = try_after(After, Env, L, St3),
    Ann = line_file_anno(L, St4),
    {ann_c_try(Ann, Ce, [Cv], Cb, Evs, Ecs),St4};
comp_try(E, Case, Catch, After, Env, L, St) ->
    %% Both catch and after - (try E [(case ...)] (catch ...) (after ...))
    %% The case where all options are given.
    Try = ['try',E,['case'|Case],['catch'|Catch]],
    comp_try(Try, [], [], After, Env, L, St).

%% try_case(CaseClauses, Env, Line, State) -> {Var,c_case()|c_var(),State}.
%%  Case is optional, no case just returns value.

try_case([], _, L, St0) ->                      %No case, just return value
    {Cv,St1} = new_c_var(L, St0),
    {Cv,Cv,St1};
try_case(Cls, Env, L, St0) ->
    {Cv,St1} = new_c_var(L, St0),
    {Ccs,St2} = case_clauses(Cls, Env, L, St1),
    Cf = try_case_fail(L, St2),
    {Cv,ann_c_case([L], Cv, Ccs ++ [Cf]),St2}.

try_case_fail(L, St) ->
    Cv = c_var(omega),
    fail_clause([Cv], c_tuple([c_atom(try_clause),Cv]), [], L, St).

%% try_exception(CatchClauses, Env, L, State) -> {Vars,c_case(),State}.

try_exception(Cls, Env, L, St0) ->
    %% Note that Tag is not needed for rethrow - it is already in Info.
    {Cvs,St1} = new_c_vars(3, L, St0),          %Tag, Value, Info
    {Ccs,St2} = case_clauses(Cls, Env, L, St1),
    [_,Val,Info] = Cvs,
    Arg = c_tuple(Cvs),
    Fc = ann_c_clause(comp_gen_anno(L, St2),    %It is compiler generated
                      [Arg], raise_primop([Info,Val], L, St2)),
    Excp = ann_c_case([L], Arg, Ccs ++ [Fc]),
    {Cvs,Excp,St2}.

%% try_after(AfterBody, Env, L, State) -> {Vars,After,State}.

try_after(B, Env, L, St0) ->
    %% Note that Tag is not needed for rethrow - it is already in Info.
    {[_,Val,Info]=Cvs,St1} = new_c_vars(3, L, St0), %Tag, Value, Info
    {Cb,St2} = comp_body(B, Env, L, St1),
    After = ann_c_seq([L], Cb, raise_primop([Info,Val], L, St2)),
    {Cvs,After,St2}.

raise_primop(Args, L, _) ->
    ann_c_primop([L], c_atom(raise), Args).

tag_tail([[Tag|Tail]|_], Tag) -> Tail;
tag_tail([_|Try], Tag) -> tag_tail(Try, Tag);
tag_tail([], _) -> [].

%% comp_funcall(Function, Args, Env, Line, State) -> {Core,State}.
%%  Special case if Function is directly function, lambda or
%%  match-lambda, convert to a let. Might be useful in macros. We can
%%  do this is the lambda body is still "inside" the outer
%%  function. If handling of function changes then may need to be
%%  changed.

comp_funcall([function,F,Ar]=Func, As, Env, L, St) ->
    if Ar == length(As) ->                      %Check right number of args
            Las = new_vars(Ar),
            comp_funcall_let(Las, [[F|Las]], As, Env, L, St);
       true ->                                  %Catch arg mismatch at runtime
            comp_funcall_1(Func, As, Env, L, St)
    end;
comp_funcall([lambda,Las|Body]=Func, As, Env, L, St) ->
    if length(Las) == length(As) ->             %Check right number of args
            comp_funcall_let(Las, Body, As, Env, L, St);
       true ->                                  %Catch arg mismatch at runtime
            comp_funcall_1(Func, As, Env, L, St)
    end;
comp_funcall(['match-lambda'|Cls]=Func, As, Env, L, St0) ->
    case match_lambda_arity(Cls) == length(As) of
        true ->
            %% Expand comp_let as we need to special case body.
            {Cf,St1} = comp_match_lambda(Cls, Env, L, St0),
            Cvs = fun_vars(Cf),
            Cb = fun_body(Cf),
            Efun = fun (E, St) -> comp_expr(E, Env, L, St) end,
            {Ces,St2} = mapfoldl(Efun, St1, As),
            {ann_c_let([L], Cvs, ann_c_values([L], Ces), Cb),St2};
        false ->                                %Catch arg mismatch at runtime
            comp_funcall_1(Func, As, Env, L, St0)
    end;
comp_funcall(Func, As, Env, L, St0) ->
    comp_funcall_1(Func, As, Env, L, St0).      %Naively just do it.

comp_funcall_let(Las, Body, As, Env, L, St) ->
    %% Convert into a let. Would like to sequentialise eval of
    %% args here but leave that to let.
    Vbs = zipwith(fun (V, E) -> [V,E] end, Las, As),
    comp_let(Vbs, Body, Env, L, St).

comp_funcall_1(Func, As, Env, L, St0) ->
    App = fun ([Cf|Cas], _, Li, St) ->
                  Ann = line_file_anno(Li, St),
                  {ann_c_apply(Ann, Cf, Cas),St}
          end,
    comp_args([Func|As], App, Env, L, St0).

%% comp_binary(Segs, Env, Line, State) -> {CbinaryExpr,State}.
%% Compile a binary.

comp_binary(Segs, Env, L, St0) ->
    Vsps = get_bitsegs(Segs),
    comp_bitsegs(Vsps, Env, L, St0).

get_bitsegs(Segs) ->
    foldr(fun (Seg, Vs) -> get_bitseg(Seg, Vs) end, [], Segs).

%% get_bitseg(Bitseg, ValSpecs) -> ValSpecs.
%%  A bitseg is either an atomic value, a list of value and specs, or
%%  a string. Note that this function can prepend a list of valspecs.

get_bitseg([Val|Specs]=F, Vsps) ->
    case is_integer_list(F) of                  %Is bitseg a string?
        true ->                                 %A string
            {Sz,Ty} = get_bitspecs([]),
            foldr(fun (V, Vs) -> [{V,Sz,Ty}|Vs] end, Vsps, F);
        false ->                                %A value and spec
            {Sz,Ty} = get_bitspecs(Specs),
            case is_integer_list(Val) of        %Is val a string?
                true -> foldr(fun (V, Vs) -> [{V,Sz,Ty}|Vs] end, Vsps, Val);
                false -> [{Val,Sz,Ty}|Vsps]     %The default
            end
    end;
get_bitseg(Val, Vsps) ->
    {Sz,Ty} = get_bitspecs([]),
    [{Val,Sz,Ty}|Vsps].

get_bitspecs(Ss) ->
    {ok,Sz,Ty} = lfe_bits:get_bitspecs(Ss),
    {Sz,Ty}.

is_integer_list([I|Is]) when is_integer(I) ->
    is_integer_list(Is);
is_integer_list([]) -> true;
is_integer_list(_) -> false.

%% comp_bitsegs(ValSpecs, Env, Line, State) -> {CBitSegs,State}.
%%  Compile the bitsegements sequentialising them with simple_seq.

comp_bitsegs(Vsps, Env, L, St) ->
    comp_bitsegs(Vsps, [], Env, L, St).

comp_bitsegs([Vsp|Segs], Csegs, Env, L, St0) ->
    {Cval,Csize,Un,Ty,Fs,St1} = comp_bitseg(Vsp, Env, L, St0),
    %% Sequentialise Val and Size if necessary, then do rest
    Next = fun ([Cv,Csz], En, Li, St) ->
                   Cs = c_bitstr(Cv, Csz, Un, Ty, Fs),
                   comp_bitsegs(Segs, [Cs|Csegs], En, Li, St)
           end,
    simple_seq([Cval,Csize], Next, Env, L, St1);
comp_bitsegs([], Csegs, _, L, St) ->
    {ann_c_binary([L], reverse(Csegs)),St}.

%% comp_bitseg(ValSpec, Env, Line, State) -> {Cval,Csize,Unit,Type,Fs,State}.
%%  Need to handle some special cases.

comp_bitseg({Val,_,{Ty,_,Si,En}}, Env, L, St0)
  when Ty =:= utf8 ; Ty =:= utf16 ; Ty =:= utf32 ->
    %% Special case utf types.
    {Cval,St1} = comp_expr(Val, Env, L, St0),
    Undef = c_atom(undefined),
    {Cval,Undef,Undef,c_atom(Ty),c_lit([Si,En]),St1};
comp_bitseg({Val,all,{binary,_,_,_}=Ty}, Env, L, St) ->
    comp_bitseg({Val,?Q(all),Ty}, Env, L, St);
comp_bitseg({Val,Sz,{Ty,Un,Si,En}}, Env, L, St0) ->
    {Cval,St1} = comp_expr(Val, Env, L, St0),
    {Csize,St2} = comp_expr(Sz, Env, L, St1),
    {Cval,Csize,c_int(Un),c_atom(Ty),c_lit([Si,En]),St2}.

%% comp_map(Args, Env, Line, State) -> {Core,State}.
%% comp_set_map(Map, Args, Line, State) -> {Core,State}.
%% comp_upd_map(Map, Args, Line, State) -> {Core,State}.

-ifdef(HAS_MAPS).

%% There is no need to check for HAS_FULL_KEYS here as the linter will
%% catch the limited code.  The setting/updating maps operations need
%% to be wrapped with an 'if' which does an explicit test that the map
%% argument is a map.  This does not have exactly the same structure
%% and annotations as a "normal" 'if'.

comp_map(Args, Env, Line, St0) ->
    Mapper = fun (Cas, _, L, St1) ->
                     Cpairs = comp_map_pairs(Cas, assoc, L),
                     {ann_c_map([L], c_lit(#{}), Cpairs),St1}
             end,
    comp_args(Args, Mapper, Env, Line, St0).

comp_set_map(Map, Args, Env, Line, St) ->
    comp_modify_map(Map, Args, assoc, Env, Line, St).

comp_upd_map(Map, Args, Env, Line, St) ->
    comp_modify_map(Map, Args, exact, Env, Line, St).

comp_modify_map(Map, Args, Key, Env, Line, St0) ->
    %% Evaluate map, keys and values and build modify form.
    Mapper = fun ([Cm|Cas], E, L, St) ->
                     Cpairs = comp_map_pairs(Cas, Key, L),
                     comp_map_test(Cm, Cpairs, E, L, St)
             end,
    comp_args([Map|Args], Mapper, Env, Line, St0).

comp_map_test(Cm, Cpairs, _, L, St) ->
    %% Build map type tester.
    Ann = line_file_anno(L, St),
    Cmap = ann_c_clause([compiler_generated|Ann], [],
                        ann_c_call(Ann, ann_c_atom(Ann, erlang),
                                   ann_c_atom(Ann, is_map), [Cm]),
                        ann_c_map(Ann, Cm, Cpairs)),
    Cfail = map_fail(Cm, L, St),
    {ann_c_case(Ann, ann_c_values(Ann, []), [Cmap,Cfail]),St}.

map_fail(_Map, L, St) ->
    Fann = [{eval_failure,badmap}],
    fail_clause([], c_atom(badmap), Fann, L, St).
%%    fail_clause([], c_tuple([c_atom(badmap),Map]), Fann, L, St).

comp_map_pairs([K,V|Ps], Op, L) ->
    [ann_c_map_pair([L], c_lit(Op), K, V)|comp_map_pairs(Ps, Op, L)];
comp_map_pairs([], _, _) -> [].
-else.
%% These are just dummy functions which will never be called as
%% lfe_lint will catch these forms.

comp_map(_, _, _, St) -> {c_lit(map),St}.
comp_set_map(_, _, _, _, St) -> {c_lit(map),St}.
comp_upd_map(_, _, _, _, St) -> {c_lit(map),St}.
-endif.

%% comp_guard(GuardTests, Env, Line, State) -> {CoreGuard,State}.
%%  Can compile much of the guard as an expression but must wrap it
%%  all in a try, which we do here. This try handles exceptions in the
%%  guard and has a very rigid structure.

comp_guard([], _, _, St) -> {c_atom(true),St};  %The empty guard
comp_guard(Gts, Env, L, St0) ->
    {Ce,St1} = comp_guard_tests(Gts, Env, L, St0), %Guard expression
    %% Can hard code the rest!
    Cv = c_var('Try'),
    Evs = [c_var('T'),c_var('R')],              %Why only two?
    False = c_atom(false),                      %Exception returns false
    Ann = line_file_anno(L, St1),
    {ann_c_try(Ann, Ce, [Cv], Cv, Evs, False),St1}.

%% comp_guard_tests(GuardTests, Env, Line, State) -> {CoreTest,State}.
%%  Compile a guard test, making sure it returns a boolean value. We
%%  do this in a naive way by always explicitly comparing the result
%%  to 'true' and letting the optimiser clean this up. Ignore errors.

comp_guard_tests(Gts, Env, Line, St0) ->
    {Gas,St1} = mapfoldl(fun (Gt, St) -> comp_guard_test(Gt, Env, Line, St) end,
                         St0, Gts),
    Ands = fun guard_ands/4,
    simple_seq(Gas, Ands, Env, Line, St1).

guard_ands([Ga], _, _, St) -> {Ga,St};
guard_ands([G1,G2], _, Line, St) ->
    {ann_c_call([Line], c_atom(erlang), c_atom('and') , [G1,G2]), St};
guard_ands([G1,G2|Gas], Env, Line, St0) ->
    {Cv,St1} = new_c_var(Line, St0),
    {Gr,St2} = guard_ands([Cv|Gas], Env, Line, St1),
    And = ann_c_call([Line], c_atom(erlang), c_atom('and'), [G1,G2]),
    {ann_c_let([Line], [Cv], And, Gr),St2}.

%% comp_guard_test(Test, Env, Line, State) -> {CoreTest,State}.
%%  Compile one test. We try to avoid generating an unnecessary true
%%  test by checking the test and only adding one when we know the
%%  test won't automatically return a boolean value.

comp_guard_test([quote,Bool], _, _, St) when is_boolean(Bool) ->
    {c_atom(Bool),St};                          %A small optimisation
comp_guard_test([call,[quote,erlang],[quote,Op]|Args]=Test, Env, L, St) ->
    comp_guard_test_1(Test, Op, Args, Env, L, St);
comp_guard_test([Op|Args]=Test, Env, L, St) ->
    comp_guard_test_1(Test, Op, Args, Env, L, St);
comp_guard_test(Symb, _, L, St) when is_atom(Symb) ->
    Ann = comp_gen_anno(L, St),
    {ann_c_call(Ann, c_atom(erlang), c_atom('=:='), [c_var(Symb),c_atom(true)]),
     St};
comp_guard_test(_, _, _, St) ->
    %% Everything else always will always fail.
    {c_atom(false),St}.

comp_guard_test_1(Test, Op, Args, Env, L, St0) ->
    Ar = length(Args),
    %% Check if this is a boolean test, else add a boolean test.
    case erl_internal:bool_op(Op, Ar) orelse
        erl_internal:comp_op(Op, Ar) orelse
        erl_internal:type_test(Op, Ar) of
        true ->                                 %It's already boolean
            comp_gexpr(Test, Env, L, St0);
        false ->                                %No it's not, then make it one
            Call = fun (Cas, _, Li, St) ->
                           Ann = comp_gen_anno(Li, St),
                           {ann_c_call(Ann, c_atom(erlang), c_atom('=:='), Cas),
                            St}
                   end,
            comp_gargs([Test,?Q(true)], Call, Env, L, St0)
    end.

%% comp_gexpr(Expr, Env, Line, State) -> {CoreExpr,State}.

%% Handle the Core data special forms.
comp_gexpr([quote,E], _, _, St) -> {comp_lit(E),St};
comp_gexpr([cons,H,T], Env, L, St0) ->
    Cons = fun ([Ch,Ct], _, _, St1) -> {c_cons(Ch, Ct),St1} end,
    comp_gargs([H,T], Cons, Env, L, St0);
comp_gexpr([car,E], Env, L, St) ->              %Provide lisp names
    comp_gcall(hd, [E], Env, L, St);
comp_gexpr([cdr,E], Env, L, St) ->
    comp_gcall(tl, [E], Env, L, St);
comp_gexpr([list|Es], Env, L, St0) ->
    List = fun (Ces, _, _, St1) ->
                   {foldr(fun (E, T) -> c_cons(E, T) end, c_nil(), Ces),St1}
           end,
    comp_gargs(Es, List, Env, L, St0);
comp_gexpr([tuple|As], Env, L, St0) ->
    Tuple = fun (Args, _, _, St1) -> {c_tuple(Args),St1} end,
    comp_gargs(As, Tuple, Env, L, St0);
comp_gexpr([tref,Tup,I], Env, L, St) ->
    comp_gcall(element, [I,Tup], Env, L, St);
comp_gexpr([binary|Segs], Env, L, St) ->
    comp_binary(Segs, Env, L, St);              %And bitstring as well
%% Map operations are not allowed in guards.
%% Handle the Core closure special forms.
%% (let-syntax ...) should never be seen here!
%% Handle the Core control special forms.
comp_gexpr(['progn'|Body], Env, L, St) ->
    comp_guard_tests(Body, Env, L, St);
comp_gexpr(['if'|Body], Env, L, St) ->
    comp_gif(Body, Env, L, St);
comp_gexpr([call,[quote,erlang],[quote,Fun]|As], Env, L, St) ->
    comp_gcall(Fun, As, Env, L, St);
%% Finally the not so general case.
comp_gexpr([Fun|As], Env, L, St) ->
    comp_gcall(Fun, As, Env, L, St);
comp_gexpr(Symb, _, _, St) when is_atom(Symb) ->
    {c_var(Symb),St};
%% Everything is a literal constant (nil, tuples, numbers, binaries).
comp_gexpr(Const, _, _, St) ->
    {comp_lit(Const),St}.

%% comp_gcall(Function, Args, Env, Line, State) -> {Call,State}.
%%  Only guard BIFs can be called in the guard.

comp_gcall(Fun, As, Env, L, St) ->
    Call = fun (Cas, _, Li, Sta) ->
                   Ann = line_file_anno(Li, Sta),
                   {ann_c_call(Ann, c_atom(erlang), c_atom(Fun), Cas),Sta}
           end,
    comp_gargs(As, Call, Env, L, St).

%% comp_gargs(Args, CallFun, Env, Line, State) -> {Call,State}.

comp_gargs(As, Call, Env, L, St0) ->
    {Cas,St1} = mapfoldl(fun (A, St) -> comp_gexpr(A, Env, L, St) end, St0, As),
    simple_seq(Cas, Call, Env, L, St1).

%% comp_gif(IfBody, Env, Line, State) -> {c_case(),State}.
%%  Compile in if form to a case testing the Test expression.

comp_gif([Test,True], Env, L, St) ->
    comp_gif(Test, True, ?Q(false), Env, L, St);
comp_gif([Test,True,False], Env, L, St) ->
    comp_gif(Test, True, False, Env, L, St).

comp_gif(Te, Tr, Fa, Env, L, St0) ->
    {Cte,St1} = comp_gexpr(Te, Env, L, St0),    %Test expression
    {Ctr,St2} = comp_gexpr(Tr, Env, L, St1),    %True test
    {Cfa,St3} = comp_gexpr(Fa, Env, L, St2),    %False test
    True = c_atom(true),
    False = c_atom(false),
    Omega = c_var(omega),
    Ctrue = ann_c_clause([L], [True], Ctr),
    Cfalse = ann_c_clause([L], [False], Cfa),
    Cfail = ann_c_clause(comp_gen_anno(L, St3), [Omega], Omega),
    {ann_c_case([L], Cte, [Ctrue,Cfalse,Cfail]),St3}.

%% This produces code which is harder to optimise, strangely enough.
%% comp_gif(Te, Tr, Fa, Env, L, St0) ->
%%     {Cte,St1} = comp_gexpr(Te, Env, L, St0),    %Test expression
%%     {Ctr,St2} = comp_gexpr(Tr, Env, L, St1),    %True expression
%%     {Cfa,St3} = comp_gexpr(Fa, Env, L, St2),    %False expression
%%     If = fun ([Ctest], _, _, St) ->
%%                  True = c_atom(true),
%%                  False = c_atom(false),
%%                  Omega = c_var(omega),
%%                  Ctrue = ann_c_clause([L], [True], Ctr),
%%                  Cfalse = ann_c_clause([L], [False], Cfa),
%%                  Cfail = ann_c_clause(comp_gen_anno(L, St3), [Omega], Omega),
%%                  {ann_c_case([L], Ctest, [Ctrue,Cfalse,Cfail]),St}
%%          end,
%%     simple_seq([Cte], If, Env, L, St3).

%% comp_lit(Value) -> LitExpr.
%%  Make a literal expression from an Erlang value. Try to make it as
%%  literal as possible. This function will fail if the value is not
%%  expressable as a literal (for instance, a pid).

comp_lit([H|T]) ->
    Ch = comp_lit(H),
    Ct = comp_lit(T),
    %% c_cons is smart and can handle head and tail both literals.
    c_cons(Ch, Ct);
comp_lit([]) -> c_nil();
comp_lit(T) when is_tuple(T) ->
    Es = comp_lit_list(tuple_to_list(T)),
    %% c_tuple is smart and can handle a list of literals.
    c_tuple(Es);
comp_lit(A) when is_atom(A) -> c_atom(A);
comp_lit(I) when is_integer(I) -> c_int(I);
comp_lit(F) when is_float(F) -> c_float(F);
comp_lit(Bin) when is_bitstring(Bin) ->
    Bits = comp_lit_bitsegs(Bin),
    ann_c_binary([], Bits);
comp_lit(Map) when ?IS_MAP(Map) ->
    comp_lit_map(Map).

comp_lit_list(Vals) -> [ comp_lit(V) || V <- Vals ].

is_lit_list(Es) -> all(fun (E) -> is_literal(E) end, Es).

comp_lit_bitsegs(<<B:8,Bits/bitstring>>) ->     %Next byte
    [c_byte_bitseg(B, 8)|comp_lit_bitsegs(Bits)];
comp_lit_bitsegs(<<>>) -> [];                   %Even bytes
comp_lit_bitsegs(Bits) ->                       %Size < 8
    N = bit_size(Bits),
    <<B:N>> = Bits,
    [c_byte_bitseg(B, N)].

c_byte_bitseg(B, Sz) ->
    c_bitstr(c_lit(B), c_int(Sz), c_int(1), c_atom(integer),
             c_lit([unsigned,big])).

-ifdef(HAS_MAPS).
comp_lit_map(Map) ->
    Pairs = comp_lit_map_pairs(maps:to_list(Map)),
    ann_c_map([], c_lit(#{}), Pairs).

comp_lit_map_pairs([{K,V}|Ps]) ->
    [ann_c_map_pair([], c_lit(assoc), comp_lit(K), comp_lit(V))|
     comp_lit_map_pairs(Ps)];
comp_lit_map_pairs([]) -> [].
-else.
comp_lit_map(_) -> c_lit(map).
-endif.

%% pattern(Pattern, Line, Status) -> {CorePat,PatVars,VarTests,State}.
%%  Compile a pattern into a Core term. Handle quoted sexprs here
%%  especially for symbols which then become variables instead of
%%  atoms.

pattern(Pat, L, St) -> pattern(Pat, L, [], [], St).

pattern([quote,E], _, Vs, Ts, St) -> {pat_lit(E),Vs,Ts,St};
pattern(['=',P1,P2], L, Vs0, Ts0, St0) ->
    %% Core can only alias against a variable so there is work to do!
    {Cp1,Vs1,Ts1,St1} = pattern(P1, L, Vs0, Ts0, St0),
    {Cp2,Vs2,Ts2,St2} = pattern(P2, L, Vs0, Ts1, St1),
    Cp = pat_alias(Cp1, Cp2),
    {Cp,union(Vs1, Vs2),Ts2,St2};
pattern([cons,H,T], L, Vs0, Ts0, St0) ->
    {Ch,Vs1,Ts1,St1} = pattern(H, L, Vs0, Ts0, St0),
    {Ct,Vs2,Ts2,St2} = pattern(T, L, Vs1, Ts1, St1),
    {c_cons(Ch, Ct),Vs2,Ts2,St2};
pattern([list|Ps], L, Vs, Ts, St) ->
    pat_list(Ps, L, Vs, Ts, St);
pattern([tuple|Ps], L, Vs0, Ts0, St0) ->
    Fun = fun (P, {Vsa,Tsa,Sta}) ->
                  {Cp,Vsb,Tsb,Stb} = pattern(P, L, Vsa, Tsa, Sta),
                  {Cp,{Vsb,Tsb,Stb}}
          end,
    {Cps,{Vs1,Ts1,St1}} = mapfoldl(Fun, {Vs0,Ts0,St0}, Ps),
    {c_tuple(Cps),Vs1,Ts1,St1};
pattern([binary|Segs], L, Vs, Ts, St) ->
    pat_binary(Segs, L, Vs, Ts, St);
pattern([map|As], L, Vs, Ts, St) ->
    pat_map(As, L, Vs, Ts, St);
%% This allows us to use ++ macro in patterns.
%% pattern([call,[quote,erlang],[quote,'++'],A1,A2], L, Vs, St) ->
%%     Pat = foldr(fun (H, T) -> [cons,H,T] end, A2, A1),
%%     pattern(Pat, L, Vs, St);
%% Compile old no contructor list forms.
pattern([H|T], L, Vs0, Ts0, St0) ->
    {Ch,Vs1,Ts1,St1} = pattern(H, L, Vs0, Ts0, St0),
    {Ct,Vs2,Ts2,St2} = pattern(T, L, Vs1, Ts1, St1),
    {c_cons(Ch, Ct),Vs2,Ts2,St2};
pattern([], _, Vs, Ts, St) -> {c_nil(),Vs,Ts,St};
%% Literals.
pattern(Bin, _, Vs, Ts, St) when is_bitstring(Bin) ->
    {pat_lit(Bin),Vs,Ts,St};
pattern(Tup, _, Vs, Ts, St) when is_tuple(Tup) ->
    {pat_lit(Tup),Vs,Ts,St};
pattern(Symb, L, Vs, Ts,St) when is_atom(Symb) ->
    pat_symb(Symb, L, Vs, Ts, St);              %Variable
pattern(Numb, _, Vs, Ts, St) when is_number(Numb) ->
    {c_lit(Numb),Vs,Ts,St}.

pat_list([P|Ps], L, Vs0, Ts0, St0) ->
    {Cp,Vs1,Ts1,St1} = pattern(P, L, Vs0, Ts0, St0),
    {Cps,Vs2,Ts2,St2} = pat_list(Ps, L, Vs1, Ts1, St1),
    {c_cons(Cp, Cps),Vs2,Ts2,St2};
pat_list([], _, Vs, Ts, St) -> {c_nil(),Vs,Ts,St}.

pat_symb('_', L, Vs, Ts, St0) ->                %Don't care variable.
    {Cv,St1} = new_c_var(L, St0),
    {Cv,Vs,Ts,St1};                             %Not added to variables
pat_symb(Symb, _, Vs, Ts, St0) ->
    case is_element(Symb, Vs) of
        true ->                                 %Replace and add test
            {New,St1} = new_var(St0),
            {c_var(New),Vs,[['=:=',Symb,New]|Ts],St1};
        false ->                                %Just add variable
            {c_var(Symb),add_element(Symb, Vs),Ts,St0}
    end.

%% pat_alias(CorePat, CorePat) -> AliasPat.
%%  Normalise aliases. This has been taken from v3_core.erl in the
%%  erlang compiler. This is more complicated in core as we can
%%  sometimes get structures as "literal". Trap bad aliases by
%%  throwing 'nomatch' as these should have been caught in lfe_lint.

pat_alias(Cp1, Cp2) ->
    %% io:format("pa: ~p\n", [{Cp1,Cp2}]),
    case {cerl:type(Cp1),cerl:type(Cp2)} of
        {var,_} -> c_alias(Cp1, Cp2);
        {_,var} -> c_alias(Cp2, Cp1);
        {cons,literal} ->
            pat_alias_cons(Cp1, Cp2);
        {literal,cons} ->
            pat_alias_cons(Cp2, Cp1);
        {cons,cons} ->
            c_cons(pat_alias(cons_hd(Cp1), cons_hd(Cp2)),
                   pat_alias(cons_tl(Cp1), cons_tl(Cp2)));
        {tuple,literal} ->
            pat_alias_tuple(Cp1, Cp2);
        {literal,tuple} ->
            pat_alias_tuple(Cp2, Cp1);
        {tuple,tuple} ->
            c_tuple(pat_alias_list(tuple_es(Cp1), tuple_es(Cp2)));
        {alias,alias} ->
            Cv1 = alias_var(Cp1),
            Cv2 = alias_var(Cp2),
            if Cv1 =:= Cv2 ->
                    pat_alias(alias_pat(Cp1), alias_pat(Cp2));
               true ->
                    c_alias(Cv1, c_alias(Cv2, pat_alias(alias_pat(Cp1),
                                                        alias_pat(Cp2))))
            end;
        {alias,_} ->
            c_alias(alias_var(Cp1), pat_alias(alias_pat(Cp1), Cp2));
        {_,alias} ->
            c_alias(alias_var(Cp2), pat_alias(Cp1, alias_pat(Cp2)));
        _ ->
            %% Check that they are the same except for annotation.
            case {set_ann(Cp1, []),set_ann(Cp2, [])} of
                {P,P} -> Cp1;
                _ -> throw({nomatch,Cp1,Cp2})
            end
    end.

pat_alias_cons(Ccons, Clit) ->
    case lit_val(Clit) of
        [H|T] ->
            %% Must be sure to build a #c_cons{} here
            pat_alias(Ccons, c_cons_skel(c_lit(H), c_lit(T)));
        _ -> throw(nomatch)
    end.

pat_alias_tuple(Ctup, Clit) ->
    case lit_val(Clit) of
        Tup when is_tuple(Tup) ->
            update_c_tuple(Ctup,
                           pat_alias_list(tuple_es(Ctup), data_es(Clit)));
        _ -> throw(nomatch)
    end.

%% pat_alias_list([A1], [A2]) -> [A].

pat_alias_list([A1|A1s], [A2|A2s]) ->
    [pat_alias(A1, A2)|pat_alias_list(A1s, A2s)];
pat_alias_list([], []) -> [];
pat_alias_list(_, _) -> throw(nomatch).

%% pat_binary(Segs, Line, PatVars, VarTests, State) ->
%%     {c_binary(),PatVars,VarTests,State}.

pat_binary(Segs, L, Vs0, Ts0, St0) ->
    Vsps = get_bitsegs(Segs),
    {Csegs,Vs1,Ts1,St1} = pat_bitsegs(Vsps, L, Vs0, Ts0, St0),
    {ann_c_binary([L], Csegs),Vs1,Ts1,St1}.

%% pat_bitsegs(Segs, Line, PatVars, VarTests, State) ->
%%     {CBitsegs,PatVars,VarTests,State}.

pat_bitsegs(Segs, L, Vs0, Ts0, St0) ->
    {Csegs,{Vs1,Ts1,St1}} =
        mapfoldl(fun (S, {Vsa,Tsa,Sta}) ->
                         {Cs,Vsb,Tsb,Stb} = pat_bitseg(S, L, Vsa, Tsa, Sta),
                         {Cs,{Vsb,Tsb,Stb}}
                 end, {Vs0,Ts0,St0}, Segs),
    {Csegs,Vs1,Ts1,St1}.

%% pat_bitseg(Seg, Line, PatVars, State) -> {c_bitstr(),PatVars,State}.
%%  ??? Should noenv be lfe_env:new() instead ???
%%  ??? We know its correct so why worry? ???

pat_bitseg({Pat,_,{Ty,_,Si,En}}, L, Vs0, Ts0, St0)
  when Ty =:= utf8 ; Ty =:= utf16 ; Ty =:= utf32 ->
    %% Special case utf types.
    {Cpat,Vs1,Ts1,St1} = pattern(Pat, L, Vs0, Ts0, St0),
    Undef = c_atom(undefined),
    {c_bitstr(Cpat,Undef,Undef,c_atom(Ty),c_lit([Si,En])),Vs1,Ts1,St1};
pat_bitseg({Pat,all,{binary,_,_,_}=Ty}, L, Vs, Ts, St) ->
    pat_bitseg({Pat,?Q(all),Ty}, L, Vs, Ts, St);
pat_bitseg({Pat,Sz,{Ty,Un,Si,En}}, L, Vs0, Ts0, St0) ->
    {Cpat,Vs1,Ts1,St1} = pattern(Pat, L, Vs0, Ts0, St0),
    {Csize,St2} = comp_expr(Sz, noenv, L, St1),
    {c_bitstr(Cpat, Csize, c_int(Un), c_atom(Ty), c_lit([Si,En])),Vs1,Ts1,St2}.

-ifdef(HAS_MAPS).
%% pat_map(Args, Line, PatVars, State) -> {c_map(),PatVars,State}.

pat_map(Args, L, Vs0, Ts0, St0) ->
    {Pairs,Vs1,Ts1,St1} = pat_map_pairs(Args, L, Vs0, Ts0, St0),
    %% Build #c_map{} then fill it in.
    Map = ann_c_map_pattern([L], Pairs),        %Must us this for a pattern
    {Map,Vs1,Ts1,St1}.

pat_map_pairs([K,V|As], L, Vs0, Ts0, St0) ->
    Ck = pat_map_key(K),
    {Cv,Vs1,Ts1,St1} = pattern(V, L, Vs0, Ts0, St0),
    {Cps,Vs2,Ts2,St2} = pat_map_pairs(As, L, Vs1, Ts1, St1),
    {[ann_c_map_pair([L], c_lit(exact), Ck, Cv)|Cps],
     Vs2,Ts2,St2};
pat_map_pairs([], _, Vs, Ts, St) -> {[],Vs,Ts,St}.

pat_map_key([quote,L]) -> pat_lit(L);
pat_map_key(L) -> pat_lit(L).
-else.
pat_map(_, _, Vs, Ts, St) -> {c_lit(map),Vs,Ts,St}.
-endif.

%% pat_lit(Value) -> LitExpr.
%%  Make a literal expression from an Erlang value. Make it as literal
%%  as is required for a pattern. This function will fail if the value
%%  is not expressable as a literal (for instance, a pid).

pat_lit([H|T]) ->
    Ch = pat_lit(H),
    Ct = pat_lit(T),
    %% c_cons is smart and can handle head and tail both literals.
    c_cons(Ch, Ct);
pat_lit([]) -> c_nil();
pat_lit(T) when is_tuple(T) ->
    Es = pat_lit_list(tuple_to_list(T)),
    %% c_tuple is smart and can handle a list of literals.
    c_tuple(Es);
pat_lit(A) when is_atom(A) -> c_atom(A);
pat_lit(I) when is_integer(I) -> c_int(I);
pat_lit(F) when is_float(F) -> c_float(F);
pat_lit(Bin) when is_bitstring(Bin) ->
    Bits = pat_lit_bitsegs(Bin),
    ann_c_binary([], Bits);
pat_lit(Map) when ?IS_MAP(Map) ->
    pat_lit_map(Map).

pat_lit_list(Vals) -> [ pat_lit(V) || V <- Vals ].

pat_lit_bitsegs(<<B:8,Bits/bitstring>>) ->      %Next byte
    [c_byte_bitseg(B, 8)|pat_lit_bitsegs(Bits)];
pat_lit_bitsegs(<<>>) -> [];                    %Even bytes
pat_lit_bitsegs(Bits) ->                        %Size < 8
    N = bit_size(Bits),
    <<B:N>> = Bits,
    [c_byte_bitseg(B, N)].

-ifdef(HAS_MAPS).
pat_lit_map(Map) ->
    Pairs = pat_lit_map_pairs(maps:to_list(Map)),
    ann_c_map([], c_lit(#{}), Pairs).

pat_lit_map_pairs([{K,V}|Ps]) ->
    [ann_c_map_pair([], c_lit(assoc), pat_lit(K), pat_lit(V))|
     pat_lit_map_pairs(Ps)];
pat_lit_map_pairs([]) -> [].
-else.
pat_lit_map(_) -> c_lit(map).
-endif.

%% line_file_anno(Line, State) -> Anno.
%%  Make annotation with line number and file.

line_file_anno(L, St) ->
    [L,{file,St#cg.file}].

%% comp_gen_anno(Line, State) -> Anno.
%%  Make annotation with line number and compiler_generated.

comp_gen_anno(L, _) ->
    [L,compiler_generated].

%% new_symb(State) -> {Symbol,State}.
%% Create a hopefully new unused symbol.

%% new_symb(St) ->
%%     C = St#cg.vc,
%%     {list_to_atom("|=" ++ integer_to_list(C) ++ "=|"),St#cg{vc=C+1}}.

new_fun_name(Pre, St) ->
    C = St#cg.fc,
    {list_to_atom("'" ++ Pre ++ "~" ++ integer_to_list(C)),St#cg{fc=C+1}}.

%% new_vars(N) -> Vars.

new_vars(N) when N > 0 ->
    V = list_to_atom(integer_to_list(N)),
    [V|new_vars(N-1)];
new_vars(0) -> [].

%% new_var(State) -> {VarName,State}.
%% new_c_var(Line, State) -> {c_var(),State}.
%% Create a hopefully new core variable.

new_var(#cg{vc=C}=St) ->
    {list_to_atom(lists:concat([" ",C," "])),St#cg{vc=C+1}}.

new_c_var(_, St0) ->
    {Name,St1} = new_var(St0),
    {c_var(Name),St1}.

new_c_vars(N, L, St) -> new_c_vars(N, L, St, []).

new_c_vars(N, L, St0, Vs) when N > 0 ->
    {V,St1} = new_c_var(L, St0),
    new_c_vars(N-1, L, St1, [V|Vs]);
new_c_vars(0, _, St, Vs) -> {Vs,St}.

add_vbindings(Vs, Env) ->
    foldl(fun (V, E) -> add_vbinding(V, dummy, E) end, Env, Vs).

%% safe_fetch(Key, Dict, Default) -> Value.

safe_fetch(Key, D, Def) ->
    case find(Key, D) of
        {ok,Val} -> Val;
        error -> Def
    end.

%% is_simple(CoreExp) -> bool().
%%  Test if CoreExp is simple, i.e. just constructs terms.

is_simple(Ce) ->
    case cerl:type(Ce) of
        var -> true;
        literal -> true;
        cons ->
            is_simple(cons_hd(Ce)) andalso is_simple(cons_tl(Ce));
        tuple ->
            is_simple_list(tuple_es(Ce));
        binary ->
            is_simple_bin(binary_segments(Ce));
        _ -> false
    end.

is_simple_list(Es) -> all(fun is_simple/1, Es).

is_simple_bin(Ss) ->
    all(fun (Seg) ->
                is_simple(bitstr_val(Seg)) andalso is_simple(bitstr_size(Seg))
        end, Ss).

%%  Constructor functions for building Core forms. These now just call
%%  functions in cerl.

c_module(Name, Exp, Defs) ->
    cerl:c_module(Name, Exp, Defs).

update_c_module(Mod, Name, Exp, Atts, Defs) ->
    cerl:update_c_module(Mod, Name, Exp, Atts, Defs).

ann_c_call(Ann, M, F, As) ->
    cerl:ann_c_call(Ann, M, F, As).

ann_c_try(Ann, E, Vs, B, Evs, H) ->
    cerl:ann_c_try(Ann, E, Vs, B, Evs, H).

ann_c_fun(Ann, Vs, B) ->
    cerl:ann_c_fun(Ann, Vs, B).

fun_vars(Fun) -> cerl:fun_vars(Fun).
fun_body(Fun) -> cerl:fun_body(Fun).

ann_c_primop(Ann, N, As) ->
    cerl:ann_c_primop(Ann, N, As).

ann_c_let(Ann, Vs, A, B) ->
    cerl:ann_c_let(Ann, Vs, A, B).

ann_c_letrec(Ann, Defs, B) ->
    cerl:ann_c_letrec(Ann, Defs, B).

ann_c_catch(Ann, Body) ->
    cerl:ann_c_catch(Ann, Body).

ann_c_receive(Ann, Cs, To, A) ->
    cerl:ann_c_receive(Ann, Cs, To, A).

ann_c_case(Ann, E, Cs) ->
    cerl:ann_c_case(Ann, E, Cs).

%% Clause functions.
ann_c_clause(Ann, Ps, B) ->                     %Default true guard
    cerl:ann_c_clause(Ann, Ps, B).

ann_c_clause(Ann, Ps, G, B) ->
    cerl:ann_c_clause(Ann, Ps, G, B).

%% Expression sequence functions.
ann_c_seq(Ann, A, B) ->
    cerl:ann_c_seq(Ann, A, B).

update_c_seq(Node, A, B) ->
    cerl:update_c_seq(Node, A, B).

is_c_seq(Node) -> cerl:is_c_seq(Node).

seq_arg(Seq) -> cerl:seq_arg(Seq).
seq_body(Seq) -> cerl:seq_body(Seq).

c_fname(N, A) -> cerl:c_fname(N, A).

ann_c_apply(Ann, Op, As) ->
    cerl:ann_c_apply(Ann, Op, As).

ann_c_values(Ann, Vs) -> cerl:ann_c_values(Ann, Vs).

%% General annotation access functions.
get_ann(Node) -> cerl:get_ann(Node).
set_ann(Node, Ann) -> cerl:set_ann(Node, Ann).

c_alias(Var, Pat) -> cerl:c_alias(Var, Pat).
alias_var(Alias) -> cerl:alias_var(Alias).
alias_pat(Alias) -> cerl:alias_pat(Alias).

%% Atomic data type functions.
c_atom(A) -> cerl:c_atom(A).
ann_c_atom(Ann, A) -> cerl:ann_c_atom(Ann, A).
c_int(I) -> cerl:c_int(I).
c_float(F) -> cerl:c_float(F).
c_nil() -> cerl:c_nil().

%% Literal value functions.
ann_c_lit(Ann, Val) -> cerl:ann_abstract(Ann, Val). %Generic literal
c_lit(Val) -> cerl:abstract(Val).
is_literal(Node) -> cerl:is_literal(Node).
lit_val(Lit) -> cerl:concrete(Lit).

data_es(Data) -> cerl:data_es(Data).

c_cons(Hd, Tl) -> cerl:c_cons(Hd, Tl).
c_cons_skel(Hd, Tl) -> cerl:c_cons_skel(Hd, Tl).
cons_hd(Cons) -> cerl:cons_hd(Cons).
cons_tl(Cons) -> cerl:cons_tl(Cons).

c_tuple(Es) -> cerl:c_tuple(Es).
update_c_tuple(Tup, Es) ->
    cerl:update_c_tuple(Tup, Es).
tuple_es(Tup) -> cerl:tuple_es(Tup).

c_var(N) -> cerl:c_var(N).

ann_c_binary(Ann, Segs) -> cerl:ann_c_binary(Ann, Segs).
update_c_binary(Bin, Segs) ->
    cerl:update_c_binary(Bin, Segs).
binary_segments(Bin) -> cerl:binary_segments(Bin).

c_bitstr(Val, Sz, Un, Ty, Fs) ->
    cerl:c_bitstr(Val, Sz, Un, Ty, Fs).
update_c_bitstr(Bit, Val, Sz, Un, Ty, Fs) ->
    cerl:update_c_bitstr(Bit, Val, Sz, Un, Ty, Fs).
bitstr_val(Bit) -> cerl:bitstr_val(Bit).
bitstr_size(Bit) -> cerl:bitstr_size(Bit).
bitstr_unit(Bit) -> cerl:bitstr_unit(Bit).
bitstr_type(Bit) -> cerl:bitstr_type(Bit).
bitstr_flags(Bit) -> cerl:bitstr_flags(Bit).

-ifdef(HAS_MAPS).
ann_c_map(Ann, Arg, Ps) ->
    cerl:ann_c_map(Ann, Arg, Ps).

%% ann_c_map_pattern(Ann, Pairs) -> Map
%%  This function will come first in 18. Until then this is a little
%%  tricky as ann_c_map will create a literal if the map pattern is a
%%  literal and this is NOT what the compiler wants.

ann_c_map_pattern(Ann, Ps) ->
    case erlang:function_exported(cerl, ann_c_map_pattern, 2) of
        true ->
            cerl:ann_c_map_pattern(Ann, Ps);
        false ->
            Map0 = ann_c_map(Ann, dummy, Ps),
            update_c_map(Map0, c_lit(#{}), Ps)
    end.

update_c_map(Map, Arg, Ps) ->
    cerl:update_c_map(Map, Arg, Ps).

ann_c_map_pair(Ann, Op, Key, Val) ->
    cerl:ann_c_map_pair(Ann, Op, Key, Val).
-endif.
