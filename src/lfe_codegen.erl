%% Copyright (c) 2008-2018 Robert Virding
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
%%% Purpose : Lisp Flavoured Erlang code generator (to Erlang AST).

%%% We have to be very careful to generate annotations in exactly the
%%% same way as the erlang compiler does and in the same places.
%%% Dialyzer is very finnicky about this and seriously fails if things
%%% are not as it expects them to be. Note that now the whole
%%% annotation is passed into the constructor functions, not just the
%%% line number.

-module(lfe_codegen).

-export([module/2]).

%% -compile(export_all).

-import(lists, [member/2,keysearch/3,reverse/1,
                all/2,map/2,foldl/3,foldr/3,mapfoldl/3,mapfoldr/3,
                concat/1,zipwith/3]).
-import(ordsets, [add_element/2,is_element/2,from_list/1,union/2]).
-import(orddict, [store/3,find/2]).

%% -import(lfe_env, [new/0,add_env/2,
%%                   add_vbinding/3,get_vbinding/2,add_fbinding/4,
%%                   add_ibinding/5,get_gbinding/3]).

-include("lfe_comp.hrl").

%% Define IS_MAP/1 macro for is_map/1 bif.
-ifdef(HAS_MAPS).
-define(IS_MAP(T), is_map(T)).
-else.
-define(IS_MAP(T), false).
-endif.

-define(Q(E), [quote,E]).                       %We do a lot of quoting!

-record(cg, {module=[],                         %Module name
             mline=0,                           %Module definition line
             exps=[],                           %Exports (ordsets)
             imps=[],                           %Imports (orddict)
             pref=[],                           %Prefixes
             atts=[],                           %Attrubutes
             mets=[],                           %Metadata
             defs=[],                           %Function definitions.
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
    compile_forms(Fbs, St1).

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
    St1 = collect_metas(Metas, L, St0#cg{module=Mod,mline=L}),
    {Fds,collect_attrs(Atts, L, St1)};
collect_form({['extend-module',Meta,Atts],L}, {Fds,St0}) ->
    St1 = collect_metas(Meta, L, St0),
    {Fds,collect_attrs(Atts, L, St1)};
collect_form({['define-type',Type,Def],L}, {Fds,St}) ->
    {Fds,collect_meta([type,[Type,Def]], L, St)};
collect_form({['define-opaque-type',Type,Def],L}, {Fds,St}) ->
    {Fds,collect_meta([opaque,[Type,Def]], L, St)};
collect_form({['define-function-spec',Func,Spec],L}, {Fds,St}) ->
    {Fds,collect_meta([spec,[Func,Spec]], L, St)};
collect_form({['define-function',Name,Meta,Def],L}, {Fds,St}) ->
    {collect_function(Name, Meta, Def, L, Fds),St};
%% Ignore macro definitions and eval-when-compile forms.
collect_form({['define-macro'|_],_}, {Fds,St}) -> {Fds,St};
collect_form({['eval-when-compile'|_],_}, {Fds,St}) -> {Fds,St}.

%% collect_metas(Metas, Line, State) -> State.
%%  Collect module metadata which is to be compiled. Only type
%%  information is to be kept. There can be only one type/spec/record
%%  per attribute.

collect_metas(Ms, L, St) ->
    foldl(fun (M, S) -> collect_meta(M, L, S) end, St, Ms).

collect_meta([type|Tds], L, #cg{mets=Ms}=St) ->
    Ts = [ {type,Td,L} || Td <- Tds ],
    St#cg{mets=Ms ++ Ts};
collect_meta([opaque|Tds], L, #cg{mets=Ms}=St) ->
    Os = [ {opaque,Td,L} || Td <- Tds ],
    St#cg{mets=Ms ++ Os};
collect_meta([spec|Sps], L, #cg{mets=Ms}=St) ->
    Ss = [ {spec,Sp,L} || Sp <- Sps ],
    St#cg{mets=Ms ++ Ss};
collect_meta([record|Rds], L, #cg{mets=Ms}=St) ->
    Rs = [ {record,Rd,L} || Rd <- Rds ],
    St#cg{mets=Ms ++ Rs};
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
collect_attr([N,V], L, #cg{atts=As}=St) ->      %Common case
    St#cg{atts=As ++ [{N,V,L}]};
collect_attr([N|Vs], L, #cg{atts=As}=St) ->
    St#cg{atts=As ++ [{N,Vs,L}]}.

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
    Pstr = atom_to_list(Pre),                   %Store prefix as string
    St#cg{pref=store(Pstr, Mod, St#cg.pref)}.

collect_imp(Fun, Mod, St, Fs) ->
    Imps0 = safe_fetch(Mod, St#cg.imps, []),
    Imps1 = foldl(Fun, Imps0, Fs),
    St#cg{imps=store(Mod, Imps1, St#cg.imps)}.

collect_function(Name, _Meta, Def, L, Fds) ->
    %% Ignore the meta data.
    Fs = lfe_codelift:function(Name, Def, L),   %Lift all local functions
    Fs ++ Fds.

%% compile_forms(Forms, State) -> {ErlForms,State}.
%%  Compile the forms from the file as stored in the state record.

compile_forms(Fbs0, St0) ->
    %% Make initial environment and set state.
    St1 = St0#cg{defs=Fbs0},                    %Save the forms in #cg{}
    Exp = comp_export(Fbs0, St1),
    Imps = comp_imports(St1),
    Atts = comp_attributes(St1),
    Mets = map(fun (Meta) ->
                       %% io:format("cm: ~p\n", [Meta]),
                       comp_metadata(Meta)
               end, St1#cg.mets),
    %% Compile the functions.
    {Edefs,St2} = mapfoldl(fun (D, St) -> comp_define(D, St) end,
                           St1, St1#cg.defs),
    Mline = St2#cg.mline,
    Erl = [make_attribute(file, {St2#cg.file,Mline}, Mline),
           make_attribute(module, St2#cg.module, Mline),
           Exp|
           Imps ++ Atts ++ Mets ++ Edefs],
    {Erl,St2}.

comp_export(Fbs, #cg{exps=Exps,mline=Line}) ->
    Es = if Exps =:= all ->
                 [ {F,func_arity(Def)} || {F,Def,_} <- Fbs ];
            true -> Exps                        %Already in right format
         end,
    make_attribute(export, Es, Line).

comp_imports(#cg{mline=L,imps=Imps}) ->
    Mfun = fun ({Mod,Imps0}) ->
                   Ifun = fun ({{N,Ar},N}) -> {N,Ar} end,
                   Imps1 = lists:map(Ifun, Imps0),
                   make_attribute(import, {Mod,Imps1}, L)
           end,
    lists:map(Mfun, Imps).

comp_attributes(#cg{atts=Atts}) ->
    %% These are already in the right format.
    Afun = fun ({N, V, L}) -> make_attribute(N, V, L) end,
    map(Afun, Atts).

%% comp_metadata(Metadata) -> CoreAttr.
%%  Compile metadata handling the special cases.

comp_metadata({type,Type,Line}) ->
    comp_type_metadata(type, Type, Line);
comp_metadata({opaque,Type,Line}) ->
    comp_type_metadata(opaque, Type, Line);
comp_metadata({spec,Spec,Line}) ->
    comp_spec_metadata(Spec, Line);
comp_metadata({record,Record,Line}) ->
    comp_record_metadata(Record, Line);
comp_metadata({N,V,Line}) ->
    make_attribute(N, V, Line).

comp_type_metadata(Attr, [[Type|Args],Def], Line) ->
    Tdef = {Type,
            lfe_types:to_type_def(Def, Line),
            lfe_types:to_type_defs(Args, Line)},
    make_attribute(Attr, Tdef, Line).

comp_spec_metadata([[Name,Ar],Spec], Line) ->
    Sdef = {{Name,Ar},lfe_types:to_func_spec_list(Spec, Line)},
    make_attribute(spec, Sdef, Line).

%% comp_record_metadata(Record, Line) -> Metadata.
%%  Format depends on whether 18 and older or newer.

-ifdef(NEW_REC_CORE).
comp_record_metadata([Name|Fields], Line) ->
    Fdefs = [ comp_record_field(Fdef, Line) || Fdef <- Fields ],
    make_attribute(record, {Name,Fdefs}, Line).
-else.
comp_record_metadata([Name|Fields], Line) ->
    Fdefs = [ comp_record_field(Fdef, Line) || Fdef <- Fields ],
    make_attribute(type, {{record,Name},Fdefs}, Line).
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

%% comp_define(DefForm, State) -> {ErlFunc,State}.
%%  Compile a top-level define. Sets current function name.

comp_define({Name,Def,L}, St) ->
    {'fun',_,{clauses,Clauses}} = lfe_trans:to_expr(Def, L),
    %% io:format("~p\n", [{Def,Clauses}]),
    {{function,L,Name,func_arity(Def),Clauses},St}.

%% match_lambda_arity(MatchClauses) -> int().

match_lambda_arity([[Pats|_]|_]) -> length(Pats).

%% func_arity(FuncDef) -> Arity.
%%  Return the arity of a function definition.

func_arity([lambda,Args|_]) -> length(Args);
func_arity(['match-lambda'|Cls]) ->
    match_lambda_arity(Cls).

%% safe_fetch(Key, Dict, Default) -> Value.

safe_fetch(Key, D, Def) ->
    case find(Key, D) of
        {ok,Val} -> Val;
        error -> Def
    end.

make_attribute(Name, Val, Line) ->
    {attribute,Line,Name,Val}.
