%% Copyright (c) 2008-2020 Robert Virding
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

%%% We must be careful to generate codee in the right order so as not
%%% to generate something the Erlang AST compiler won't find errors
%%% that are due to ordering but don't really exist. We first collect
%%% and generate all the "real" attributes from the module forms which
%%% must be first but we keep type/spec/record declarations in the
%%% same relative place as in the original file.
%%%
%%% Note that for (export all) we only export the top-level functions
%%% defined in the module, not any of the lambda lifted
%%% functions. This means that we cannot generate
%%% "-compile(export_all)." but must explicitly export the functions.

-module(lfe_codegen).

-export([module/2]).

%% -compile(export_all).

-include("lfe.hrl").
-include("lfe_comp.hrl").

-record(lfe_cg, {module=[],                     %Module name
                 mline=0,                       %Module definition line
                 exps=[],                       %Exports (ordsets)
                 imps=[],                       %Imports (orddict)
                 pref=[],                       %Prefixes
                 atts=[],                       %Attrubutes
                 mets=[],                       %Metadata
                 defs=[],                       %Defined top-level functions
                 opts=[],                       %Options
                 file=[],                       %File name
                 func=[]                        %Current function
            }).

%% module(ModuleForms, CompInfo) ->
%%     {ok,ModuleName,ASTModule,[Warning]} | {error,[Error],[Warning]}.

module(Mfs, #cinfo{opts=Opts,file=File}) ->
    St0 = #lfe_cg{opts=Opts,file=File},
    {AST,St1} = compile_module(Mfs, St0),
    {ok,St1#lfe_cg.module,AST,[]}.

compile_module(Mfs, St0) ->
    %% Collect all the module attributes and output them first.
    St1 = collect_mod_defs(Mfs, St0),
    Attrs = compile_attributes(St1),
    %% Now we do the meta, function and record forms in order.
    Forms = compile_forms(Mfs, St1),
    {Attrs ++ Forms,St1}.

%% collect_mod_defs(ModuleForms, State) -> State.
%%  Collect the attributee information in define-module and
%%  extend-module's which must be first in the output file.

collect_mod_defs(Mfs, St) ->
    lists:foldl(fun collect_mod_def/2, St, Mfs).

collect_mod_def({['define-module',Mod,_Metas,Attrs],Line}, St0) ->
    St1 = coll_mdef_attrs(Attrs, Line, St0),
    St1#lfe_cg{module=Mod,mline=Line};
collect_mod_def({['extend-module',_Metas,Attrs],Line}, St0) ->
    coll_mdef_attrs(Attrs, Line, St0);
collect_mod_def({['define-function',Name,_Meta,Def],Line},
                #lfe_cg{defs=Defs}=St) ->
    %% Must save all functions for export all.
    St#lfe_cg{defs=Defs ++ [{Name,Def,Line}]};
collect_mod_def(_Form, St) -> St.               %Ignore everything else here

%% coll_mdef_attrs(Attributes, Line, State) -> State.
%%  Collect all the module attributes.

coll_mdef_attrs(Attrs, Line, St) ->
    lists:foldl(fun (A, S) -> coll_mdef_attr(A, Line, S) end, St, Attrs).

coll_mdef_attr([export|Es], _Line, St) ->
    coll_mdef_exps(Es, St);
coll_mdef_attr([import|Is], _Line, St) ->
    coll_mdef_imps(Is, St);
%% Explicitly ignore any doc or record information here.
coll_mdef_attr([doc|_], _Line, St) -> St;
coll_mdef_attr([record|_], _Line, St) -> St;
%% Save anything else and get the format right.
coll_mdef_attr([Name,Val], Line, #lfe_cg{atts=As}=St) ->
    St#lfe_cg{atts=As ++ [{Name,Val,Line}]};
coll_mdef_attr([Name|Vals], Line, #lfe_cg{atts=As}=St) ->
    St#lfe_cg{atts=As ++ [{Name,Vals,Line}]}.

%% coll_mdef_exps(Export, State) -> State.
%%  Collect exports special casing 'all'.

coll_mdef_exps([all], St) -> St#lfe_cg{exps=all};
coll_mdef_exps(_Exps, #lfe_cg{exps=all}=St) -> St;
coll_mdef_exps(Exps, #lfe_cg{exps=Exps0}=St) ->
    Exps1 = lists:foldl(fun ([F,A], E) -> ordsets:add_element({F,A}, E) end,
                        Exps0, Exps),
    St#lfe_cg{exps=Exps1}.

%% coll_mdef_imps(Imports, State) -> State.
%%  Collect imports.

coll_mdef_imps(Imps, St) ->
    lists:foldl(fun (I, S) -> coll_mdef_imp(I, S) end, St, Imps).

coll_mdef_imp(['from',Mod|Fs], St) ->
    coll_mdef_imp(fun ([F,A], Imps) -> orddict:store({F,A}, F, Imps) end,
                  Mod, St, Fs);
coll_mdef_imp(['rename',Mod|Rs], St) ->
    coll_mdef_imp(fun ([[F,A],R], Imps) -> orddict:store({F,A}, R, Imps) end,
                  Mod, St, Rs);
coll_mdef_imp(['prefix',Mod,Pre], St) ->
    Pstr = atom_to_list(Pre),                   %Store prefix as string
    St#lfe_cg{pref=orddict:store(Pstr, Mod, St#lfe_cg.pref)}.

coll_mdef_imp(Fun, Mod, St, Fs) ->
    Imps0 = safe_fetch(Mod, St#lfe_cg.imps, []),
    Imps1 = lists:foldl(Fun, Imps0, Fs),
    St#lfe_cg{imps=orddict:store(Mod, Imps1, St#lfe_cg.imps)}.

%% compile_attributes(State) -> MdefAST.
%%  Compile the module attributes.

compile_attributes(St) ->
    Exp = comp_export(St),
    Imps = comp_imports(St),
    Atts = comp_attributes(St),
    Mline = St#lfe_cg.mline,
    %% Collect all the attributes.
    AST = [make_attribute(file, {St#lfe_cg.file,Mline}, Mline),
           make_attribute(module, St#lfe_cg.module, Mline),
           Exp |
           Imps ++ Atts],
    AST.

%% compile_forms(ModuleForms, State) -> [AST].
%%  Compile the function and record forms into Erlang ASTs.

compile_forms(Forms, St) ->
    lists:flatmap(fun (F) -> compile_form(F, St) end, Forms).

compile_form({['define-module',_Mod,Metas,_Attrs],Line}, St) ->
    comp_mod_metas(Metas, Line, St);
compile_form({['extend-module',Metas,_Attrs],Line}, St) ->
    comp_mod_metas(Metas, Line, St);
compile_form({['define-type',Type,Def],Line}, _St) ->
    comp_type_def(type, Type, Def, Line);
compile_form({['define-opaque-type',Type,Def],Line}, _St) ->
    comp_type_def(opaque, Type, Def, Line);
compile_form({['define-function-spec',Func,Spec],Line}, _St) ->
    comp_function_spec(Func, Spec, Line);
compile_form({['define-record',Name,Fields],Line}, _St) ->
    comp_record_def(Name, Fields, Line);
compile_form({['define-function',Name,_Meta,Def],Line}, _St) ->
    comp_function_def(Name, Def, Line);
%% Ignore anything else for now. Hopefully there shouldn't be anything
%% else.
compile_form(_Other, _St) -> [].

%% comp_mod_metas(Metas, Line, State) -> [AST].

comp_mod_metas(Metas, Line, _St) ->
    lists:flatmap(fun (M) -> comp_mod_meta(M, Line) end, Metas).

comp_mod_meta([type|Tdefs], Line) ->
    lists:flatmap(fun (Tdef) -> comp_type_def(type, Tdef, Line) end, Tdefs);
comp_mod_meta([opaque|Tdefs], Line) ->
    lists:flatmap(fun (Tdef) -> comp_type_def(opaque, Tdef, Line) end, Tdefs);
comp_mod_meta([spec|Fspecs], Line) ->
    Fun = fun (Fspec) -> comp_function_spec(Fspec, Line) end,
    lists:flatmap(Fun, Fspecs);
comp_mod_meta([record|Rdefs], Line) ->
    Fun = fun ([Rec,Fds]) -> comp_record_def(Rec, Fds, Line) end,
    lists:flatmap(Fun, Rdefs);
comp_mod_meta(_Meta, _Line) -> [].

%% comp_type_def(Attr, TypeDef, Line) -> [AST].
%% comp_type_def(Attr, Type, Def, Line) -> [AST].

comp_type_def(Attr, [Type,Def], Line) ->
    comp_type_def(Attr, Type, Def, Line).

comp_type_def(Attr, [Type|Args], Def, Line) ->
    Tdef = {Type,
            lfe_types:to_type_def(Def, Line),
            lfe_types:to_type_defs(Args, Line)},
    [make_attribute(Attr, Tdef, Line)].

%% comp_function_spec(FuncSpec, Line) -> [AST].
%% comp_function_spec(Func, Spec, Line) -> [AST].

comp_function_spec([Func|Spec], Line) ->
    comp_function_spec(Func, Spec, Line).

comp_function_spec([Name,Ar], Spec, Line) ->
    Sdef = {{Name,Ar},lfe_types:to_func_spec_list(Spec, Line)},
    [make_attribute(spec, Sdef, Line)].

%% comp_function_def(Func, Def, Line) -> [AST].
%%  Lambda lift the function returning all the functions.

comp_function_def(Name, Def, Line) ->
    %% This also returns the defined top function.
    Lfs = lfe_codelift:function(Name, Def, Line),
    lists:map(fun ({N,D,L}) ->
                      {'fun',_,{clauses,Clauses}} = lfe_trans:to_expr(D, L),
                      {function,L,N,func_arity(D),Clauses}
              end, Lfs).

%% comp_record_def(Record, Line) -> [Attribute].
%%  Format depends on whether 18 and older or newer.

comp_record_def(Name, Fields, Line) ->
    Fdefs = [ comp_record_field(Fdef, Line) || Fdef <- Fields ],
    [make_record_attribute(Name, Fdefs, Line)].

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

-ifdef(NEW_REC_CORE).
make_record_attribute(Name, Fdefs, Line) ->
    make_attribute(record, {Name,Fdefs}, Line).
-else.
make_record_attribute(Name, Fdefs, Line) ->
    make_attribute(type, {{record,Name},Fdefs}, Line).
-endif.

%% comp_export(State) -> Attribute.
%% comp_imports(State) -> [Attribute].
%% comp_attributes(State) -> [Attribute].

comp_export(#lfe_cg{exps=Exps,defs=Defs,mline=Line}) ->
    Es = if Exps =:= all ->
                 [ {F,func_arity(Def)} || {F,Def,_} <- Defs ];
            true -> Exps                        %Already in right format
         end,
    make_attribute(export, Es, Line).

comp_imports(#lfe_cg{mline=L,imps=Imps}) ->
    Mfun = fun ({Mod,Imps0}) ->
                   Ifun = fun ({{N,Ar},N}) -> {N,Ar} end,
                   Imps1 = lists:map(Ifun, Imps0),
                   make_attribute(import, {Mod,Imps1}, L)
           end,
    lists:map(Mfun, Imps).

comp_attributes(#lfe_cg{atts=Atts}) ->
    lists:map(fun comp_attribute/1, Atts).

%% comp_attribute({spec,[Func|Spec],Line}) ->
%%     hd(comp_func_spec(Func, Spec, Line));       %We know!
comp_attribute({Name,Val,Line}) ->
    make_attribute(Name, Val, Line).

%% make_attribute(Name, Value, Line) -> Atttribute.

make_attribute(Name, Val, Line) ->
    {attribute,Line,Name,Val}.

%% func_arity(FuncDef) -> Arity.
%%  Return the arity of a function definition.

func_arity([lambda,Args|_]) -> length(Args);
func_arity(['match-lambda'|Cls]) ->
    match_lambda_arity(Cls).

%% match_lambda_arity(MatchClauses) -> int().

match_lambda_arity([[Pats|_]|_]) -> length(Pats).

%% safe_fetch(Key, Dict, Default) -> Value.

safe_fetch(Key, D, Def) ->
    case orddict:find(Key, D) of
        {ok,Val} -> Val;
        error -> Def
    end.
