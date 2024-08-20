%% Copyright (c) 2008-2024 Robert Virding
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

%%% We must be careful to generate code in the right order so as not
%%% to generate something the Erlang AST compiler won't find errors
%%% that are due to ordering but don't really exist. We first collect
%%% and generate all the "real" attributes from the module forms which
%%% must be first but we keep type/spec/record declarations in the
%%% same relative place as in the original file.
%%%
%%% Note that for (export all) we only export the top-level functions
%%% defined in the module, not any of the lambda lifted functions.
%%% This means that we cannot generate "-compile(export_all)." but
%%% must explicitly export the functions.
%%%
%%% Having import from and rename forces us to explicitly convert the
%%% call as we can't use an import attribute to do this properly for
%%% us. Hence we collect the imports here and pass them into
%%% lfe_translate.
%%%
%%% Module aliases are also collected here and passed on to
%%% lfe_translate.

-module(lfe_codegen).

-export([module/2,format_error/1]).

%% -compile(export_all).

-include("lfe.hrl").
-include("lfe_comp.hrl").

-record(lfe_cg, {module=[],                     %Module name
                 mline=0,                       %Module definition line
                 exports=ordsets:new(),         %Exports
                 imports=orddict:new(),         %Imports
                 aliases=orddict:new(),         %Aliases
                 onload=[],                     %Onload
                 records=[],                    %Records
                 struct=undefined,              %Struct definition
                 attrs=[],                      %Attrubutes
                 metas=[],                      %Meta data
                 funcs=orddict:new(),           %Defined top-level functions
                 opts=[],                       %Compiler options
                 file=[],                       %File name
                 func=[],                       %Current function
                 errors=[],                     %Errors
                 warnings=[]                    %Warnings
            }).

%% Errors.
format_error({illegal_code,Code}) ->
    lfe_io:format1(<<"illegal ~w code">>, [Code]).

%% module(ModuleForms, CompInfo) ->
%%     {ok,ModuleName,ASTModule,[Warning]} | {error,[Error],[Warning]}.

module(Mfs, #cinfo{opts=Opts,file=File}) ->
    St0 = #lfe_cg{opts=Opts,file=File},
    {AST,St1} = compile_module(Mfs, St0),
    %% io:format("st1 ~p\n", [St1]),
    return_status(AST, St1).

return_status(AST, #lfe_cg{module=M,errors=[]}=St) ->
    {ok,M,AST,St#lfe_cg.warnings};
return_status(_AST, St) ->
    {error,St#lfe_cg.errors,St#lfe_cg.warnings}.

compile_module(Mfs, St0) ->
    %% Collect all the module attributes and output them first.
    St1 = collect_mod_defs(Mfs, St0),
    %% Build the struct functions then __info__ function last.
    St2 = build_struct_def(St1),
    St3 = build_info_func(St2),                 %Must be last!
    Attrs = compile_attributes(St3),
    %% Now we do the meta, function and record forms in order. Here we
    %% can get translation errors.
    %% Forms = compile_functions(St1),
    {Functions,St4} =
        try
            {compile_functions(St3),St3}
        catch
            error:{illegal_code,Line,Code} ->
                {[],add_error(Line, {illegal_code,Code}, St3)}
        end,
    {Attrs ++ Functions,St4}.

%% collect_mod_defs(ModuleForms, State) -> State.
%%  Collect all the information in the module deinition for processing.

collect_mod_defs(Mfs, St) ->
    lists:foldl(fun collect_mod_def/2, St, Mfs).

collect_mod_def({['define-module',Mod,Metas,Attrs],Line}, St0) ->
    %% And now add the rest of the attributes and metas.
    St1 = coll_mdef_attrs(Attrs, Line, St0),
    St2 = coll_mdef_metas(Metas, Line, St1),
    St2#lfe_cg{module=Mod,mline=Line};
collect_mod_def({['extend-module',Metas,Attrs],Line}, St0) ->
    St1 = coll_mdef_attrs(Attrs, Line, St0),
    coll_mdef_metas(Metas, Line, St1);
collect_mod_def({['define-type',Type,Def],Line}, St) ->
    coll_mdef_meta([type,[Type,Def]], Line, St);
collect_mod_def({['define-opaque-type',Type,Def],Line}, St) ->
    coll_mdef_meta([opaque,[Type,Def]], Line, St);
collect_mod_def({['define-function-spec',Func,Specs],Line}, St) ->
    %% io:format("lc ~p\n", [{cmd,Func,Specs}]),
    coll_func_spec(Func, Specs, Line, St);
collect_mod_def({['define-record',Name,Fields],Line}, St) ->
    %% io:format("cmd ~p\n", [[record,[Name,Fields]]]),
    coll_mdef_meta([record,[Name,Fields]], Line, St);
collect_mod_def({['define-struct',Fields],Line}, St) ->
    St#lfe_cg{struct={Fields,Line}};
collect_mod_def({['define-function',Name,_Meta,Def],Line},
                #lfe_cg{funcs=Funcs0}=St) ->
    %% Must save all the functions.
    Arity = func_arity(Def),
    Funcs1 =  orddict:store({Name,Arity}, {Def,Line}, Funcs0),
    St#lfe_cg{funcs=Funcs1};
collect_mod_def(_Form, St) -> St.               %Ignore everything else here

%% coll_mdef_attrs(Attributes, Line, State) -> State.
%%  Collect all the module attributes. Keep the attributes in order.

coll_mdef_attrs(Attrs, Line, St) ->
    lists:foldl(fun (A, S) -> coll_mdef_attr(A, Line, S) end, St, Attrs).

coll_mdef_attr([export|Es], _Line, St) ->
    coll_mdef_exports(Es, St);
coll_mdef_attr([import|Is], _Line, St) ->
    coll_mdef_imports(Is, St);
coll_mdef_attr(['module-alias'|As], _Line, St) ->
    coll_mdef_aliases(As, St);
coll_mdef_attr([on_load,Onload], _Line, St) ->
    coll_mdef_onload(Onload, St);
%% Explicitly ignore any doc here.
coll_mdef_attr([doc|_], _Line, St) -> St;
coll_mdef_attr([record|Recs], Line, St) ->
    coll_mdef_records(Recs, Line, St);
%% Save anything else and get the format right.
coll_mdef_attr([Name,Val], Line, #lfe_cg{attrs=Attrs}=St) ->
    St#lfe_cg{attrs=Attrs ++ [{Name,Val,Line}]};
coll_mdef_attr([Name|Vals], Line, #lfe_cg{attrs=Attrs}=St) ->
    St#lfe_cg{attrs=Attrs ++ [{Name,Vals,Line}]}.

%% coll_mdef_exports(Export, State) -> State.
%%  Collect exports special casing 'all'.

coll_mdef_exports([all], St) -> St#lfe_cg{exports=all};
coll_mdef_exports(_Exps, #lfe_cg{exports=all}=St) -> St;
coll_mdef_exports(Exps, #lfe_cg{exports=Exps0}=St) ->
    Exps1 = lists:foldl(fun ([F,A], E) -> ordsets:add_element({F,A}, E) end,
                        Exps0, Exps),
    St#lfe_cg{exports=Exps1}.

%% coll_mdef_imports(Imports, State) -> State.
%%  Collect imports keeping track of local and imported names.

coll_mdef_imports(Imps, St) ->
    lists:foldl(fun (I, S) -> coll_mdef_import(I, S) end, St, Imps).

coll_mdef_import(['from',Mod|Fs], St) ->
    Ifun = fun ([F,A], Ifs) -> orddict:store({F,A}, {Mod,F}, Ifs) end,
    coll_mdef_import(Ifun, St, Fs);
coll_mdef_import(['rename',Mod|Fs], St) ->
    %% Get it right here, R is the renamed local called function, F is
    %% the name in the other module.
    Ifun = fun ([[F,A],R], Ifs) -> orddict:store({R,A}, {Mod,F}, Ifs) end,
    coll_mdef_import(Ifun, St, Fs).

coll_mdef_import(Fun, #lfe_cg{imports=Imps0}=St, Fs) ->
    Imps1 = lists:foldl(Fun, Imps0, Fs),
    St#lfe_cg{imports=Imps1}.

%% coll_mdef_aliases(Aliases, State) -> State.
%%  Collect the module aliases.

coll_mdef_aliases(As, #lfe_cg{aliases=Als0}=St) ->
    Als1 = lists:foldl(fun ([M,A], Mas) -> orddict:store(A, M, Mas) end,
                       Als0, As),
    St#lfe_cg{aliases=Als1}.

%% coll_mdef_onload(Onload, State) -> State.
%%  Collect the on_load function name.

coll_mdef_onload([Name,Ar], St) ->
    St#lfe_cg{onload={Name,Ar}}.

%% coll_mdef_records(Records, Line, State) -> State.
%%  Collect the record definitions.

coll_mdef_records(RecordDefs, Line, St) ->
    Fun = fun ([Name,Fields], #lfe_cg{records=Recs}=S) ->
                  S#lfe_cg{records=Recs ++ [{Name,Fields,Line}]}
          end,
    lists:foldl(Fun, St, RecordDefs).

%% coll_mdef_metas(Metas, Line, State) -> State.
%%  Collect all the module metas. Keep the metas in order.

coll_mdef_metas(Metas, Line, St) ->
    lists:foldl(fun (M, S) -> coll_mdef_meta(M, Line, S) end, St, Metas).

coll_mdef_meta([type|Tdefs], Line, #lfe_cg{metas=Metas}=St) ->
    St#lfe_cg{metas=Metas ++ [{type,Tdefs,Line}]};
coll_mdef_meta([opaque|Tdefs], Line, #lfe_cg{metas=Metas}=St) ->
    St#lfe_cg{metas=Metas ++ [{opaque,Tdefs,Line}]};
coll_mdef_meta([spec|Specs], Line, St) ->
    coll_mdef_specs(Specs, Line, St);
coll_mdef_meta([record|Rdefs], Line, #lfe_cg{metas=Metas}=St) ->
    St#lfe_cg{metas=Metas ++ [{record,Rdefs,Line}]};
%% Ignore other metas.
coll_mdef_meta(_Meta, _Line, St) ->
    St.

coll_mdef_specs(Specs, Line, St) ->
    Coll = fun ([Func,Spec], S) -> coll_func_spec(Func, Spec, Line, S) end,
    lists:foldl(Coll, St, Specs).

coll_func_spec(Func, Specs, Line, #lfe_cg{metas=Metas}=St) ->
    St#lfe_cg{metas=Metas ++ [{spec,[Func,Specs],Line}]}.

%% build_struct_def(State) -> State.
%% build_info_func(State) -> State.
%%  Create functions which are built from the data collected from the
%%  file. Here the __info__ function and the struct definition
%%  functions.

build_struct_def(#lfe_cg{struct=undefined}=St) ->
    %% No struct has been defined.
    St;
build_struct_def(#lfe_cg{module=Mod,struct={Fields,Line},funcs=Funcs0}=St0) ->
    %% The default struct.
    DefStr = comp_struct_map(Mod, Fields),
    %% The default __struct__/0/1 functions.
    StrFun_0 = struct_fun_0(DefStr),
    StrFun_1 = struct_fun_1(DefStr),
    Funcs1 = orddict:store({'__struct__',0}, {StrFun_0,Line}, Funcs0),
    Funcs2 = orddict:store({'__struct__',1}, {StrFun_1,Line}, Funcs1),
    St1 = coll_mdef_exports([['__struct__',0],['__struct__',1]], St0),
    St1#lfe_cg{funcs=Funcs2}.

comp_struct_map(Mod, Fields) ->
    Fun = fun ([F,D|_]) -> {F,D};
              ([F]) -> {F,'nil'};
              (F) -> {F,'nil'}
          end,
    KeyVals = lists:map(Fun, Fields),
    maps:from_list([{'__struct__',Mod}|KeyVals]).

%% struct_fun_0(DefStr) -> FuncDef.
%% struct_fun_1(DefStr) -> FuncDef.
%%  Create the bodies __struct__/0/1 functions.

struct_fun_0(DefStr) ->
    [lambda,[],DefStr].

struct_fun_1(DefStr) ->
    [lambda,[assocs],
     [call,?Q(lists),?Q(foldl),
      ['match-lambda',[[[tuple,x,y],acc],
                       [call,?Q(maps),?Q(update),x,y,acc]]],
      DefStr,assocs]].

build_info_func(#lfe_cg{module=Mod,mline=Line,funcs=Funcs0}=St0) ->
    %% The default clauses.
    InfoCls = [[[?Q(module)],?Q(Mod)],
               [[?Q(functions)],?Q(info_functions(St0))],
               [[?Q(macros)],[]],
               [[?Q(deprecated)],[]],
               [[?Q(attributes)],
                [call,?Q(erlang),?Q(get_module_info),?Q(Mod),?Q(attributes)]],
               [[?Q(compile)],
                [call,?Q(erlang),?Q(get_module_info),?Q(Mod),?Q(compile)]],
               [[?Q(md5)],
                [call,?Q(erlang),?Q(get_module_info),?Q(Mod),?Q(md5)]]
              ],
    %% The struct clause if relevant.
    StrCls = struct_info_clause(St0),
    %% The function body.
    InfoFun = ['match-lambda' | InfoCls ++ StrCls ],
    Funcs1 = orddict:store({'__info__',1}, {InfoFun,Line}, Funcs0),
    St1 = coll_mdef_exports([['__info__',1]], St0),
    St1#lfe_cg{funcs=Funcs1}.

info_functions(#lfe_cg{exports=Exps,funcs=Funcs}) ->
    if Exps =:= all ->
            orddict:fetch_keys(Funcs);
       true -> Exps
    end.

struct_info_clause(St) ->
    case St#lfe_cg.struct of
        undefined -> [];                        %No struct def, no clause
        {Fields,_Line} ->
            Fun = fun ([F|_]) -> #{field => F, required => false};
                      (F) -> #{field => F, required => false}
                  end,
            KeyVals = lists:map(Fun, Fields),
            [ [[?Q(struct)],[list | KeyVals]] ]
    end.

%% compile_attributes(State) -> MdefAST.
%%  Compile the module attributes, metas, records and structs

compile_attributes(St) ->
    Exp = comp_export(St),
    Imps = comp_imports(St),
    Onload = comp_onload(St),
    Attrs = comp_attributes(St),
    Metas = comp_metas(St),
    Mline = St#lfe_cg.mline,
    %% Collect all the attributes.
    AST = [make_attribute(file, {St#lfe_cg.file,Mline}, Mline),
           make_attribute(module, St#lfe_cg.module, Mline),
           Exp |
           Onload ++ Imps ++ Attrs ++ Metas],
    AST.

%% comp_export(State) -> Attribute.
%% comp_imports(State) -> [Attribute].
%% comp_on_load(State) -> Attribute.
%% comp_attributes(State) -> [Attribute].
%% comp_metas(State) -> [Attribute]
%%  Currently we don't add the import attributes.

comp_export(#lfe_cg{exports=Exps,funcs=Funcs,mline=Line}) ->
    Es = if Exps =:= all ->
                 orddict:fetch_keys(Funcs);
            true -> Exps                        %Already in right format
         end,
    make_attribute(export, Es, Line).

comp_imports(_St) -> [].

comp_onload(#lfe_cg{onload={Func,Ar},mline=Line}) ->
    [make_attribute(on_load, {Func,Ar}, Line)];
comp_onload(#lfe_cg{onload=[]}) -> [].

comp_attributes(#lfe_cg{attrs=Atts}) ->
    lists:map(fun comp_attribute/1, Atts).

comp_attribute({'export-type',Ts,Line}) ->
    Ets = lists:map(fun ([T,A]) -> {T,A} end, Ts),
    make_attribute(export_type, Ets, Line);
comp_attribute({Name,Val,Line}) ->
    make_attribute(Name, Val, Line).

comp_metas(#lfe_cg{metas=Metas,mline=Line}) ->
    %% io:format("lc ~p\n", [{cms,Metas}]),
    Ms = lists:flatmap(fun (M) -> comp_meta(M, Line) end, Metas),
    %% io:format("cm ~p\n", [Ms]),
    Ms.

comp_meta({type,Tdefs,_}, Line) ->
    lists:flatmap(fun (Tdef) -> comp_type_def(type, Tdef, Line) end, Tdefs);
comp_meta({opaque,Tdefs,_}, Line) ->
    lists:flatmap(fun (Tdef) -> comp_type_def(opaque, Tdef, Line) end, Tdefs);
comp_meta({spec,[Func,Specs],Line}, _Line) ->
    %% io:format("lc ~p\n", [{cm,Func,Specs}]),
    comp_function_specs(Func, Specs, Line);
comp_meta({record,Rdefs,_}, Line) ->
    Fun = fun (Rdef) -> comp_record_def(Rdef, Line) end,
    As = lists:flatmap(Fun, Rdefs),
    %% io:format("cmr ~p\n", [As]),
    As;
comp_meta(_Meta, _Line) -> [].

%% compile_functions(State) -> [AST].

compile_functions(#lfe_cg{funcs=Funcs0}=St) ->
    Fun = fun (F, B, Fs) -> Fs ++ compile_function(F, B, St) end,
    orddict:fold(Fun, [], Funcs0).

compile_function({Name,_Arity}, {Def,Line}, St) ->
    comp_function_def(Name, Def, Line,St).

%% comp_type_def(Attr, TypeDef, Line) -> [AST].
%% comp_type_def(Attr, Type, Def, Line) -> [AST].
%%  Compile a type definition to an attribute.

comp_type_def(Attr, [Type,Def], Line) ->
    comp_type_def(Attr, Type, Def, Line).

comp_type_def(Attr, [Type|Args], Def, Line) ->
    Tdef = {Type,
            lfe_types:to_type_def(Def, Line),
            lfe_types:to_type_defs(Args, Line)},
    [make_attribute(Attr, Tdef, Line)].

%% comp_function_specs(FuncSpecs, Line) -> [AST].
%% comp_function_specs(Func, Spec, Line) -> [AST].
%%  Compile a function specification to an attribute.

%% comp_function_specs([Func,Specs], Line) ->
%%     %% io:format("lc ~p\n", [{cfs1,Func,Specs}]),
%%     comp_function_specs(Func, Specs, Line).

comp_function_specs([Name,Ar], Specs, Line) ->
    %% io:format("lc ~p\n", [{cfs2,Name,Ar,Specs}]),
    Sdef = {{Name,Ar},lfe_types:to_func_spec_list(Specs, Line)},
    [make_attribute(spec, Sdef, Line)].

%% comp_function_def(Func, Def, Line, State) -> [AST].
%%  Lambda lift the function returning all the functions.

comp_function_def(Name, Def, Line, #lfe_cg{imports=Imps,aliases=Aliases}) ->
    %% This also returns the defined top function.
    Lfs = lfe_codelift:function(Name, Def, Line),
    lists:map(fun ({N,D,L}) ->
                      {'fun',_,{clauses,Clauses}} =
                          lfe_translate:to_expr(D, L, {Imps,Aliases}),
                      {function,L,N,func_arity(D),Clauses}
              end, Lfs).

%% comp_record_def(RecordDef, Line) -> [Attribute].
%% comp_record_def(Name, Fields, Line) -> [Attribute].
%%  Format depends on whether 18 and older or newer. Meta is not
%%  passed on.

comp_record_def([Name,Fields], Line) ->
    comp_record_def(Name, Fields, Line).

comp_record_def(Name, Fields, Line) ->
    %% io:format("crd ~p ~p\n", [Name,Fields]),
    Fdefs = [ comp_record_field(Fdef, Line) || Fdef <- Fields ],
    %% io:format("cra ~p\n", [make_record_attribute(Name, Fdefs, Line)]),
    [make_record_attribute(Name, Fdefs, Line)].

comp_record_field([F,D,T], Line) ->
    {typed_record_field,
     comp_untyped_field([F,D], Line),
     lfe_types:to_type_def(T, Line)};
comp_record_field(Fd, Line) ->
    comp_untyped_field(Fd, Line).

comp_untyped_field([F,?Q(undefined)], Line) ->
    %% No need for undefined default.
    {record_field,Line,{atom,Line,F}};
comp_untyped_field([F,D], Line) ->
    {record_field,Line,{atom,Line,F},lfe_translate:to_expr(D, Line)};
comp_untyped_field([F], Line) ->
    {record_field,Line,{atom,Line,F}};
comp_untyped_field(F, Line) ->
    {record_field,Line,{atom,Line,F}}.

-ifdef(NEW_REC_CORE).
make_record_attribute(Name, Fdefs, Line) ->
    make_attribute(record, {Name,Fdefs}, Line).
-else.
make_record_attribute(Name, Fdefs, Line) ->
    make_attribute(type, {{record,Name},Fdefs}, Line).
-endif.

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
%%  Fetch a value with a default if it doesn't exist.

%% safe_fetch(Key, D, Def) ->
%%     case orddict:find(Key, D) of
%%         {ok,Val} -> Val;
%%         error -> Def
%%     end.

%% add_error(Line, Error, State) -> State.

add_error(L, E, #lfe_cg{errors=Errs}=St) ->
    St#lfe_cg{errors=Errs ++ [{L,?MODULE,E}]}.
