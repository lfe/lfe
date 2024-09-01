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

%%% Note that we are careful when generating the Erlang code. We don't
%%% try to be too helpful here. We separate the code into the
%%% predefined Erlang attributes which must be first and we output
%%% them first. For the other forms we keep their order output them in
%%% that order. This will result in somethings coming in the way that
%%% Erlang expects like keeping 'doc' and 'spec' before the functions
%%% they refer to.
%%%
%%% However it also means that there can come attributes inbetween
%%% functions which Erlang does not allow. This is an issue which the
%%% users must be aware of.
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
                 modline=0,                     %Module definition line
                 exports=ordsets:new(),         %Exports
                 imports=orddict:new(),         %Imports
                 aliases=orddict:new(),         %Aliases
                 attributes=[],                 %Attributes
                 functions=[],                  %Functions
                 on_load=[],                    %On_Load
                 struct=undefined,              %Struct definition
                 opts=[],                       %Compiler options
                 file=[],                       %File name
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
    %% io:format("ast ~p\n", [AST]),
    %% io:format("st ~p\n", [St1]),
    return_status(AST, St1).

return_status(AST, #lfe_cg{module=M,errors=[]}=St) ->
    {ok,M,AST,St#lfe_cg.warnings};
return_status(_AST, St) ->
    {error,St#lfe_cg.errors,St#lfe_cg.warnings}.

compile_module(Mfs, St0) ->
    {PreDef,Rest,St1} = compile_forms(Mfs, St0),
    {StrFuncs,St2} = build_struct_def(St1),
    {InfoFuncs,St3} = build_info_function(St2), %Must be last!
    Exp = compile_export(St3),                  %Make export
    %% io:format("ex ~p\n", [{St3#lfe_cg.exports,Exp}]),
    %% Add the leading file and module attributes.
    Modline = St3#lfe_cg.modline,
    {[make_attribute(file, {St3#lfe_cg.file,Modline},Modline),
      make_attribute(module, St3#lfe_cg.module, Modline),
      Exp |
      PreDef] ++ Rest ++ StrFuncs ++ InfoFuncs,St3}.

%% compile_forms(Forms, State) -> {Predefs,Rest,State}.
%% compile_form(Forms, State) -> {Predefs,Rest,State}.
%%  Compile the forms into Erlang separating the PreDefs, those which
%%  must be first in the module definition, and the others which can
%%  be anywhere. Apart from this separation we preserve the order of
%%  the forms.

%% compile_forms(Forms, St0) ->
%%     Compile = fun (F, {Fs0,Rs0,S0}) ->
%%                       {Fs,Rs,S1} = compile_form(F, S0),
%%                       {Fs0 ++ Fs,Rs0 ++ Rs,S1}
%%               end,
%%     {PreDef,Rest,St1} = lists:foldl(Compile, {[],[],St0}, Forms),
%%     {PreDef,Rest,St1}.

compile_forms(Forms, St) ->
    compile_forms(Forms, [], [], St).

compile_forms([Form|Forms], PreDef, Rest, St0) ->
    {Ps,Rs,St1} = compile_form(Form, Forms, St0),
    compile_forms(Forms, PreDef ++ Ps, Rest ++ Rs, St1);
compile_forms([], PreDef, Rest, St) ->
    {PreDef,Rest,St}.

%% The predefs which must come first in the module. The module,
%% exports and imports will be added later when we have enough
%% information.
compile_form(['module',Line,Name], _Forms, St0) ->
    St1 = St0#lfe_cg{module=Name,modline=Line}, %Save the module name and line.
    {[],[],St1};
compile_form(['export',Line,Exports], _Forms, St) ->
    {[],[],collect_exports(Exports, Line, St)};
compile_form(['import',Line,Imports], _Forms, St) ->
    {[],[],collect_imports(Imports, Line, St)};
compile_form(['moduledoc',Line,Doc], _Forms, St) ->
    {[make_attribute(moduledoc, Doc, Line)],[],St};
compile_form(['vsn',Line,Vsn], _Forms, St) ->
    {[make_attribute(vsn, Vsn, Line)],[],St};
compile_form(['on_load',Line,[[Func,Ar]]], _Forms, St) ->
    {[make_attribute(on_load, {Func,Ar}, Line)],[],St};
compile_form(['nifs',Line,Nifs], _Forms, St) ->
    Ns = lists:map(fun ([Func,Ar]) -> {Func,Ar} end, Nifs),
    {[make_attribute(nifs, Ns, Line)],[],St};
%% Normal attributes and functions.
compile_form(['export-macro',Line,Exports], _Forms, St) ->
    {[],[make_attribute('export-macro', Exports, Line)],St};
compile_form(['type',Line,Type,Def], _Forms, St) ->
    {[],comp_type_def('type', Type, Def, Line),St};
compile_form(['opaque',Line,Type,Def], _Forms, St) ->
    {[],comp_type_def('opaque', Type, Def, Line),St};
compile_form(['module-alias',Line,Aliases], _Forms, St) ->
    {[],[],collect_aliases(Aliases, Line, St)};
compile_form(['record',Line,Name,Fields], _Forms, St) ->
    {[],comp_record_def(Name, Fields, Line), St};
compile_form(['struct',Line,Fields], _Forms, St) ->
    {[],[],St#lfe_cg{struct={Fields,Line}}};
compile_form(['spec',Line,Func,Specs], _Forms, St) ->
    {[],comp_function_specs(Func, Specs, Line),St};
compile_form(['function',Line,Name,Def], _Forms, #lfe_cg{functions=Funcs}=St) ->
    Fs = comp_function_def(Name, Def, Line, St),
    Arity = func_arity(Def),
    {[],Fs,St#lfe_cg{functions=[{Name,Arity}|Funcs]}};
compile_form(['doc',_Line,_Docs], [['macro'|_]|_], St) ->
    %% Assume doc refers to macro so drop it completely.
    {[],[],St};
compile_form(['doc',Line,Docs], _Forms, St) ->
    make_doc_attribute(Docs, Line, St);
%% The general attribute.
compile_form(['attribute',Line,Name,Value], _Forms, St) ->
    {[],[make_attribute(Name, Value, Line)],St};
%% Ignore everything else here.
compile_form(_Other, _Forms, St) ->
    {[],[],St}.

%% collect_exports(Exports, Line, State) -> State.
%%  We have to save all the export info and build the info later. We
%%  cannot just use export_all as this will export all the lambda
%%  lifted fuctions.

collect_exports(Exports, _Line, St) ->
    collect_exports(Exports, St).

%% collect_exports(Export, State) -> State.
%%  Collect exports special casing 'all'.

collect_exports(all, St) -> St#lfe_cg{exports=all};
collect_exports(_Exps, #lfe_cg{exports=all}=St) -> St;
collect_exports(Exps, #lfe_cg{exports=Exps0}=St) ->
    Exps1 = lists:foldl(fun ([F,A], E) -> ordsets:add_element({F,A}, E) end,
                        Exps0, Exps),
    St#lfe_cg{exports=Exps1}.

%% collect_imports(Imports, Line, State) -> State.
%%  Collect imports keeping track of local and imported names.

collect_imports(Imps, _Line, St) ->
    lists:foldl(fun (I, S) -> collect_import(I, S) end, St, Imps).

collect_import(['from',Mod|Fs], St) ->
    From = fun ([F,A], Ifs) -> orddict:store({F,A}, {Mod,F}, Ifs) end,
    collect_import(From, St, Fs);
collect_import(['rename',Mod|Fs], St) ->
    %% Get it right here, R is the renamed local called function, F is
    %% the name in the other module.
    Rename = fun ([[F,A],R], Ifs) -> orddict:store({R,A}, {Mod,F}, Ifs) end,
    collect_import(Rename, St, Fs).

collect_import(Fun, #lfe_cg{imports=Imps0}=St, Fs) ->
    Imps1 = lists:foldl(Fun, Imps0, Fs),
    St#lfe_cg{imports=Imps1}.

%% comp_type_def(Attr, Type, Def, Line) -> [AST].
%%  Compile a type definition to an attribute.

comp_type_def(Attr, [Type|Args], Def, Line) ->
    Tdef = {Type,
            lfe_types:to_type_def(Def, Line),
            lfe_types:to_type_defs(Args, Line)},
    [make_attribute(Attr, Tdef, Line)].

%% collect_alias(Aliases, Line, State) -> State.
%%  Collect the module aliases.

collect_aliases(As, _L, #lfe_cg{aliases=Als0}=St) ->
    Als1 = lists:foldl(fun ([M,A], Mas) -> orddict:store(A, M, Mas) end,
                       Als0, As),
    St#lfe_cg{aliases=Als1}.

%% comp_record(Name, Fields, Line) -> [AST].

%% comp_record_def(Name, Fields, Line) -> [Attribute].
%%  Format depends on whether 18 and older or newer. Meta is not
%%  passed on.

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

%% comp_function_specs(Func, Spec, Line) -> [AST].
%%  Compile a function specification to an attribute.

comp_function_specs([Name,Ar], Specs, Line) ->
    %% io:format("lc ~p\n", [{cfs2,Name,Ar,Specs}]),
    Sdef = {{Name,Ar},lfe_types:to_func_spec_list(Specs, Line)},
    [make_attribute(spec, Sdef, Line)].


%% comp_function_def(Name, Def, Line, State) -> [AST].
%%  Lambda lift the function returning all the functions.

comp_function_def(Name, Def, Line,
		  #lfe_cg{imports=Imps,aliases=Aliases}) ->
    %% This also returns the defined top function.
    Lfs = lfe_codelift:function(Name, Def, Line),
    lists:map(fun ({N,D,L}) ->
		      {'fun',_,{clauses,Clauses}} =
			  lfe_translate:to_expr(D, L, {Imps,Aliases}),
		      {function,L,N,func_arity(D),Clauses}
	      end, Lfs).

%% compile_export(St) -> Attribute,

compile_export(#lfe_cg{modline=Line}=St) ->
    Es = export_functions(St),
    make_attribute(export, Es, Line).

export_functions(#lfe_cg{exports=all,functions=Funcs}) ->
    Funcs;
export_functions(#lfe_cg{exports=Exps}) ->
    Exps.                                       %Already in right format

%% make_doc_attribute(Docs, Line, State) -> {PreDef,Rest,State}.
%%  Where we add the doc depends on whether we are running OTP 27 and
%%  later or not as this affects how the doc is interpreted.

-ifdef(OTP27_DOCS).
make_doc_attribute(Docs, Line, St) ->
    {[],[make_attribute(doc, Docs, Line)],St}.
-else.
make_doc_attribute(Docs, Line, St) ->
    {[make_attribute(doc, Docs, Line)],[],St}.
-endif.

%% build_struct_def(State) -> State.
%% build_info_func(State) -> State.
%%  Create functions which are built from the data collected from the
%%  file. Here the __info__ function and the struct definition
%%  functions.

build_struct_def(#lfe_cg{struct=undefined}=St) ->
    %% No struct has been defined.
    {[],St};
build_struct_def(#lfe_cg{module=Mod,struct={Fields,Line},functions=Funcs0}=St0) ->
    %% The default struct.
    DefStr = comp_struct_map(Mod, Fields),
    %% The default __struct__/0/1 functions.
    StrFun_0 = struct_fun_0(DefStr),
    StrFun_1 = struct_fun_1(DefStr),
    StrFs0 = comp_function_def('__struct__', StrFun_0, Line, St0),
    StrFs1 = comp_function_def('__struct__', StrFun_1, Line, St0),
    St1 = collect_exports([['__struct__',0],['__struct__',1]], St0),
    Funcs1 = [{'__struct__',0},{'__struct__',1}|Funcs0],
    {StrFs0 ++ StrFs1,St1#lfe_cg{functions=Funcs1}}.

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

build_info_function(#lfe_cg{module=Mod,modline=Line,functions=Funcs}=St0) ->
    %% The default clauses.
    InfoCls = [[[?Q(module)],?Q(Mod)],
               [[?Q(functions)],?Q(export_functions(St0))],
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
    InfoFuncs = comp_function_def('__info__', InfoFun, Line, St0),
    St1 = collect_exports([['__info__',1]], St0),
    {InfoFuncs,St1#lfe_cg{functions=[{'__info__',1}|Funcs]}}.

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

%% add_error(L, E, #lfe_cg{errors=Errs}=St) ->
%%     St#lfe_cg{errors=Errs ++ [{L,?MODULE,E}]}.
