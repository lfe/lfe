%% Copyright (c) 2026 Robert Virding
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

%% File    : lfe_normalise.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang normalise input code.

%% Here we normalise the code and convert it to an internal form for
%% the compiler. This makes the main part of the LFE compiler less
%% sensitive to changes in the parsed and expanded forms.
%%
%% Note that WE DON'T REORDER THE NORMS, normalised forms, so they are
%% returned in the same order as the input forms.
%%
%% Note while many of the internals forms resemble the standard forms
%% they are not the same so we avoid any chance of mix-up.
%%
%% The normalised forms, Norms. The ones we show here are all
%% "special" and known and have their own form, but those that are
%% passed on to Erlang will become attributes in Erlang.
%%
%% The predefined attributes which must come before functions:
%%
%% [module,Line,Name]
%% [export,Line,all | Exports]
%% [import,Line,Imports]
%% [rename,Line,Renames]
%% [moduledoc,Line,Docs]
%% [compile,Line,Options]
%% [vsn,Line,Vsn]
%% [on_load,Line,Function]
%% [nifs,Line,Nifs]
%% ['alias',Line,Module,Alias]
%%
%% The other predefined norms can come anywhere.
%%
%% ['export-type',Line,Types]
%% [macro,Line,Name,Definition]
%% [function,Line,Name,Definition]
%% ['eval-when-compile',Body]
%% ['export-macro',Line,all | Exports]
%%
%% And the attributes, predefined and general
%%
%% [doc,Line,Docs]                      Special handling in 27+
%% [type,Line,Type,Def]
%% [opaque,Line,Type,Def]
%% [spec,Line,Function,Specs]
%% [record,Line,Name,Fields]
%% [struct,Line,Fields]
%%
%% [attribute,Line,Name,Value]          General attribute norm
%% ['-',Line,Name,Value]                General attribute norm

-module(lfe_normalise).

-export([module/1,module/2,forms/1,forms/2,format_error/1]).
%% -compile(export_all).

-include("lfe.hrl").
-include("lfe_comp.hrl").

-record(lfe_norm, {module=[],                   %Module name
                   errors=[],                   %Errors
                   warnings=[]                  %Warnings
                  }).

%% Errors.
format_error(illegal_form) ->
    <<"illegal form">>;
format_error({illegal_form,Form}) ->
    lfe_io:format1(<<"illegal ~w form">>, [Form]);
format_error(bad_attribute) ->
    <<"bad attribute">>;
format_error({bad_attribute,Attr}) ->
    lfe_io:format1(<<"bad ~w attribute">>, [Attr]);
format_error(undefined_name) ->
    <<"undefined module name">>;
format_error({deprecated,What}) ->
    lfe_io:format1("~s is deprecated", [What]).

%% module(ModuleForms) ->
%%     {ok,ModuleName,Forms,[Warning]} | {error,[Error],[Warning]}.
%% module(ModuleForms, CompInfo) ->
%%     {ok,ModuleName,Forms,[Warning]} | {error,[Error],[Warning]}.
%%  Normalise the forms in one module file.

module(Forms) ->
    module(Forms, #cinfo{file=nofile,opts=[]}).

module(Forms, _Cinfo) ->
    St0 = #lfe_norm{},
    {Norms,St1} = forms(Forms, St0),
    %% io:format("norms ~p\n", [Norms]),
    return_status(Norms, St1).

return_status(Forms, #lfe_norm{module=[]}=St0) ->
    St1 = add_error(1, undefined_name, St0),
    return_status(Forms, St1);
return_status(Forms, #lfe_norm{module=M,errors=[]}=St) ->
    {ok,M,Forms,St#lfe_norm.warnings};
return_status(_AST, St) ->
    {error,St#lfe_norm.errors,St#lfe_norm.warnings}.

%% forms(Forms) -> {[Norm],State}.
%% forms(Forms, State) -> {[Norm],State}.

forms(Forms) ->
    forms(Forms, #lfe_norm{}).

forms(Forms, St0) ->
    %% io:format("fs ~p\n", [Forms]),
    Norm = fun (F, {Fs0,S0}) ->
                   {Fs,S1} = form(F, S0),
                   {Fs0 ++ Fs,S1}
           end,
    lists:foldl(Norm, {[],St0}, Forms).

%% form({Form,Line}, State) -> {[Norm],State}.
%%  Process the special forms we recognise, everything else is either
%%  an attribute of type [Name|Value] or an unrecognised form which
%%  just pass on to lint. For macros and functions we place their
%%  metas before them in the returned norms.

form({['define-module',Name,Metas,Attrs],Line}, St0) ->
    ModDef = ['module',Line,Name],
    {AttrDefs,St1} = module_attributes(Metas, Attrs, Line,
                                       St0#lfe_norm{module=Name}),
    {[ModDef] ++ AttrDefs,St1};
form({['extend-module',Metas,Attrs],Line}, St0) ->
    {AttrDefs,St1} = module_attributes(Metas, Attrs, Line, St0),
    {AttrDefs,St1};
%% We even allow an explicit module form here.
form({['module',Name],Line}, St) ->
    ModDef = ['module',Line,Name],
    %% We add the default MODULE macro as well, (macro MODULE Name).
    ModMac = ['macro',Line,'MODULE',
              ['match-lambda',[[[list],'$ENV'],?BQ(?Q(Name))]]],
    {[ModDef,ModMac],St#lfe_norm{module=Name}};
%% Export and import are handled in the attributes.
form({['define-type',Type,Def],Line},St) ->
    {[['type',Line,Type,Def]],St};
form({['define-opaque-type',Type,Def],Line}, St) ->
    {[['opaque',Line,Type,Def]],St};
form({['define-function-spec',Func,Specs],Line}, St) ->
    {[['spec',Line,Func,Specs]],St};
form({['export-type'|Types],Line}, St) ->
    {[['export-type',Line,Types]],St};
form({['module-alias'|Aliases],Line}, St) ->
    %% Should we really support this at the top-level?
    module_alias(Aliases, Line, St);
form({['define-record',Name,Fields],Line}, St) ->
    {[['record',Line,Name,Fields]],St};
form({['define-struct',Fields],Line}, St) ->
    {[['struct',Line,Fields]],St};
form({['define-macro',Name,Metas,Def],Line}, St0) ->
    MacroDef = ['macro',Line,Name,Def],
    {MetaDefs,St1} = macro_metas(Name, Line, Metas, St0),
    {MetaDefs ++ [MacroDef],St1};
form({['define-function',Name,Metas,Def],Line}, St0) ->
    FuncDef = ['function',Line,Name,Def],
    {MetaDefs,St1} = function_metas(Name, Line, Metas, St0),
    {MetaDefs ++ [FuncDef],St1};
form({['eval-when-compile'|_]=Form,_Line}, St) ->
    %% Just pass this on as is without modifying it.
    {[Form],St};
%% Handle the 'function' and 'macro' forms.
form({['function',Name,Def],Line}, St) ->
    {[['function',Line,Name,Def]],St};
form({['macro',Name,Def],Line}, St) ->
    {[['macro',Line,Name,Def]],St};
%% Special handling of the 'attribute' and '-' attribute forms.
form({['attribute'|Attribute],Line}, St) ->
    attribute_attribute(Attribute, Line, St);
form({['-'|Args],Line}, St) ->
    attribute_attribute(Args, Line, St);
%% The default attribute case which will also catch unknown illegal
%% forms.
form({Form,Line}, St) ->
    form_attribute(Form, Line, St).

%% attribute_attribute(Attribute, Line, State) ->
%%     {[Norm],State}.
%%  We just require that it is one of the legal attributes or an
%%  (attribute name value).

attribute_attribute(Attr, Line, St) ->
    Unrecog = fun ([Name,Value], L, S) when is_atom(Name) ->
                      {[['attribute',L,Name,Value]],S};
                  (A, L, S) ->
                      {[],add_error(L, {bad_attribute,A}, S)}
              end,
    attribute(Attr, Line, Unrecog, St).

%% form_attribute(Attribute, Line, State) ->
%%     {[Norm],State}.
%%  Handle unrecognised forms, either the legal attribute forms or bad
%%  formats. We only accept the legal attributes.

form_attribute(Form, Line, St) ->
    Unrecog = fun
                  %% (['attribute',Name,Value], L, S) when is_atom(Name) ->
                  %%     {[['attribute',L,Name,Value]],S};
                  %% (['-',Name,Value], L, S) when is_atom(Name) ->
                  %%     {[['attribute',L,Name,Value]],S};
                  %% ([Name,Value], L, S) when is_atom(Name) ->
                  %%     {[['attribute',L,Name,Value]],S};
                  ([F|_], L, S) ->
                      {[],add_error(L, {illegal_form,F}, S)};
                  (_F, L, S) ->
                      {[],add_error(L, illegal_form, S)}
              end,
    attribute(Form, Line, Unrecog, St).

%% module_attributes(Metas, Attrs, State) -> {[Norm],State}.
%%  These are the standard define/extend-module metas and attributes
%%  which are now one and the same. We specially handle the 'doc' case
%%  and change it to 'moduledoc' otherwise just process it as an
%%  attribute.

module_attributes(Metas, Attrs, Line, St) ->
    Attr = fun (A, {Fs0,S0}) ->
                   {Fs,S1} = module_attribute(A, Line, S0),
                   {Fs0 ++ Fs,S1}
           end,
    lists:foldl(Attr, {[],St}, Metas ++ Attrs).

%% module_attribute([doc,Docs], Line, St0) ->
%%     St1 = add_warning(Line, {deprecated,<<"module attribute doc">>}, St0),
%%     {[[doc,Line,Docs]],St1};
module_attribute([export|Exports], Line, St) ->
    module_export(Exports, Line, St);
module_attribute([import|Imports], Line, St) ->
    module_import(Imports, Line, St);
module_attribute([type|TypeDefs], Line, St) ->
    module_type(type, TypeDefs, Line, St);
module_attribute([opaque|TypeDefs], Line, St) ->
    module_type(opaque, TypeDefs, Line, St);
module_attribute([nifs|Nifs], Line, St) ->
    {[[nifs,Line,Nifs]],St};
module_attribute([spec|SpecDefs], Line, St) ->
    module_spec(SpecDefs, Line, St);
module_attribute(['module-alias'|Aliases], Line, St) ->
    module_alias(Aliases, Line, St);
module_attribute(['export-macro'|Exports], Line, St) ->
    attribute_export_macro(Exports, Line, St);
module_attribute(Attr, Line, St) ->
    %% Handle unrecognised module attributes, either the legal short
    %% form or bad formats.
    Unrecog = fun ([Name,Value], L, S) when is_atom(Name) ->
                      {[['attribute',L,Name,Value]],S};
                  (A, L, S) ->
                      {[],add_error(L, {bad_attribute,A}, S)}
              end,
    attribute(Attr, Line, Unrecog, St).

module_export([all], Line, St) ->
    {[['export',Line,all]],St};
module_export(Exports, Line, St) ->
    {[['export',Line,Exports]],St}.

module_import(Imports, Line, St) ->
    ImpFunc = fun ([from,Mod|Imps], {As,S}) ->
                      {As ++ [['import',Line,Mod,Imps]],S};
                  ([rename,Mod|Rens], {As,S}) ->
                      {As ++ [['rename',Line,Mod,Rens]],S}
              end,
    lists:foldl(ImpFunc, {[],St}, Imports).

module_type(Attr, TypeDefs, Line, St) ->
    TypeFunc = fun ([Type,Def], {As0,S0}) ->
                       %%e io:format("mtype ~p ~p\n", [Type,Def]),
                       {As,S1} = attribute_type(Attr, Type, Def, Line, S0),
                       {As0 ++ As,S1}
               end,
    lists:foldl(TypeFunc, {[],St}, TypeDefs).

module_spec(SpecDefs, Line, St) ->
    SpecFunc = fun ([Spec,Def], {As0,S0}) ->
                       %%e io:format("mspec ~p\n", [[Spec,Def]]),
                       {As,S1} = attribute_spec(Spec, Def, Line, S0),
                       {As0 ++ As,S1}
               end,
    lists:foldl(SpecFunc, {[],St}, SpecDefs).

module_alias(Aliases, Line, St) ->
    AliasFun = fun ([Mod,Alias], {As,S}) ->
                       {As ++ [['alias',Line,Mod,Alias]],S}
               end,
    lists:foldl(AliasFun, {[],St}, Aliases).

%% attribute(Attribute, Line, Unrecognised, State) -> {[Norm],State}.
%%  These "attributes" can both occur in the define/extend-module
%%  forms and are also non-specific top-level forms. We handle known
%%  specific cases and have been passed what to do with an
%%  unrecognised attribute.

%% The standard Erlang attributes.
attribute([module,Name], Line, _Unrecog, St) ->
    {[[module,Line,Name]],St};
attribute([export,Exports], Line, _Unrecog, St) ->
    attribute_export(Exports, Line, St);
attribute([import,Module,Imports], Line, _Unrecog, St) ->
    {[[import,Line,Module,Imports]],St};
attribute([rename,Module,Renames], Line, _Unrecog, St) ->
    {[[rename,Line,Module,Renames]],St};
attribute([moduledoc,Docs], Line, _Unrecog, St) ->
    {[[moduledoc,Line,Docs]],St};
attribute([compile,Options], Line, _Unrecog, St) ->
    {[[compile,Line,Options]],St};
attribute([vsn,Vsn], Line, _Unrecog, St) ->
    {[[vsn,Line,Vsn]],St};
attribute([on_load,Funcs], Line, _Unrecog, St) ->
    {[[on_load,Line,Funcs]],St};
attribute([nifs,Nifs], Line, _Unrecog, St) ->
    {[[nifs,Line,Nifs]],St};
attribute([type,Type,Def], Line, _Unrecog, St) ->
    attribute_type('type', Type, Def, Line, St);
attribute([opaque,Type,Def], Line, _Unrecog, St) ->
    attribute_type('opaque', Type, Def, Line, St);
attribute([spec,Func,Spec], Line, _Unrecog, St) ->
    attribute_spec(Func, Spec, Line, St);
attribute([record,Name,Fields], Line, _Unrecog, St) ->
    {[['record',Line,Name,Fields]],St};
attribute([struct,Fields], Line,_Unrecog, St) ->
    {[['struct',Line,Fields]],St};
attribute([doc,Docs], Line, _Unrecog, St) ->
    {[[doc,Line,Docs]],St};
attribute([file,FileName,FileLine], Line, _Unrecog, St) ->
    {[['attribute',Line,file,{FileName,FileLine}]],St};
%% We accept both spellings of behaviour/behavior.
attribute(['behaviour',Behaviour], Line, _Unrecog, St) ->
    {[['behaviour',Line,Behaviour]],St};
attribute(['behavior',Behaviour], Line, _Unrecog, St) ->
    {[['behaviour',Line,Behaviour]],St};
attribute(['feature',Name,EnaDis], Line, _Unrecog, St) ->
    {[['feature',Line,Name,EnaDis]],St};
%% The standard LFE attributes.
attribute(['export-macro',Exports], Line, _Unrecog, St) ->
    attribute_export_macro(Exports, Line, St);
attribute(['alias',Module,Alias], Line, _Unrecog, St) ->
    {[['alias',Line,Module,Alias]],St};
%% Everything else is unrecognised.
attribute(Attr, Line, Unrecog, St) ->
    Unrecog(Attr, Line, St).

%% attribute_export(Exports, Line, State) -> {[Export],State}.
%% attribute_export_macro(Exports, Line, State) -> {[Export],State}.
%%  Need to specially handle 'all'.

attribute_export(all, Line, St) ->
    {[[export,Line,all]],St};
attribute_export(Exports, Line, St) ->
    {[[export,Line,Exports]],St}.

attribute_export_macro(all, Line, St) ->
    {[['export-macro',Line,all]],St};
attribute_export_macro(Exports, Line, St) ->
    {[['export-macro',Line,Exports]],St}.

%% attribute_type(Attribute, Type, Def, Line, State) -> {[Norm],St}'
%%  Returns type norm where we have checked the formats and made sure
%%  there is enough data in the arguments.

attribute_type(Attr, Type0, Def0, Line, St) ->
    Type1 = if is_list(Type0) -> Type0; true -> [Type0] end,
    Def1 = if Def0 =:= [] -> [any]; true -> Def0 end,
    {[[Attr,Line,Type1,Def1]],St}.
%% attribute_type(Attr, _Type, _Def, Line, St) ->
%%     {[],add_error(Line, {bad_attribute,Attr}, St)}.

%% attribute_spec(Func, Spec, Line, State) -> {[Norm],St}.
%%  Return a spec norm. If the spec form does not include a functiona
%%  arity then we calculate one from the spec if we can. The linter
%%  will check this.

attribute_spec([_Name,_Ar]=Func, Specs, Line, St) ->
    {[['spec',Line,Func,Specs]],St};
attribute_spec(Name, Specs, Line, St) ->
    Arity = spec_arity(Specs),
    {[['spec',Line,[Name,Arity],Specs]],St}.
%% attribute_spec(Specs, Line, St) ->
%%     {[['spec',Line|Specs]],St}.

%% spec_arity(Specs) -> Arity.
%%  Just return the length of the first arg list and let lint check
%%  properly later.

spec_arity([#{'arg-types' := Args}|_]) ->
    case lfe_lib:is_proper_list(Args) of
        true -> length(Args);
        false -> 0
    end;
spec_arity([[Args|_]|_]) ->
    case lfe_lib:is_proper_list(Args) of
        true -> length(Args);
        false -> 0
    end;
spec_arity(_) -> 0.

%% macro_metas(Name, Line, Metas, State) -> {[Form],State}.
%%  Only handle the leading doc meta, which is really all it can be.

macro_metas(_Name, Line, [[doc,_String]=Doc|_], St) ->
    form_attribute(Doc, Line, St);
macro_metas(_Name, _Line, _Metas, St) ->
    {[],St}.

%% function_metas(FuncName, Line, Metas, State) -> {[Form],State}.
%% function_meta(FuncName, Line, Meta, State) -> {[Form],State}.
%%  Go through the function metas. We only specially handle spec here,
%%  the rest as "normal" forms.

function_metas(Name, Line, Metas, St) ->
    MetaFunc = fun (Meta, {Fs0,S0}) ->
                       {Fs,S1} = function_meta(Name, Line, Meta, S0),
                       {Fs0 ++ Fs,S1}
               end,
    lists:foldl(MetaFunc, {[],St}, Metas).
    
function_meta(Name, Line, [spec|Specs], St) ->
    %% form({['define-function-spec',[Name,Arity],Specs],Line}, St);
    attribute_spec(Name, Specs, Line, St);
function_meta(_Name, Line, Meta, St) ->
    form({Meta,Line}, St).

%% add_error(Line, Error, State) -> State.

add_error(L, E, #lfe_norm{errors=Errs}=St) ->
    St#lfe_norm{errors=Errs ++ [{L,?MODULE,E}]}.

%% add_warning(L, W, #lfe_norm{warnings=Warns}=St) ->
%%     St#lfe_norm{warnings=Warns ++ [{L,?MODULE,W}]}.
