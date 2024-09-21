%% Copyright (c) 2024 Robert Virding
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
%% Note that the we don't reorder the norms, normalised forms, so
%% their returnend order the same as the order of the input forms.
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
%% [moduledoc,Line,Docs]  ??
%% [compile,Line,Options]
%% [vsn,Line,Vsn]
%% [on_load,Line,Function]
%% [nifs,Line,Nifs]
%%
%% The other predefined norms can come anywhere.
%%
%% ['export-type',Line,Types]
%% ['module-alias',Line,Aliases]
%% [struct,Line,Fields]
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
%%
%% [attribute,Line,Name,Value]          General attribute norm

-module(lfe_normalise).

-export([module/1,module/2,forms/1,forms/2,format_error/1]).
%% -compile(export_all).

-include("lfe.hrl").
-include("lfe_comp.hrl").

-record(lfe_norm, {module=[],                   %Module name
		   errors=[],			%Errors
		   warnings=[]			%Warnings
		  }).

%% Errors.
format_error(bad_attribute) ->
    <<"bad attribute">>;
format_error({bad_attribute,A}) ->
    lfe_io:format1(<<"bad ~w attribute">>, [A]);
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
    {[['module',Line,Name]],St#lfe_norm{module=Name}};
%% Export and import are handled in the attributes.
form({['define-type',Type,Def],Line},St) ->
    {[['type',Line,Type,Def]],St};
form({['define-opaque-type',Type,Def],Line}, St) ->
    {[['opaque',Line,Type,Def]],St};
form({['export-type'|Types],Line}, St) ->
    {[['export-type',Line,Types]],St};
form({['module-alias'|Aliases],Line}, St) ->
    {[['module-alias',Line,Aliases]],St};
form({['define-record',Name,Fields],Line}, St) ->
    {[['record',Line,Name,Fields]],St};
form({['define-struct',Fields],Line}, St) ->
    {[['struct',Line,Fields]],St};
form({['define-macro',Name,Metas,Def],Line}, St0) ->
    MacroDef = ['macro',Line,Name,Def],
    {MetaDefs,St1} = macro_metas(Name, Line, Metas, St0),
    {MetaDefs ++ [MacroDef],St1};
%% form({['define-macro',Name,Metas,Def],Line}, St0) ->
%%     MacroDef = ['macro',Line,Name,Def],
%%     {MetaDefs,St1} = function_metas(Name, Line, Metas, St0),
%%     {MetaDefs ++ [MacroDef],St1};
%% form({['define-macro',_Name,_Metas,_Def],_Line}, St) ->
%%     {[],St};
form({['define-function-spec',Func,Specs],Line}, St) ->
    {[['spec',Line,Func,Specs]],St};
form({['define-function',Name,Metas,Def],Line}, St0) ->
    FuncDef = ['function',Line,Name,Def],
    {MetaDefs,St1} = function_metas(Name, Line, Metas, St0),
    {MetaDefs ++ [FuncDef],St1};
form({['eval-when-compile'|_]=Form,_Line}, St) ->
    %% Just pass this on without as is.
    {[Form],St};
%% What more should we especially handle here?
%% The default attribute case which will also catch unknown illegal
%% forms.
form({Form,Line}, St) ->
    attribute(Form, Line, St).

%% module_attributes(Metas, Attrs, State) -> {[Norm],State}.
%%  Metas and attributes are now one and the same. We specially handle
%%  the 'doc' case and change it to 'moduledoc' otherwise just process
%%  it as an attribute.

module_attributes(Metas, Attrs, Line, St) ->
    Attr = fun (A, {Fs0,S0}) ->
		   {Fs,S1} = module_attribute(A, Line, S0),
		   {Fs0 ++ Fs,S1}
	   end,
    lists:foldl(Attr, {[],St}, Metas ++ Attrs).

%% module_attribute([doc,Docs], Line, St0) ->
%%     St1 = add_warning(Line, {deprecated,<<"module attribute doc">>}, St0),
%%     {[[doc,Line,Docs]],St1};
module_attribute([type|TypeDefs], Line, St) ->
    module_type(type, TypeDefs, Line, St);
module_attribute([opaque|TypeDefs], Line, St) ->
    module_type(opaque, TypeDefs, Line, St);
module_attribute([spec|SpecDefs], Line, St) ->
    module_spec(SpecDefs, Line, St);
module_attribute(Attr, Line, St) ->
    attribute(Attr, Line, St).

module_type(Attr, TypeDefs, Line, St) ->
    TypeFunc = fun (TypeDef, {As0,S0}) ->
		       {As,S1} = attribute_type(Attr, TypeDef, Line, S0),
		       {As0 ++ As,S1}
	       end,
    lists:foldl(TypeFunc, {[],St}, TypeDefs).

module_spec(SpecDefs, Line, St) ->
    SpecFunc = fun (SpecDef, {As0,S0}) ->
		       {As,S1} = attribute_spec(SpecDef, Line, S0),
		       {As0 ++ As,S1}
	       end,
    lists:foldl(SpecFunc, {[],St}, SpecDefs).

%% attribute(Attribute, Line, State) -> {[Norm],State}.
%%  These "attributes" can both occur in the define/extend-module
%%  forms and are also non-specific top-level forms. We handle some
%%  specific cases and the accepted general attribute.

attribute([export|Exports], Line, St) ->
    attribute_export(Exports, Line, St);
attribute(['export-macro'|Exports], Line, St) ->
    attribute_export_macro(Exports, Line, St);
attribute([import|Imports], Line, St) ->
    {[[import,Line,Imports]],St};
attribute([moduledoc,Docs], Line, St) ->
    {[[moduledoc,Line,Docs]],St};
attribute([compile|Options], Line, St) ->
    {[[compile,Line,Options]],St};
attribute([on_load|Func], Line, St) ->
    {[[on_load,Line,Func]],St};
attribute([nifs|Nifs], Line, St) ->
    {[[nifs,Line,Nifs]],St};
attribute([type|TypeDef], Line, St) ->
    attribute_type('type', TypeDef, Line, St);
attribute([opaque|TypeDef], Line, St) ->
    attribute_type('opaque', TypeDef, Line, St);
attribute([record|RecDef], Line, St) ->
    {[['record',Line|RecDef]],St};
attribute([spec|Spec], Line, St) ->
    attribute_spec(Spec, Line, St);
attribute([doc,Docs], Line, St) ->
    {[[doc,Line,Docs]],St};
attribute([Name,Value], Line, St) when is_atom(Name) ->
    %% The general attribute.
    {[['attribute',Line,Name,Value]],St};
attribute(_Attr, Line, St) ->
    %% io:format("ba ~p\n", [_Attr]),
    %% Signal the error here as lint won't see it.
    {[],add_error(Line, bad_attribute, St)}.

%% attribute_export(Exports, Line, State) -> {[Export],State}.
%% attribute_export_macro(Exports, Line, State) -> {[Export],State}.
%%  Need to specially handle 'all'.

attribute_export([all], Line, St) ->
    {[[export,Line,all]],St};
attribute_export(Exports, Line, St) ->
    {[[export,Line,Exports]],St}.

attribute_export_macro([all], Line, St) ->
    {[['export-macro',Line,all]],St};
attribute_export_macro(Exports, Line, St) ->
    {[['export-macro',Line,Exports]],St}.

%% attribute_type(Attribute, TypeDef, Line, State) -> {[Norm],St}'
%%  Returns type norm where we have checked the formats and made sure
%%  there is enough data in the arguments.

attribute_type(Attr, [Type0|Def0], Line, St) ->
    Type1 = if is_list(Type0) -> Type0; true -> [Type0] end,
    Def1 = if Def0 =:= [] -> [any]; true -> hd(Def0) end,
    {[[Attr,Line,Type1,Def1]],St};
attribute_type(Attr, _TypeDef, Line, St) ->
    {[],add_error(Line, {bad_attribute,Attr}, St)}.

%% attriubute_spec(Spec, Line, State) -> {[Norm],St}.
%%  Return a spec norm. If the spec form does not include a functiona
%%  arity then we calculate one from the spec if we can. The linter
%%  will check this.

attribute_spec([[_Name,_Ar]=Func|Spec], Line, St) ->
    {[['spec',Line,Func,Spec]],St};
attribute_spec([Name|Spec], Line, St) ->
    Arity = spec_arity(Spec),
    {[['spec',Line,[Name,Arity],Spec]],St};
attribute_spec(Spec, Line, St) ->
    {[['spec',Line|Spec]],St}.

%% spec_arity(Spec) -> Arity.
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

%% macro_metas(Nae, Line, Metas, State) -> {[Form],State}.
%%  Only handle the leading doc meta, which is really all it can be.

macro_metas(_Name, Line, [[doc,_String]=Doc|_], St) ->
    attribute(Doc, Line, St);
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
    attribute_spec([Name|Specs], Line, St);
function_meta(_Name, Line, Meta, St) ->
    form({Meta,Line}, St).

%% add_error(Line, Error, State) -> State.

add_error(L, E, #lfe_norm{errors=Errs}=St) ->
    St#lfe_norm{errors=Errs ++ [{L,?MODULE,E}]}.

add_warning(L, W, #lfe_norm{warnings=Warns}=St) ->
    St#lfe_norm{warnings=Warns ++ [{L,?MODULE,W}]}.
