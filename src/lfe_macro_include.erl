%% Copyright (c) 2013-2017 Robert Virding
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

%% File    : lfe_macro_include.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang macro expander for include macros.

%% Expand the (include-file ...) and (include-lib ...) macros handling
%% if they are LFE syntax files or erlang syntax files. Erlang syntax
%% files are ones which end in .hrl. We only handle basic record and
%% macro definitions.

-module(lfe_macro_include).

-export([file/3,lib/3,format_error/1,stringify/1]).

-export([read_hrl_file_1/1,read_hrl_file/2]).   %For testing

%%-compile([export_all]).

-include("lfe_macro.hrl").

%% Test function to inspect output of parsing functions.
read_hrl_file_1(Name) ->
    case epp:open(Name, []) of
        {ok,Epp} ->
            %% These are two undocumented functions of epp.
            Fs = epp:parse_file(Epp),
            Ms = epp:macro_defs(Epp),
            epp:close(Epp),                     %Now we close epp
            {ok,Fs,Ms};
        {error,E} -> {error,E}
    end.

%% Errors.
format_error({notrans_function,F,A}) ->
    io_lib:format("unable to translate function ~w/~w", [F,A]);
format_error({notrans_record,R}) ->
    io_lib:format("unable to translate record ~w", [R]);
format_error({notrans_type,T}) ->
    io_lib:format("unable to translate type ~w", [T]);
format_error({notrans_macro,M}) ->
    io_lib:format("unable to translate macro ~w", [M]).

%% add_warning(Warning, State) -> State.
%% add_warning(Line, Warning, State) -> State.

add_warning(W, St) -> add_warning(St#mac.line, W, St).

add_warning(L, W, St) ->
    St#mac{warnings=St#mac.warnings ++ [{L,?MODULE,W}]}.

%% file([FileName], Env, MacState) ->
%%     {yes,(progn ...),MacState} | {error,Error}.
%%  Expand the (include-file ...) macro.  This is a VERY simple
%%  include file macro! We just signal errors.

file(Body, _, #mac{ipath=Path}=St0) ->
    case include_name(Body) of
        {ok,Name} ->
            case path_read_file(Path, Name, St0) of
                {ok,Fs,St1} -> {yes,['progn'|Fs],St1};
                {error,E} -> error(E);
                not_found -> error(enoent)
            end;
        {error,E} -> error(E)
    end.

%% lib([FileName], Env, MacState) ->
%%     {yes,(progn ...),MacState} | {error,Error}.
%%  Expand the (include-lib ...) macro.  This is a VERY simple include
%%  lib macro! First try to include the file directly else assume
%%  first directory name is a library name. We just signal errors.

lib(Body, _, St0) ->
    case include_name(Body) of
        {ok,Name} ->
            case path_read_file(St0#mac.ipath, Name, St0) of
                {ok,Fs,St1} -> {yes,['progn'|Fs],St1};
                {error,E} -> error(E);          %Found contained error
                not_found ->                    %File not found
                    case lib_file_name(Name) of
                        {ok,Lfile} ->
                            case read_file(Lfile, St0) of
                                {ok,Fs,St1} -> {yes,['progn'|Fs],St1};
                                {error,E} -> error(E)
                            end;
                        {error,_} -> error(badarg)
                    end
            end;
        {error,E} -> error(E)
    end.

%% path_read_file(Path, Name, State) -> {ok,Forms,State} | {error,E} | error.
%%  Step down the path trying to read the file. We first test if we
%%  can open it, if so then this the file we use, if not we go on.

path_read_file([P|Ps], Name, St) ->
    File = filename:join(P, Name),
    case file:open(File, [read,raw]) of         %Test if we can open the file
        {ok,F} ->
            file:close(F),                      %Close it again
            read_file(File, St);
        {error,_} ->
            path_read_file(Ps, Name, St)
    end;
path_read_file([], _, _) ->                     %Couldn't find/open the file
    not_found.

%% include_name(Body) -> bool().
%%  Gets the file name from the include-XXX body.

include_name([Name]) ->
    case io_lib:char_list(Name) of
        true -> {ok,Name};
        false -> {error,badarg}
    end;
include_name(_) -> {error,badarg}.

%% lib_file_name(LibPath) -> {ok,LibFileName} | {error,Error}.
%%  Construct path to true library file.

lib_file_name(Lpath) ->
    [Lname|Rest] = filename:split(Lpath),
    case code:lib_dir(list_to_atom(Lname)) of
        Ldir when is_list(Ldir) ->
            {ok,filename:join([Ldir|Rest])};
        {error,E} -> {error,E}
    end.

%% read_file(FileName, State) -> {ok,Forms,State} | {error,Error}.

read_file(Name, St) ->
    case lists:suffix(".hrl", Name) of
        true -> read_hrl_file(Name, St);        %Read file as .hrl file
        false -> read_lfe_file(Name, St)
    end.

read_lfe_file(Name, St) ->
    %% Read the file as an LFE file.
    case lfe_io:read_file(Name) of
        {ok,Fs} -> {ok,Fs,St};
        {error,E} -> {error,E}
    end.

%% read_hrl_file(FileName, State) -> {ok,Forms,State} | {error,Error}.
%%  We use two undocumented functions of epp which allow us to get
%%  inside and get out the macros.

read_hrl_file(Name, St) ->
    case epp:open(Name, []) of
        {ok,Epp} ->
            %% These are two undocumented functions of epp.
            Fs = epp:parse_file(Epp),           %This must be called first
            Ms = epp:macro_defs(Epp),           % then this!
            epp:close(Epp),                     %Now we close epp
            parse_hrl_file(Fs, Ms, St);
        {error,E} -> {error,E}
    end.

%% parse_hrl_file(Forms, Macros, State) -> {ok,Forms,State} | {error,Error}.
%%  All the attributes go in an extend-module form. In 18 and older a
%%  typed record definition would result in 2 attributes, the bare
%%  record def and the record type def. We want just the record type
%%  def.

-ifdef(NEW_REC_CORE).
parse_hrl_file(Fs, Ms, St0) ->
    {As,Lfs,St1} = trans_forms(Fs, St0),
    %% io:format("~p\n",[Ms]),
    {Lms,St2} = trans_macros(Ms, St1),
    {ok,[['extend-module',[],As]] ++ Lfs ++ Lms,St2}.
-else.
parse_hrl_file(Fs0, Ms, St0) ->
    %% Trim away untyped record attribute when there is a typed record
    %% attribute as well.
    Trs = typed_record_attrs(Fs0),
    Fs1 = delete_typed_record_defs(Fs0, Trs),
    {As,Lfs,St1} = trans_forms(Fs1, St0),
    {Lms,St2} = trans_macros(Ms, St1),
    {ok,[['extend-module',[],As]] ++ Lfs ++ Lms,St2}.

typed_record_attrs(Fs) ->
    [ Name || {attribute,_,type,{{record,Name},_,_}} <- Fs ].

delete_typed_record_defs(Fs, Trs) ->
    Dfun = fun ({attribute,_,record,{Name,_}}) ->
                   not lists:member(Name, Trs);
               (_) -> true
           end,
    lists:filter(Dfun, Fs).
-endif.

%% trans_forms(Forms, State) -> {Attributes,LForms,State}.
%%  Translate the record and function defintions and attributes in the
%%  forms to LFE record and function definitions and
%%  attributes.

trans_forms(Fs, St0) ->
    Tfun = fun (F, {As,Lfs,St}) -> trans_form(F, As, Lfs, St) end,
    {As,Lfs,St1} = lists:foldl(Tfun, {[],[],St0}, Fs),
    {lists:reverse(As),lists:reverse(Lfs),St1}.

%% trans_form(Form, Attributes, LispForms, State) ->
%%     {Attributes,LispForms,State}.
%%  Note that the Attributes and LispForms are the ones that have
%%  preceded this form, but in reverse order.

trans_form({attribute,Line,record,{Name,Fields}}, As, Lfs, St) ->
    case catch {ok,trans_record(Name, Line, Fields)} of
        {ok,Lrec} -> {As,[Lrec|Lfs],St};
        {'EXIT',_} ->                           %Something went wrong
            {As,Lfs,add_warning({notrans_record,Name}, St)}
    end;
trans_form({attribute,Line,type,{Name,Def,E}}, As, Lfs, St) ->
    case catch {ok,trans_type(Name, Line, Def, E)} of
        {ok,Ltype} -> {As,[Ltype|Lfs],St};
        {'EXIT',_E} ->                          %Something went wrong
            exit({boom,_E,{Name,Line,Def,E}}),
            {As,Lfs,add_warning({notrans_type,Name}, St)}
    end;
trans_form({attribute,Line,opaque,{Name,Def,E}}, As, Lfs, St) ->
    case catch {ok,trans_opaque(Name, Line, Def, E)} of
        {ok,Ltype} -> {As,[Ltype|Lfs],St};
        {'EXIT',_E} ->                          %Something went wrong
            exit({boom,_E,{Name,Line,Def,E}}),
            {As,Lfs,add_warning({notrans_type,Name}, St)}
    end;
trans_form({attribute,Line,spec,{Func,Types}}, As, Lfs, St) ->
    case catch {ok,trans_spec(Func, Line, Types)} of
        {ok,Lspec} -> {As,[['define-function-spec'|Lspec]|Lfs],St};
        {'EXIT',_E} ->                          %Something went wrong
            exit({boom,_E,{Func,Line,Types}}),
            {As,Lfs,add_warning({notrans_spec,Func}, St)}
    end;
trans_form({attribute,_,export,Es}, As, Lfs, St) ->
    Les = trans_farity(Es),
    {[[export|Les]|As],Lfs,St};
trans_form({attribute,_,import,{Mod,Es}}, As, Lfs, St) ->
    Les = trans_farity(Es),
    {[[import,[from,Mod|Les]]|As],Lfs,St};
trans_form({attribute,_,Name,E}, As, Lfs, St) ->
    {[[Name,E]|As],Lfs,St};
trans_form({function,_,Name,Arity,Cls}, As, Lfs, St) ->
    case catch {ok,trans_function(Name, Arity, Cls)} of
        {ok,Lfunc} -> {As,[Lfunc|Lfs],St};
        {'EXIT',_} ->                           %Something went wrong
            {As,Lfs,add_warning({notrans_function,Name,Arity}, St)}
    end;
trans_form({error,E}, As, Lfs, #mac{errors=Es}=St) ->
    %% Assume the error is in the right format, {Line,Mod,Err}.
    {As,Lfs,St#mac{errors=Es ++ [E]}};
trans_form(_, As, Lfs, St) ->                   %Ignore everything else
    {As,Lfs,St}.

trans_farity(Es) ->
    lists:map(fun ({F,A}) -> [F,A] end, Es).

%% trans_record(Name, Line, Fields) -> LRecDef.
%%  Translate an Erlang record definition to LFE. We currently ignore
%%  any type information.

trans_record(Name, _, Fs) ->
    Lfs = record_fields(Fs),
    [defrecord,Name|Lfs].

record_fields(Fs) ->
    [ record_field(F) || F <- Fs ].

record_field({record_field,_,F}) ->             %Just the field name
    lfe_trans:from_lit(F);
record_field({record_field,_,F,Def}) ->         %Field name and default value
    Fd = lfe_trans:from_lit(F),
    Ld = lfe_trans:from_expr(Def),
    [Fd,Ld];
record_field({typed_record_field,Rf,Type}) ->
    typed_record_field(Rf, Type).

typed_record_field({record_field,_,F}, Type) ->
    %% Just the field name, set default value to 'undefined.
    Fd = lfe_trans:from_lit(F),
    Td = lfe_types:from_type_def(Type),
    [Fd,?Q(undefined),Td];
typed_record_field({record_field,_,F,Def}, Type) ->
    Fd = lfe_trans:from_lit(F),
    Ld = lfe_trans:from_expr(Def),
    Td = lfe_types:from_type_def(Type),
    [Fd,Ld,Td].

%% trans_type(Name, Line, Definition, Extra) -> TypeDef.
%%  Translate an Erlang type definition to LFE. In 18 and older this
%%  could also contain a typed record definition which we use.

-ifdef(NEW_REC_CORE).
trans_type(Name, Line, Def, E) ->
    ['define-type',[Name|lfe_types:from_type_defs(E)],
     lfe_types:from_type_def(Def)].
-else.
trans_type({record,Name}, Line, Def, _E) ->
    trans_record(Name, Line, Def);
trans_type(Name, _, Def, E) ->
    ['define-type',[Name|lfe_types:from_type_defs(E)],
     lfe_types:from_type_def(Def)].
-endif.

%% trans_opaque(Name, Line, Definition, Extra) -> TypeDef.
%%  Translate an Erlang opaque type definition to LFE.

trans_opaque(Name, _, Def, E) ->
    ['define-opaque-type',[Name|lfe_types:from_type_defs(E)],
     lfe_types:from_type_def(Def)].
    %%[type,{Name,convert_type(Def, Line),E}].

%% trans_spec(FuncArity, Line, TypeList) -> SpecDef.

trans_spec({Name,Arity}, _, Tl) ->
    [[Name,Arity],lfe_types:from_func_spec_list(Tl)].

%% trans_function(Name, Arity, Clauses) -> LfuncDef.

trans_function(Name, _, Cls) ->
    %% Make it a fun and then drop the match-lambda.
    ['match-lambda'|Lcs] = lfe_trans:from_expr({'fun',0,{clauses,Cls}}),
    [defun,Name|Lcs].

%% trans_macros(MacroDefs, State) -> {LMacroDefs,State}.
%%  Translate macro definitions to LFE macro definitions. Ignore
%%  undefined and predefined macros.

trans_macros([{{atom,Mac},Defs}|Ms], St0) ->
    {Lms,St1} = trans_macros(Ms, St0),
    case catch trans_macro(Mac, Defs, St1) of
        {'EXIT',E} ->                           %It crashed
            {Lms,add_warning({notrans_macro,Mac,E}, St1)};
        {none,St2} -> {Lms,St2};                %No definition, ignore
        {Mdef,St2} -> {[Mdef|Lms],St2}
    end;
trans_macros([], St) -> {[],St}.

trans_macro(_, undefined, St) -> {none,St};     %Undefined macros
trans_macro(_, {none,_}, St) -> {none,St};      %Predefined macros
trans_macro(Mac, Defs0, St) ->
    Defs1 = order_macro_defs(Defs0),
    case trans_macro_defs(Defs1) of
        [] -> {none,St};                        %No definitions
        Lcls -> {[defmacro,Mac|Lcls],St}
    end.

order_macro_defs([{none,Ds}|Defs]) ->           %Put the no arg version last
    Defs ++ [{none,Ds}];
order_macro_defs(Defs) -> Defs.

%% trans_macro_defs(MacroDef) -> [] | [Clause].
%%  Translate macro definition to a list of clauses. Put the no arg
%%  version last as a catch all. Clash if macro has no arg definition
%%  *and* function definition with no args:
%%  -define(foo, 42).
%%  -define(foo(), 17).
%%
%%  NOTE: Don't yet generate code to macros with *only* no arg case to
%%  be used as functions. So -define(foo, bar) won't work for foo(42).

trans_macro_defs([{none,{none,Ts}}|Defs]) ->
    Ld = trans_macro_body([], Ts),
    AnyArgs = ['_'|Ld],
    [AnyArgs|trans_macro_defs(Defs)];
trans_macro_defs([{N,{As,Ts}}|Defs]) when is_integer(N) ->
    Ld = trans_macro_body(As, Ts),
    ListArgs = [[list|As]|Ld],
    [ListArgs|trans_macro_defs(Defs)];
trans_macro_defs([]) -> [].

trans_macro_body([], Ts0) ->
    Ts1 = trans_qm(Ts0),
    %% io:format("parse: ~p\n",[Ts1 ++ [{dot,0}]]),
    {ok,[E]} = erl_parse:parse_exprs(Ts1 ++ [{dot,0}]),
    %% io:format("result: ~p\n",[E]),
    [?BQ(lfe_trans:from_expr(E))];
trans_macro_body(As, Ts0) ->
    Ts1 = trans_qm(Ts0),
    {ok,[E]} = erl_parse:parse_exprs(Ts1 ++ [{dot,0}]),
    Le0 = lfe_trans:from_expr(E),
    %% Wrap variables in arg list with an (comma ...) call.
    Alist = [ [A|[comma,A]] || A <- As ],
    Le1 = lfe:sublis(Alist, Le0),
    %% Le1 = unquote_vars(Alist, Le0),
    [?BQ(Le1)].

    %% {ok,[_]=F} = erl_parse:parse_exprs(Ts1 ++ [{dot,0}]),
    %% backquote_last(lfe_trans:from_body(F)).

%% unquote_vars(Alist, Expr) -> Expr.
%%  Special version of sublis which doesn't enter quotes. Specially
%%  made for traversing code and unquote-ing vars.

%% unquote_vars(_, ?Q(_)=E) -> E;
%% unquote_vars(Alist, E) ->
%%     case lfe:assoc(E, Alist) of
%%     [_|New] -> New;          %Found it
%%     [] ->                    %Not there
%%         case E of
%%         [H|T] ->
%%             [unquote_vars(Alist, H)|unquote_vars(Alist, T)];
%%         _ -> E
%%         end
%%     end.

%% Backquote the last expression in the body.
%% backquote_last([E]) -> [?BQ(E)];
%% backquote_last([E|Es]) -> [E|backquote_last(Es)].

%% trans_qm(Tokens) -> Tokens.
%%  Translate variable argument names to atoms to get correct
%%  translation to LFE later on: ?Sune -> ?'Sune' -> (Sune)

%% Translate ?FOO( ==> FOO(
trans_qm([{'?',_},{atom,_,_}=A,{'(',_}=Lp|Ts]) ->
    [A,Lp|trans_qm(Ts)];
trans_qm([{'?',_},{var,L,V},{'(',_}=Lp|Ts]) ->
    [{atom,L,V},Lp|trans_qm(Ts)];
%% Translate ?FOO:bar ==> (FOO()):bar.
trans_qm([{'?',L},{atom,_,_}=A,{':',_}=C|Ts]) ->
    Lp = {'(',L},
    Rp = {')',L},
    [Lp,A,Lp,Rp,Rp,C|trans_qm(Ts)];
trans_qm([{'?',L},{var,_,V},{':',_}=C|Ts]) ->
    Lp = {'(',L},
    Rp = {')',L},
    [Lp,{atom,L,V},Lp,Rp,Rp,C|trans_qm(Ts)];
%% Translate ?FOO ==> FOO().
trans_qm([{'?',L},{atom,_,_}=A|Ts]) ->
    [A,{'(',L},{')',L}|trans_qm(Ts)];
trans_qm([{'?',L},{var,_,V}|Ts]) ->
    [{atom,L,V},{'(',L},{')',L}|trans_qm(Ts)];
%% Translate ??FOO ==> lfe_macro_include:stringify(quote(FOO))
trans_qm([{'?',L},{'?',_},Arg|Ts]) ->
    Lp = {'(',L},
    Rp = {')',L},
    [{atom,L,?MODULE},{':',L},{atom,L,stringify},Lp,
     {atom,L,quote},Lp,Arg,Rp,Rp|
     trans_qm(Ts)];
trans_qm([T|Ts]) -> [T|trans_qm(Ts)];
trans_qm([]) -> [].

%%% stringify(Sexpr) -> String.
%%  Returns a list of sexpr, a string which when parse would return
%%  the sexpr.

stringify(E) -> lists:flatten(lfe_io:print1(E)).
