%% Copyright (c) 2016-2017 Eric Bailey
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

%% File    : lfe_doc.erl
%% Author  : Eric Bailey
%% Purpose : Lisp Flavoured Erlang (LFE) documentation parser.

%% There is no guarantee the internal formats will not change
%% but the interface functions should stay the same.

-module(lfe_doc).

-export([format_error/1]).

-export([make_doc_info/2,make_chunk/2]).
%%-export([extract_module_docs/1,extract_module_docs/2,save_module_docs/3]).

%% Access functions for documentation in modules.
-export([get_module_docs/1,module_doc/1,function_docs/1,macro_docs/1,
         function_docs/3,macro_docs/2,
         function_name/1,function_arity/1,function_line/1,
         function_patterns/1, function_doc/1,
         macro_name/1,macro_line/1,macro_patterns/1,macro_doc/1]).

-export([format_docs/1]).

-import(lists, [map/2,member/2,filter/2,foldl/3,foldr/3,reverse/1]).

-include("lfe_comp.hrl").
-include("lfe_doc.hrl").

-ifdef(EUNIT).
-export([collect_docs/2,pprint/2]).             %Used by prop_lfe_doc

-include_lib("eunit/include/eunit.hrl").

-define(QC_OPTS, [{on_output,fun pprint/2},{numtests,1000},{max_size,10}]).
-define(QC(T,P), {timeout,30,{T,?_assert(proper:quickcheck(P, ?QC_OPTS))}}).
-endif.

-record(st, {moddoc=[],                         %Module docs
             fs=[],                             %Function info
             ms=[],                             %Macro info
             expf=[],                           %Exported functions and macros
             expm=[],
             errors=[]
            }).

%% Errors
format_error(Error) -> io_lib:format("doc error: ~p", [Error]).

%% make_chunk(Defs, Opts) -> {ok,Chunk}.
%% make_doc_info(Defs, Opts) -> {ok,Chunk}.

-spec make_chunk(Defs, Opts) -> {ok,Chunk} when
      Defs :: [{Form,Line}],
      Form :: [_],
      Line :: non_neg_integer(),
      Opts :: [_],
      Chunk :: {string(),binary()}.

-spec make_doc_info(Defs, Opts) -> {ok,DocInfo} | {error,Errors,[]} when
      Defs :: [{Form,Line}],
      Form :: [_],
      Line :: non_neg_integer(),
      Opts :: [_],
      DocInfo :: lfe_docs_v1(),
      Errors :: nonempty_list({error,Line,any()}).

make_chunk(Defs, Opts) ->
    {ok,DocInfo} = make_doc_info(Defs, Opts),
    Chunk = {"LDoc",erlang:term_to_binary(DocInfo, [compressed])},
    {ok,Chunk}.

make_doc_info(Defs, Opts) ->
    St0 = do_forms(Defs, #st{}),
    St1 = trim_info(St0),
    St2 = format_info(St1),
    #st{moddoc=Moddoc,fs=Fdocs,ms=Mdocs} = St2,
    ?DEBUG("#doc: ~p\n", [{Moddoc,Fdocs,Mdocs}], Opts),
    ?IF(St2#st.errors =:= [],
        {ok,#lfe_docs_v1{moduledoc=Moddoc,fdocs=Fdocs,mdocs=Mdocs}},
        {error,St2#st.errors,[]}).

%% trim_info(State) -> State.
%%  Remove all non-exported functions and macros from the state.

trim_info(#st{fs=Fs0,ms=Ms0,expf=Expf,expm=Expm}=St) ->
    Fs1 = filter(fun (#fdoc{name=N,arity=Ar}) ->
                         is_exported(N, Ar, Expf)
                 end, Fs0),
    Ms1 = filter(fun (#mdoc{name=N}) ->
                         ordsets:is_element(N, Expm)
                 end, Ms0),
    St#st{fs=Fs1,ms=Ms1}.

is_exported(N, Ar, Expf) ->
    Expf =:= all orelse ordsets:is_element([N,Ar], Expf).

%% format_info(State) -> State.
%%  Format all the module, function and macro doc strings in the state.

format_info(#st{moddoc=Mdoc0,fs=Fs0,ms=Ms0}=St) ->
    Mdoc1 = format_docs(Mdoc0),
    Fs1 = map(fun (#fdoc{doc=Ds}=Fd) -> Fd#fdoc{doc=format_docs(Ds)} end, Fs0),
    Ms1 = map(fun (#mdoc{doc=Ds}=Md) -> Md#mdoc{doc=format_docs(Ds)} end, Ms0),
    St#st{moddoc=Mdoc1,fs=Fs1,ms=Ms1}.

%% do_forms(Forms, State) -> State.

do_forms(Forms, St) -> foldl(fun do_form/2, St, Forms).

do_form({['define-module',_,Meta,Atts],_}, St) ->
    define_module(Meta, Atts, St);
do_form({['extend-module',Meta,Atts],_}, St) ->
    define_module(Meta, Atts, St);
do_form({['define-function',Name,Meta,Def],Line}, St) ->
    define_function(Name, Meta, Def, Line, St);
do_form({['define-macro',Name,Meta,Def],Line}, St) ->
    define_macro(Name, Meta, Def, Line, St);
do_form(_, St) -> St.                         %Ignore other forms

define_module(Meta, Atts, #st{moddoc=Md0,expf=Expf0,expm=Expm0}=St) ->
    Md1 = collect_docs(Meta, Md0),
    Md2 = collect_docs(Atts, Md1),
    Expf1 = collect_fexports(Atts, Expf0),
    Expm1 = collect_mexports(Atts, Expm0),
    St#st{moddoc=Md2,expf=Expf1,expm=Expm1}.

collect_fexports(Atts, Expf) ->
    Fun = fun ([export|Es], Efs) -> collect_fexport(Es, Efs);
              (_, Efs) -> Efs
          end,
    foldl(Fun, Expf, Atts).

collect_fexport(_, all) -> all;
collect_fexport([all], _) -> all;
collect_fexport(Es, Efs) ->
    foldl(fun ordsets:add_element/2, Efs, Es).

collect_mexports(Atts, Expm) ->
    Fun = fun (['export-macro'|Es], Em) ->
                  foldl(fun ordsets:add_element/2, Em, Es);
              (_, Em) -> Em
          end,
    foldl(Fun, Expm, Atts).

define_function(Name, Meta, Def, Line, #st{fs=Fs}=St) ->
    {Arity,Pats} = get_function_patterns(Def),
    ?IF(exclude_function(Name, Arity, Meta),
        St,
        begin
            Ds = collect_docs(Meta, []),
            Fd = #fdoc{name=Name,arity=Arity,patterns=Pats,doc=Ds,line=Line},
            St#st{fs=[Fd|Fs]}
        end).

define_macro(Name, Meta, Def, Line, #st{ms=Ms}=St) ->
    ?IF(exclude_macro(Name, Meta),
        St,
        begin
            Ds = collect_docs(Meta, []),
            Pats = get_macro_patterns(Def),
            Md = #mdoc{name=Name,patterns=Pats,doc=Ds,line=Line},
            St#st{ms=[Md|Ms]}
        end).

%% func_arity(['lambda',As|_]) -> length(As);
%% func_arity(['match-lambda',[Pats|_]|_]) -> length(Pats).

collect_docs(As, Mdoc) ->
    %% Collect all the docs in all the doc metas/attributes.
    Afun = fun ([doc|Docs], Md) ->
                   Dfun = fun (D, M) -> M ++ [string_to_binary(D)] end,
                   foldl(Dfun, Md, Docs);
               (_, Md) -> Md
           end,
    foldl(Afun, Mdoc, As).

%% exclude_function(Name, Arity, Meta) -> boolean().
%% exclude_macro(Name, Meta) -> boolean().
%%  Return true if a function should be excluded from the docs chunk.

-spec exclude_function(Name, Arity, Meta) -> boolean() when
      Name  :: atom(),
      Arity :: non_neg_integer(),
      Meta  :: list().
-spec exclude_macro(Name, Meta) -> boolean() when
      Name  :: atom(),
      Meta  :: list().

exclude_function('LFE-EXPAND-EXPORTED-MACRO', 3, _)  -> true;
exclude_function(_, _, _) -> false.

exclude_macro('MODULE', _) -> true;
exclude_macro('FILE', _)   -> true;
exclude_macro('LINE', _)   -> true;
exclude_macro(_, _)        -> false.

%% get_function_patterns(LambdaForm) -> {Arity,Patterns}.
%% get_macro_patterns(LambdaForm)    -> Patterns.
%%  Given a {match-,}lambda form, attempt to return its patterns (or
%%  arglist).  N.B. A guard is appended to its pattern and Patterns is
%%  a list of lists.  A macro definition must have two args, the pattern
%%  and the environment.

-spec get_function_patterns(LambdaForm) -> {Arity,Patterns} when
      LambdaForm :: nonempty_list(),
      Arity      :: non_neg_integer(),
      Patterns   :: nonempty_list({pattern(),guard()}).
-spec get_macro_patterns(LambdaForm) -> Patterns when
      LambdaForm :: nonempty_list(),
      Patterns   :: nonempty_list({pattern(),guard()}).

get_function_patterns([lambda,Args|_]) -> {length(Args),[{Args,[]}]};
get_function_patterns(['match-lambda',[Pat|_]=Cl|Cls]) ->
    {length(Pat),do_function_patterns([Cl|Cls], [])}.

do_function_patterns([[Pat,['when'|Guard]|_]|Cls], Acc) ->
    do_function_patterns(Cls, [{Pat,Guard}|Acc]);
do_function_patterns([[Pat|_]|Cls], Acc) ->
    do_function_patterns(Cls, [{Pat,[]}|Acc]);
do_function_patterns([], Acc) -> reverse(Acc).

get_macro_patterns([lambda,[Args,_Env]|_]) -> [Args];
get_macro_patterns(['match-lambda'|Cls])   -> do_macro_patterns(Cls, []).

do_macro_patterns([[[Pat,_Env],['when'|Guard]|_]|Cls], Acc) ->
    do_macro_patterns(Cls, [{Pat,Guard}|Acc]);
do_macro_patterns([[[Pat,_Env]|_]|Cls], Acc) ->
    do_macro_patterns(Cls, [{Pat,[]}|Acc]);
do_macro_patterns([], Acc) -> reverse(Acc).

string_to_binary(Str) when is_list(Str) ->
    unicode:characters_to_binary(Str, utf8, utf8);
string_to_binary(Bin) -> Bin.

%% format_docs([DocString]) -> [DocLine].
%%  Take a list of doc strings and generate a list of indented doc
%%  lines. Each doc string is indented separately. Should it be so?

format_docs(Ds) -> lists:flatmap(fun format_doc/1, Ds).

format_doc(D) ->
    %% Split the string into separate lines, also trims trailing blanks.
    Ls = re:split(D, <<"[ \t]*\n">>, [trim]),
    format_doc_lines(Ls).

format_doc_lines([<<>>|Ls0]) ->                 %First line empty
    case skip_empty_lines(Ls0) of               %Skip lines until text
        {_,[L|_]=Ls1} ->
            C = count_spaces(L),                %Use indentation of this line
            format_doc_lines(Ls1, C);
        {_,[]} -> []
    end;
format_doc_lines([L1|Ls0]) ->                   %First line not empty
    case skip_empty_lines(Ls0) of
        {Els,[L|_]=Ls1} ->
            C = count_spaces(L),                %Use indentation of this line
            %% Include first line as is.
            [L1] ++ Els ++ format_doc_lines(Ls1, C);
        {Els,[]} -> [L1|Els]
    end;
format_doc_lines([]) -> [].

format_doc_lines(Ls, C) -> lists:map(fun (L) -> skip_spaces(L, C) end, Ls).

count_spaces(L) ->
    {match,[{_,C}]} = re:run(L, <<"^ *">>, []),
    C.

skip_spaces(<<$\s,L/binary>>, C) when C > 0 ->
    skip_spaces(L, C-1);
skip_spaces(L, _) -> L.                         %C =:= 0 or no space

skip_empty_lines(Ls) -> lists:splitwith(fun (L) -> L =:= <<>> end, Ls).

%% Access functions for the module doc chunk.

%% get_module_docs(Module | Binary) -> {ok,Chunk} | {error,What}.

get_module_docs(Mod) when is_atom(Mod) ->
    case code:get_object_code(Mod) of
        {Mod,Bin,_} ->
            get_module_chunk(Bin);
        error -> {error,module}                 %Could not find the module
    end;
get_module_docs(Bin) when is_binary(Bin) ->
    get_module_chunk(Bin).

get_module_chunk(Bin) ->
    case beam_lib:chunks(Bin, ["LDoc"], []) of
        {ok,{_,[{"LDoc",Chunk}]}} ->
            {ok,binary_to_term(Chunk)};
        _ -> {error,docs}                       %Could not find the docs chunk
    end.

%% module_doc(Chunk) -> [binary()].
%% mf_docs(Chunk) -> [MacFuncDoc].
%% mf_doc_type(MacFuncDoc) -> function | macro.
%% function_docs(Chunk) -> [FunctionDoc].
%% macro_docs(Chunk) -> [MacroDoc].

module_doc(#lfe_docs_v1{moduledoc=Moddoc}) -> Moddoc.

function_docs(#lfe_docs_v1{fdocs=Fdocs}) -> Fdocs.
macro_docs(#lfe_docs_v1{mdocs=Mdocs}) -> Mdocs.

function_docs(Name, Ar, #lfe_docs_v1{fdocs=Fdocs}) ->
    Fun = fun (#fdoc{name=N,arity=A}) -> (N =/= Name) or (A =/= Ar) end,
    case lists:dropwhile(Fun, Fdocs) of
        [Fd|_] -> {ok,Fd};
        [] -> error
    end.

macro_docs(Name, #lfe_docs_v1{mdocs=Mdocs}) ->
    case lists:dropwhile(fun (#mdoc{name=N}) -> N =/= Name end, Mdocs) of
        [Md|_] -> {ok,Md};
        [] -> error
    end.

%% function_name(FunctionDoc) -> Name.
%% function_arity(FunctionDoc) -> Arity.
%% function_line(FunctionDoc) -> LineNo.
%% function_patterns(FunctionDoc) -> [Pattern].
%% function_doc(FunctionDoc) -> [DocString].
%%  Extract fields from a function doc structure.

function_name(#fdoc{name=Name}) -> Name.
function_arity(#fdoc{arity=Ar}) -> Ar.
function_line(#fdoc{line=Line}) -> Line.
function_patterns(#fdoc{patterns=Ps}) -> Ps.
function_doc(#fdoc{doc=Ds}) -> Ds.

%% macro_name(MacroDoc) -> Name.
%% macro_line(MacroDoc) -> LineNo.
%% macro_patterns(MacroDoc) -> [Pattern].
%% macro_doc(MacroDoc) -> [DocString].
%%  Extract fields from a macr doc structure.

macro_name(#mdoc{name=Name}) -> Name.
macro_line(#mdoc{line=Line}) -> Line.
macro_patterns(#mdoc{name=N,patterns=Ps}) when is_atom(N) -> Ps.
macro_doc(#mdoc{name=N,doc=Ds}) when is_atom(N) -> Ds.

%%%===================================================================
%%% EUnit tests
%%%===================================================================

-ifdef(EUNIT).
parse_test_() ->
    [ ?QC(<<"A lambda definition is parsed correctly.">>,
          prop_lfe_doc:prop_define_lambda())
    , ?QC(<<"A match-lambda definition is parsed correctly.">>,
          prop_lfe_doc:prop_define_match())
    ].

pprint(_Format, [{Def,_Line}]) -> lfe_io:format(user, "~p\n", [Def]);
pprint(Format, Data) -> lfe_io:format(user, Format, Data).
-endif.
