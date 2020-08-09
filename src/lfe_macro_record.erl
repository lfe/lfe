%% Copyright (c) 2008-2016 Robert Virding
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

%% File    : lfe_macro_record.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang macro expander for records.

-module(lfe_macro_record).

-export([define/3,format_error/1]).

-export([record_set_functions/4]).

-import(lists, [map/2,foldr/3,concat/1]).

-include("lfe_macro.hrl").

%% Errors.
format_error({badrecord,R}) ->
    lfe_io:format1("bad definition of record ~w",[R]);
format_error({undefined_record_field,R,F}) ->
    lfe_io:format1("undefined field ~w in record ~w",[F,R]);
format_error({missing_field_value,R,F}) ->
    lfe_io:format1("missing value to field ~w in record ~w",[F,R]);
format_error(_) -> "record error".

%% define([Name|FieldDefs], Env, State) -> {ok,Form,State}.
%% define(Name, FieldDefs, Env, State) -> {Forms,Env,State}.
%%  Define a VERY simple record by generating macros for all accesses.
%%  (define-record point x y)
%%    => make-point, is-point, match-point, set-point,
%%       point-x, set-point-x, point-y, set-point-y.

define([Name|Fdefs], Env, St0) ->
    {Macs,Type, _,St1} = define(Name, Fdefs, Env, St0),
    {yes,[progn,['extend-module',[Type],[]]|Macs],St1}.

define(Name, Fdefs, Env, St) ->
    %% Get field names, default values and indices.
    Fields = map(fun ([F,_,_]) when is_atom(F) -> F;
                     ([F,_]) when is_atom(F) -> F;
                     (F) when is_atom(F) -> F;
                     (_) -> error({badrecord,Name})
                 end, Fdefs),
    %% Make access macros.
    Macs = [make_macro(Name),                   %make-Name
            match_macro(Name),                  %match-Name
            test_macro(Name, Fields),           %is-Name
            set_macro(Name),                    %set-Name
            emp_macro(Name),                    %emp-Name
            field_macro(Name, Fields),          %fields-Name
            size_macro(Name, Fields)            %size-Name
            |
            field_macros(Name, Fields)],        %Name-F,set-Name-F
    Type = type_information(Name, Fdefs, St),
    %% lfe_io:format("~p\n", [{Macs,Type}]),
    {Macs,Type,Env,St}.

make_macro(Name) ->
    Make = list_to_atom(concat(['make','-',Name])),
    ['defmacro',Make,fds,
     ?BQ(['make-record',Name,?C_A(fds)])].

match_macro(Name) ->
    Match = list_to_atom(concat(['match','-',Name])),
    ['defmacro',Match,fds,
     ?BQ(['make-record',Name,?C_A(fds)])].

test_macro(Name, Fs) ->
    Test = list_to_atom(concat(['is','-',Name])),
    ['defmacro',Test,[rec],
     ?BQ(['is_record',?C(rec),?Q(Name),length(Fs)+1])].

set_macro(Name) ->
    Set = list_to_atom(concat(['set','-',Name])),
    [defmacro,Set,
     [[cons,rec,fds],
      ?BQ(['set-record',Name,?C(rec),?C_A(fds)])]].

emp_macro(Name) ->
    EMP = list_to_atom(concat(['emp','-',Name])),
    ['defmacro',EMP,fds,
     ?BQ(['make-record',Name,?C_A(fds) | ['_',?Q('_')]])].

field_macro(Name, Fs) ->
    Recfields = list_to_atom(concat(['fields','-',Name])),
    ['defmacro',Recfields,[],?BQ(?Q(Fs))].

size_macro(Name, Fs) ->
    Recsize = list_to_atom(concat(['size','-',Name])),
    ['defmacro',Recsize,[],length(Fs)+1].       %Don't forget the record name

field_macros(Name, Fs) ->
    Fun = fun (F, Fas) ->
                  Get = list_to_atom(concat([Name,'-',F])),
                  Set = list_to_atom(concat(['set-',Name,'-',F])),
                  [[defmacro,Get,
                    [[],?Q(['record-index',Name,F])],
                    [[list,rec],
                     ?BQ(['record-field',?C(rec),Name,F])]],
                   [defmacro,Set,[rec,new],
                    ?BQ(['set-record',?C(rec),Name,F,?C(new)])] |
                   Fas]
          end,
    lists:foldr(Fun, [], Fs).

type_information(Name, Fdefs, _St) ->
    %% We push the problem of generating the right final forms to the
    %% code generator which knows about the record attribute.
    [record,[Name|Fdefs]].

%% record_set_functions(FieldUpds, Name, IndexFun, RecordVar) ->
%%     {LetList,Body}.
%%  Define list of [V,Val] for let wrapper and the set body. RecordVar
%%  is the variable of value of the initial record.

record_set_functions(Fds, Name, Index, Rec) ->
    %% Must eval Lets first as it catches error.
    Lets = fun ([F,V|Ps], Fun) -> [[F,V]|Fun(Ps, Fun)];
               ([F], _) -> erlang:error({missing_field_value,Name,F});
               ([], _) -> []
           end,
    Body = fun ([F,_|Ps], I, B, Fun) ->
                   Fun(Ps, I, [setelement,I(F),B,F], Fun);
               ([], _, B, _) -> B
           end,
    {Lets(Fds, Lets),Body(Fds, Index, Rec, Body)}.
