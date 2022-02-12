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

%%% File    : lfe_macro_record.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang macro expander for records.

%%% Create macros for defining, creating and accessing records. Note
%%% we still create the older set-Name macros even though they have
%%% been deprecated.

-module(lfe_macro_record).

-export([define/3,format_error/1]).

-import(lists, [map/2,foldr/3,concat/1]).

-include("lfe.hrl").
-include("lfe_macro.hrl").

%% Errors.
format_error({bad_record,Name}) ->
    lfe_io:format1(<<"bad definition of record ~w">>,[Name]);
format_error(_) -> "record error".

%% define([Name|FieldDefs], Env, State) -> {ok,Form,State}.
%% define(Name, FieldDefs, Env, State) -> {Forms,Env,State}.
%%  Define a VERY simple record by generating macros for all accesses.
%%  (define-record point x y)
%%    => make-point, is-point, match-point, set-point,
%%       point-x, set-point-x, point-y, set-point-y.

define([Name|Fdefs], Env, St0) ->
    {Macs,St1} = define(Name, Fdefs, Env, St0),
    {yes,[progn,['define-record',Name,Fdefs]|Macs],St1};
define([], _Env, _St) -> no.                    %Undefined macro

define(Name, Fdefs, _Env, St) when is_atom(Name) ->
    %% Get field names, default values and indices.
    Fields = map(fun ([F,_,_]) when is_atom(F) -> F;
                     ([F,_]) when is_atom(F) -> F;
                     ([F]) when is_atom(F) -> F;
                     (F) when is_atom(F) -> F;
                     (_) -> bad_record_error(Name)
                 end, Fdefs),
    %% Make access macros.
    Macs = [make_macro(Name),                   %make-Name
            match_macro(Name),                  %match-Name
            test_macro(Name, Fields),           %is-Name
            update_macro(Name),                 %update-Name
            set_macro(Name),                    %set-Name
            field_macro(Name, Fields),          %fields-Name
            size_macro(Name, Fields)            %size-Name
            |
            field_macros(Name, Fields)],        %Name-F,set-Name-F
    {Macs,St};
define(Name, _Fdefs, _Env, _St) ->
    bad_record_error(Name).

make_macro(Name) ->
    Make = list_to_atom(concat(['make','-',Name])),
    ['defmacro',Make,fds,?BQ(['record',Name,?C_A(fds)])].

match_macro(Name) ->
    Match = list_to_atom(concat(['match','-',Name])),
    ['defmacro',Match,fds,?BQ(['record',Name,?C_A(fds)])].

test_macro(Name, _Fs) ->
    Test = list_to_atom(concat(['is','-',Name])),
    ['defmacro',Test,[rec],?BQ(['is-record',?C(rec),Name])].
     %% ?BQ(['is_record',?C(rec),?Q(Name),length(Fs)+1])].

update_macro(Name) ->
    Upd = list_to_atom(concat(['update','-',Name])),
    [defmacro,Upd,
     [[cons,rec,fds],
      ?BQ(['record-update',?C(rec),Name,?C_A(fds)])]].

set_macro(Name) ->
    Set = list_to_atom(concat(['set','-',Name])),
    [defmacro,Set,
     [[cons,rec,fds],
      ?BQ(['record-update',?C(rec),Name,?C_A(fds)])]].

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
                  Upd = list_to_atom(concat(['update-',Name,'-',F])),
                  [[defmacro,Get,
                    [[],?Q(['record-index',Name,F])],
                    [[list,rec],
                     ?BQ(['record-field',?C(rec),Name,F])]],
                   [defmacro,Upd,[rec,new],
                    ?BQ(['record-update',?C(rec),Name,F,?C(new)])],
                   [defmacro,Set,[rec,new],
                    ?BQ(['record-update',?C(rec),Name,F,?C(new)])] |
                   Fas]
          end,
    lists:foldr(Fun, [], Fs).

bad_record_error(Name) -> error({bad_record,Name}).
