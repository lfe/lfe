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

-import(lists, [map/2,foldr/3,concat/1]).

-include("lfe_macro.hrl").

%% Errors.
format_error(_) -> "record error".

%% define([Name|FieldDefs], Env, State) -> {Funs,Macs,Env,State}.
%% define(Name, FieldDefs, Env, State) -> {Funs,Macs,Env,State}.
%%  Define a VERY simple record by generating macros for all accesses.
%%  (define-record point x y)
%%    => make-point, is-point, match-point, set-point,
%%       point-x, set-point-x, point-y, set-point-y.

define([Name|Fdefs], Env, St0) ->
    {Funs,Forms, _,St1} = define(Name, Fdefs, Env, St0),
    {yes,[progn,['eval-when-compile'|Funs]|Forms],St1}.

define(Name, Fdefs, Env, St) ->
    %% Get field names, default values and indices.
    Fields = map(fun ([F,_]) when is_atom(F) -> F;
                     (F) when is_atom(F) -> F
                 end, Fdefs),
    Defs = map(fun ([F,D])when is_atom(F) -> ?Q(D);
                   (F) when is_atom(F) -> ?Q(?Q(undefined))
               end, Fdefs),
    Findexs = field_indexes(Fields),
    %% Make names for helper functions.
    Fi = list_to_atom(concat([Name,'-',field,'-',index])),
    Fu = list_to_atom(concat([Name,'-',field,'-',update])),
    %% Build helper functions.
    Funs = [index_function(Name, Fi, Findexs),
            update_function(Name, Fu, Fi)],
    %% Make access macros.
    Macs = [make_macro(Name, Defs, Fu),         %make-Name
            match_macro(Name, Fields, Fu),      %match-Name
            test_macro(Name, Fields),           %is-Name
            set_macro(Name, Fi),                %set-Name
            emp_macro(Name, Fields, Fu),        %emp-Name
            field_macro(Name, Fields)           %fields-Name
            |
            field_macros(Name, Fields)],        %Name-F,set-Name-F
    Type = type_information(Name, Fdefs, St),
    %% We can always add type information here as it is stripped later.
    Forms = [['extend-module',[],Type]|Macs],
    %% lfe_io:format("~p\n", [{Funs,Forms}]),
    {Funs,Forms,Env,St}.

field_indexes(Fs) -> field_indexes(Fs, 2).

field_indexes([F|Fs], N) ->
    [{F,N}|field_indexes(Fs, N+1)];
field_indexes([], _) -> [].

index_function(Name, Fi, Fxs) ->                %Get index of field
    [defun,Fi|
     map(fun ({F,I}) -> [[?Q(F)],I] end, Fxs) ++
         [[[f],[':',erlang,error,[tuple,?Q(undefined_field),?Q(Name),f]]]]].

update_function(Name, Fu, Fi) ->                %Update field list
    [defun,Fu,[is,def],
     %% Convert default list to tuple to make setting easier.
     [fletrec,[[l,
                [[[cons,f,[cons,v,is]],i],
                 [l,is,[setelement,['-',[Fi,f],1],i,v]]],
                [[[list,f],'_'],
                 [':',erlang,error,
                  [tuple,?Q(missing_value),?Q(Name),f]]],
                [[[],i],i]]],
      ['let',[[i,[l,is,[list_to_tuple,def]]]],
       [tuple_to_list,i]]]].

make_macro(Name, Defs, Fu) ->
    Make = list_to_atom(concat(['make','-',Name])),
    ['defmacro',Make,fds,
     ['let',[[def,[list|Defs]]],
      ?BQ([tuple,?Q(Name),?C_A([Fu,fds,def])])]].

match_macro(Name, Fs, Fu) ->
    Match = list_to_atom(concat(['match','-',Name])),
    ['defmacro',Match,fds,
     ['let',[[def,[list|lists:duplicate(length(Fs),?Q('_'))]]],
      ?BQ([tuple,?Q(Name),?C_A([Fu,fds,def])])]].

test_macro(Name, Fs) ->
    Test = list_to_atom(concat(['is','-',Name])),
    ['defmacro',Test,[rec],
     ?BQ(['is_record',?C(rec),?Q(Name),length(Fs)+1])].

set_macro(Name, Fi) ->
    Set = list_to_atom(concat(['set','-',Name])),
    ['defmacro',Set,
     [[cons,rec,fds],
      [fletrec,[[l,
                 [[[cons,f,[cons,v,is]],r],
                  %% Force evaluation left-to-right.
                  [l,is,[list,[quote,setelement],[Fi,f],r,v]]],
                 [[[list,f],'_'],
                  [':',erlang,error,
                   [tuple,?Q(missing_value),?Q(Name),f]]],
                 [[[],i],i]]],
       [l,fds,rec]]]].

emp_macro(Name, Fs, Fu) ->
    EMP = list_to_atom(concat(['emp','-',Name])),
    ['defmacro',EMP,fds,
     ['let',[[def,[list|lists:duplicate(length(Fs),?Q(?Q('_')))]]],
      ?BQ([tuple,?Q(Name),?C_A([Fu,fds,def])])]].

field_macro(Name, Fs) ->
    Recfields = list_to_atom(concat(['fields','-',Name])),
    ['defmacro',Recfields,[],?BQ(?Q(Fs))].

field_macros(Name, Fs) ->
    Fis = field_indexes(Fs),                    %Calculate indexes
    foldr(fun ({F,N}, Fas) ->
                  Get = list_to_atom(concat([Name,'-',F])),
                  Set = list_to_atom(concat(['set-',Name,'-',F])),
                  [[defmacro,Get,
                    [[],N],
                    [[list,rec],?BQ([element,N,?C(rec)])]],
                   [defmacro,Set,[rec,new],
                    ?BQ([setelement,N,?C(rec),?C(new)])]|
                   Fas]
          end, [], Fis).

type_information(Name, Fdefs, #mac{line=L}) ->
    %% Only field names which will result in default type any().
    %% Adding types greatly complicates things. If we add defaults
    %% then they would have to be expanded here.
    Fs = map(fun ([F,_D]) ->
                     %% De = lfe_trans:to_expr(D, L),
                     {record_field,L,{atom,L,F}};
                 (F) ->
                     {record_field,L,{atom,L,F}}
             end, Fdefs),
    [type,[{record,Name},Fs,[]]].
