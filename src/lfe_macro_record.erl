%% Copyright (c) 2008-2013 Robert Virding
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

-export([defrecord/3,format_error/1]).

-import(lists, [map/2,foldr/3,concat/1]).

-include("lfe_macro.hrl").

%% Errors.
format_error(_) -> "record error".

%% defrecord([Name|FieldDefs], Env, State) -> {Funs,Macs,Env,State}.
%% defrecord(Name, FieldDefs, Env, State) -> {Funs,Macs,Env,State}.
%%  Define a VERY simple record by generating macros for all accesses.
%%  (define-record point x y)
%%    => make-point, is-point, match-point, set-point,
%%       point-x, set-point-x, point-y, set-point-y.

defrecord([Name|Fdefs], Env, St0) ->
    {Funs,Macs, _,St1} = defrecord(Name, Fdefs, Env, St0),
    {yes,[progn,['eval-when-compile'|Funs]|Macs],St1}.

defrecord(Name, Fdefs, Env, St0) ->
    %% Get field names, default values and indices.
    Fields = map(fun ([F,_])when is_atom(F) -> F;
		     (F) when is_atom(F) -> F
		 end, Fdefs),
    Defs = map(fun ([F,D])when is_atom(F) -> ?Q(D);
		   (F) when is_atom(F) -> ?Q(?Q(undefined))
	       end, Fdefs),
    Findex = defrec_indexes(Fields),
    %% Make names for helper functions.
    Fi = list_to_atom(concat([Name,'-',field,'-',index])),
    Fu = list_to_atom(concat([Name,'-',field,'-',update])),
    %% Build helper functions.
    Funs = [[defun,Fi|				%Get index of field
	     map(fun ({F,I}) -> [[?Q(F)],I] end, Findex) ++
	     [[[f],[':',erlang,error,[tuple,?Q(undefined_field),?Q(Name),f]]]]],
	    [defun,Fu,[is,def],			%Update field list
	     %% Convert default list to tuple to make setting easier.
	     [fletrec,[[l,
			[[[cons,f,[cons,v,is]],i],
			 [l,is,[setelement,['-',[Fi,f],1],i,v]]],
			[[[list,f],'_'],
			 [':',erlang,error,
			  [tuple,?Q(missing_value),?Q(Name),f]]],
			[[[],i],i]]],
	      ['let',[[i,[l,is,[list_to_tuple,def]]]],
	       [tuple_to_list,i]]]]
	   ],
    %% Make names for record creator/tester/matcher/setter.
    Make = list_to_atom(concat(['make','-',Name])),
    Match = list_to_atom(concat(['match','-',Name])),
    Test = list_to_atom(concat(['is','-',Name])),
    EMP = list_to_atom(concat(['emp','-',Name])),
    Set = list_to_atom(concat(['set','-',Name])),
    %% Make access macros.
    {Fdef,St1} = defrec_fields(Fields, Name, St0), %Name is element 1!
    Macs = [['defmacro',Make,fds,
	     ['let',[[def,[list|Defs]]],
	      ?BQ([tuple,?Q(Name),?UQ_S([Fu,fds,def])])]],
	    ['defmacro',Match,fds,
	     ['let',[[def,[list|lists:duplicate(length(Fields),?Q('_'))]]],
	      ?BQ([tuple,?Q(Name),?UQ_S([Fu,fds,def])])]],
	    ['defmacro',Test,[rec],
	     ?BQ(['is_record',?UQ(rec),?Q(Name),length(Fields)+1])],
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
	       [l,fds,rec]]]],
	    ['defmacro',EMP,fds,
	      ['let',[[def,[list|lists:duplicate(length(Fields),?Q(?Q('_')))]]],
	       ?BQ([tuple,?Q(Name),?UQ_S([Fu,fds,def])])]]
	    |
	    Fdef],
    %% lfe_io:format("~p\n", [{Funs,Macs}]),
    {Funs,Macs,Env,St1}.

defrec_indexes([F|Fs]) ->
    defrec_indexes([F|Fs], 2).			%First element is record name

defrec_indexes([F|Fs], N) ->
    [{F,N}|defrec_indexes(Fs, N+1)];
defrec_indexes([], _) -> [].

defrec_fields(Fs, Name, St) ->
    Fis = defrec_indexes(Fs),			%Calculate indexes
    {foldr(fun ({F,N}, Fas) ->
		   Get = list_to_atom(concat([Name,'-',F])),
		   Set = list_to_atom(concat(['set-',Name,'-',F])),
		   [[defmacro,Get,
		     [[],N],
		     [[list,rec],?BQ([element,N,?UQ(rec)])]],
		    [defmacro,Set,[rec,new],
		     ?BQ([setelement,N,?UQ(rec),?UQ(new)])]|
		    Fas]
	   end, [], Fis), St}.
