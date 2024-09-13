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

%% File    : lfe_lib.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang library of miscellaneous functions.

-module(lfe_lib).

%% General library functions.
-export([is_symb/1,is_symb_list/1,is_posint_list/1,
         is_proper_list/1,is_doc_string/1]).

-export([proc_forms/3,proc_forms/4]).

%% Miscellaneous useful LFE functions.
-export([split_name/1]).

%% -compile([export_all]).

%% is_symb(Sexpr) -> bool().
%% is_symb_list(Sexprs) -> bool().
%% is_proper_list(Sexprs) -> bool().
%% is_doc_string(Doc) -> bool().

is_symb(S) -> is_atom(S).

is_symb_list([S|Ss]) when is_atom(S) ->
    is_symb_list(Ss);
is_symb_list([]) -> true;
is_symb_list(_) -> false.                       %Might not be a proper list

is_posint_list([I|Is]) when is_integer(I), I >= 0 ->
    is_posint_list(Is);
is_posint_list([]) -> true;
is_posint_list(_) -> false.

is_proper_list([_|Ss]) -> is_proper_list(Ss);
is_proper_list([]) -> true;
is_proper_list(_) -> false.

is_doc_string(Doc) ->
    is_binary(Doc) or io_lib:char_list(Doc).

%% proc_forms(FormFun, Forms, State) -> {Forms,State}.
%% proc_forms(FormFun, Forms, Line, State) -> {Forms,State}.
%%  Process a (progn ... ) nested list of forms where top level list
%%  has elements {Form,LineNumber}. Return a flat list of results and
%%  passes through State. All the elements are processed left to
%%  right. The accumulator is in reverse order!

proc_forms(Fun, Fs, St) -> proc_top_forms(Fun, Fs, [], St).

proc_forms(Fun, Fs, L, St0) ->
    {Rs,St1} = proc_progn_forms(Fun, Fs, L, [], St0),
    {lists:reverse(Rs),St1}.

proc_top_forms(Fun, [{['progn'|Bs],L}|Fs], Rs0, St0) ->
    {Rs1,St1} = proc_progn_forms(Fun, Bs, L, Rs0, St0),
    proc_top_forms(Fun, Fs, Rs1, St1);
proc_top_forms(Fun, [{F,L}|Fs], Rs, St0) ->
    {Frs,St1} = Fun(F, L, St0),
    proc_top_forms(Fun, Fs, lists:reverse(Frs, Rs), St1);
proc_top_forms(_, [], Rs, St) -> {lists:reverse(Rs),St}.

proc_progn_forms(Fun, [['progn'|Bbs]|Bs], L, Rs0, St0) ->
    {Rs1,St1} = proc_progn_forms(Fun, Bbs, L, Rs0, St0),
    proc_progn_forms(Fun, Bs, L, Rs1, St1);
proc_progn_forms(Fun, [B|Bs], L, Rs, St0) ->
    {Frs,St1} = Fun(B, L, St0),
    proc_progn_forms(Fun, Bs, L, lists:reverse(Frs, Rs), St1);
proc_progn_forms(_, [], _, Rs, St) ->
    {Rs,St}.

%% proc_top_forms(Fun, [{['progn'|Bs],L}|Fs], Rs, St) ->
%%     proc_progn_forms(Fun, Bs, L, [], Fs, Rs, St);
%% proc_top_forms(Fun, [{F,L}|Fs], Rs, St0) ->
%%     {Frs,St1} = Fun(F, L, St0),
%%     proc_top_forms(Fun, Fs, lists:reverse(Frs, Rs), St1);
%% proc_top_forms(_, [], Rs, St) -> {lists:reverse(Rs),St}.

%% proc_progn_forms(Fun, [['progn'|Bs1]|Bs], L, Bss, Fs, Rs, St) ->
%%     proc_progn_forms(Fun, Bs1, L, [Bs|Bss], Fs, Rs, St);
%% proc_progn_forms(Fun, [B|Bs], L, Bss, Fs, Rs, St0) ->
%%     {Frs,St1} = Fun(B, L, St0),
%%     proc_progn_forms(Fun, Bs, L, Bss, Fs, lists:reverse(Frs, Rs), St1);
%% proc_progn_forms(Fun, [], L, [Bs|Bss], Fs, Rs, St) ->
%%     proc_progn_forms(Fun, Bs, L, Bss, Fs, Rs, St);
%% proc_progn_forms(Fun, [], _, [], Fs, Rs, St) ->
%%     proc_top_forms(Fun, Fs, Rs, St).

%% Miscellaneous useful LFE functions.

%% split_name(Name) -> [Mod] | [Mod,Func] | [Mod,Func,Arity].
%%  Split a name into its parts. Don't handle the case where there is
%%  no module.

split_name('=:=/2') -> ['=:=',2];
split_name(Name) ->
    Str = atom_to_list(Name),
    case string:chr(Str, $:) of
        0 -> [Name];                            %Only module
        C when C > 1 ->                         %Don't allow empty module name
            Mod = list_to_atom(string:substr(Str, 1, C-1)),
            Rest = string:substr(Str, C+1),
            case string:rchr(Rest, $/) of
                0 -> [Mod,list_to_atom(Rest)];  %Module and function
                S ->                            %Module, function and arity
                    [Mod,list_to_atom(string:substr(Rest, 1, S-1)),
                     list_to_integer(string:substr(Rest, S+1))]
            end
    end.
