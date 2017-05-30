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

%% File    : lfe_lib.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang library of miscellaneous functions.

-module(lfe_lib).

%% General library functions.
-export([is_symb/1,is_symb_list/1,is_proper_list/1,is_doc_string/1]).

-export([proc_forms/3,proc_forms/4]).

%% Miscellaneous useful LFE functions.
-export([split_name/1]).

-export([format_exception/6,format_stacktrace/3]).

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

%% format_exception(Class, Error, Stacktrace, SkipFun, FormatFun, Indentation)
%%      -> DeepCharList.
%%  Format an exception. Class, Error and Stacktrace describe the
%%  exception; SkipFun is used to trim the end of stack; FormatFun is
%%  used to format terms; and Indentation is the current column.

format_exception(Cl, Error0, St0, Skip, Format, I) ->
    Cs = case Cl of                             %Class type as string
             throw -> "throw";
             exit -> "exit";
             error -> "error"
         end,
    {Error1,St1} = case is_stacktrace(St0) of
                       true -> {Error0,St0};
                       false -> {{Error0,St0},[]}
                   end,
    P = "exception " ++ Cs ++ ": ",             %Class description string
    [P,format_reason(Error1, length(P)+I-1),"\n",
     format_stacktrace(St1, Skip, Format)].

%% format_reason(Error, Indentation) -> DeepCharList.
%%  Format an error giving a little better information.

format_reason(badarg, _I) -> <<"bad argument">>;
format_reason(badarith, _I) -> <<"error in arithmetic expression">>;
format_reason({badmatch,V}, I) ->
    lfe_io:format1(<<"no match of value ~.*P">>, [I+18,V,10]);
format_reason(function_clause, _I) -> <<"no function clause matching">>;
format_reason({case_clause,V}, I) ->
    lfe_io:format1(<<"no case clause matching ~.*P">>, [I+24,V,10]);
format_reason(if_clause, _I) -> <<"no if clause matching">>;
format_reason(undef, _I) -> <<"undefined function">>;
%% Some LFE eval specific errors.
format_reason({unbound_symb,S}, _I) ->
    lfe_io:format1(<<"symbol ~w is unbound">>, [S]);
format_reason(illegal_guard, _I) -> <<"illegal guard">>;
format_reason({undefined_func,{F,A}}, _I) ->
    lfe_io:format1(<<"undefined function ~w/~w">>, [F,A]);
format_reason(if_expression, _I) -> <<"non-boolean if test">>;
format_reason({illegal_pattern,Pat}, _I) ->
    lfe_io:format1(<<"illegal pattern ~w">>, [Pat]);
format_reason({illegal_literal,Lit}, I) ->
    lfe_io:format1(<<"illegal literal value ~.*P">>, [I+22,Lit,10]);
format_reason(bad_arity, _I) -> <<"arity mismatch">>;
%% Default catch-all
format_reason(Error, I) ->			%Default catch-all
    lfe_io:prettyprint1(Error, 10, I).

%% format_stacktrace(Stacktrace, SkipFun, FormatFun) -> DeepCharList.
%%  Format a stacktrace. SkipFun is used to trim the end of stack;
%%  FormatFun is used to format terms.

format_stacktrace(St0, Skip, Format) ->
    St1 = lists:reverse(lists:dropwhile(Skip, lists:reverse(St0))),
    Print = fun (F) -> format_stackcall(F, Format) end,
    lists:map(Print, St1).

format_stackcall({M,F,A}, _) when is_integer(A) ->    %Pre R15
    lfe_io:format1("  in ~w:~w/~w\n", [M,F,A]);
format_stackcall({M,F,A}, Format) ->
    ["  in ",Format([M,':',F|A], 5),"\n"];
format_stackcall({M,F,A,Loc},_) when is_integer(A) -> %R15 and later.
    lfe_io:format1("  in ~w:~w/~w ~s\n", [M,F,A,location(Loc)]);
format_stackcall({M,F,A,_}, Format) ->
    ["  in ",Format([M,':',F|A], 5),"\n"].

location(Loc) ->
    File = proplists:get_value(file, Loc),
    Line = proplists:get_value(line, Loc),
    if File =/= undefined, Line =/= undefined ->
            lfe_io:format1("(~s, line ~w)", [File,Line]);
       true -> ""
    end.

is_stacktrace([{M,F,A}|Fs])                     %Pre R15
  when is_atom(M), is_atom(F), is_integer(A) -> is_stacktrace(Fs);
is_stacktrace([{M,F,As}|Fs])
  when is_atom(M), is_atom(F), length(As) >= 0 -> is_stacktrace(Fs);
is_stacktrace([{M,F,A,I}|Fs])                   %R15 and later
  when is_atom(M), is_atom(F), is_integer(A), is_list(I) -> is_stacktrace(Fs);
is_stacktrace([{M,F,As,I}|Fs])
  when is_atom(M), is_atom(F), length(As) >= 0, is_list(I) -> is_stacktrace(Fs);
is_stacktrace([]) -> true;
is_stacktrace(_) -> false.
