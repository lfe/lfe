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

%% File    : lfe_lib.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang library of miscellaneous functions.

-module(lfe_lib).

%% General library functions.
-export([is_bif/2,is_erl_bif/2,is_guard_bif/2]).

-export([is_symb/1,is_symb_list/1,is_proper_list/1,is_core_form/1]).

-export([proc_forms/3]).

%% Standard lisp library.
-export([is_lfe_bif/2,
         acons/3,pairlis/2,pairlis/3,
         assoc/2,'assoc-if'/2,'assoc-if-not'/2,
         rassoc/2,'rassoc-if'/2,'rassoc-if-not'/2,
         subst/3,'subst-if'/3,'subst-if-not'/3,sublis/2,
         eval/1,eval/2,
         'macro-function'/1,'macro-function'/2,
         macroexpand/1,macroexpand/2,
         'macroexpand-1'/1,'macroexpand-1'/2,
         'macroexpand-all'/1,'macroexpand-all'/2]).

%% Miscellaneous useful LFE functions.
-export([format_exception/6,format_stacktrace/3]).

-import(lists, [reverse/1,reverse/2,map/2,foldl/3,dropwhile/2]).

%% -compile([export_all]).

%% is_bif(Name, Arity) -> bool().
%% is_erl_bif(Name, Arity) -> bool().
%% is_guard_bif(Name, Arity) -> bool().
%% Collected tests for valid BIFs in guards and expressions.

is_bif(Name, Ar) ->
    is_lfe_bif(Name, Ar) orelse is_erl_bif(Name, Ar).

is_erl_bif(Op, Ar) ->
    erl_internal:bif(Op, Ar)
    orelse erl_internal:arith_op(Op, Ar)
    orelse erl_internal:bool_op(Op, Ar)
    orelse erl_internal:comp_op(Op, Ar)
    orelse erl_internal:list_op(Op, Ar)
    orelse erl_internal:send_op(Op, Ar).

is_guard_bif(Op ,Ar) ->
    erl_internal:guard_bif(Op, Ar)
    orelse erl_internal:arith_op(Op, Ar)
    orelse erl_internal:bool_op(Op, Ar)
    orelse erl_internal:comp_op(Op, Ar).

%% is_symb(Sexpr) -> bool().
%% is_symb_list(Sexprs) -> bool().
%% is_proper_list(Sexprs) -> bool().

is_symb(S) -> is_atom(S).

is_symb_list([S|Ss]) when is_atom(S) ->
    is_symb_list(Ss);
is_symb_list([]) -> true;
is_symb_list(_) -> false.            %Might not be a proper list

is_proper_list([_|Ss]) -> is_proper_list(Ss);
is_proper_list([]) -> true;
is_proper_list(_) -> false.

%% is_core_form(Form) -> bool().
%%  Return true if Form (name) is one of the LFE core forms, else false.

%% Core data special forms.
is_core_form(quote) -> true;
is_core_form(cons) -> true;
is_core_form(car) -> true;
is_core_form(cdr) -> true;
is_core_form(list) -> true;
is_core_form(tuple) -> true;
is_core_form(binary) -> true;
is_core_form(map) -> true;
is_core_form(mref) -> true;
is_core_form(mset) -> true;
is_core_form(mupd) -> true;
is_core_form('map-get') -> true;
is_core_form('map-set') -> true;
is_core_form('map-update') -> true;
%% Core closure special forms.
is_core_form(lambda) -> true;
is_core_form('match-lambda') -> true;
is_core_form('let') -> true;
is_core_form('let-function') -> true;
is_core_form('letrec-function') -> true;
is_core_form('let-macro') -> true;
is_core_form('eval-when-compile') -> true;
is_core_form('define-function') -> true;
is_core_form('define-macro') -> true;
%% Core control special forms.
is_core_form('progn') -> true;
is_core_form('if') -> true;
is_core_form('case') -> true;
is_core_form('receive') -> true;
is_core_form('catch') -> true;
is_core_form('try') -> true;
is_core_form('funcall') -> true;
is_core_form(call) -> true;
%% Everything else is not special.
is_core_form(_) -> false.

%% proc_forms(FormFun, Forms, State) -> {Forms,State}.
%%  Process a (progn ... ) nested list of forms where top level list
%%  has elements {Form,LineNumber}. Return a flat list of results and
%%  passes through State. All the elements are processed left to
%%  right. The accumulator is in reverse order!

proc_forms(Fun, Fs, St) -> proc_forms(Fun, Fs, [], St).

proc_forms(Fun, [{['progn'|Bs],L}|Fs], Rs0, St0) ->
    {Rs1,St1} = proc_forms_progn(Fun, Bs, L, Rs0, St0),
    proc_forms(Fun, Fs, Rs1, St1);
proc_forms(Fun, [{F,L}|Fs], Rs, St0) ->
    {Frs,St1} = Fun(F, L, St0),
    proc_forms(Fun, Fs, reverse(Frs, Rs), St1);
proc_forms(_, [], Rs, St) -> {reverse(Rs),St}.

proc_forms_progn(Fun, [['progn'|Bbs]|Bs], L, Rs0, St0) ->
    {Rs1,St1} = proc_forms_progn(Fun, Bbs, L, Rs0, St0),
    proc_forms_progn(Fun, Bs, L, Rs1, St1);
proc_forms_progn(Fun, [B|Bs], L, Rs, St0) ->
    {Frs,St1} = Fun(B, L, St0),
    proc_forms_progn(Fun, Bs, L, reverse(Frs, Rs), St1);
proc_forms_progn(_, [], _, Rs, St) ->
    {Rs,St}.

%% proc_forms(Fun, [{['progn'|Bs],L}|Fs], Rs, St) ->
%%     proc_forms_progn(Fun, Bs, L, [], Fs, Rs, St);
%% proc_forms(Fun, [{F,L}|Fs], Rs, St0) ->
%%     {Frs,St1} = Fun(F, L, St0),
%%     proc_forms(Fun, Fs, reverse(Frs, Rs), St1);
%% proc_forms(_, [], Rs, St) -> {reverse(Rs),St}.

%% proc_forms_progn(Fun, [['progn'|Bs1]|Bs], L, Bss, Fs, Rs, St) ->
%%     proc_forms_progn(Fun, Bs1, L, [Bs|Bss], Fs, Rs, St);
%% proc_forms_progn(Fun, [B|Bs], L, Bss, Fs, Rs, St0) ->
%%     {Frs,St1} = Fun(B, L, St0),
%%     proc_forms_progn(Fun, Bs, L, Bss, Fs, reverse(Frs, Rs), St1);
%% proc_forms_progn(Fun, [], L, [Bs|Bss], Fs, Rs, St) ->
%%     proc_forms_progn(Fun, Bs, L, Bss, Fs, Rs, St);
%% proc_forms_progn(Fun, [], _, [], Fs, Rs, St) ->
%%     proc_forms(Fun, Fs, Rs, St).

%% Standard lisp library functions.
%% is_lfe_bif(Name, Arity) -> bool().
%% acons(Key, Value, Alist) -> Alist.
%% pairlis(Keys, Values, Alist) -> Alist.
%% assoc(Key, Alist) -> [Key|Value] | [].
%% assoc-if(Test, Alist) -> [Key|Value] | [].
%% assoc-if-not(Test, Alist) -> [Key|Value] | [].
%% rassoc(Value, Alist) -> [Key|Value] | [].
%% rassoc-if(Test, Alist) -> [Key|Value] | [].
%% rassoc-if-not(Test, Alist) -> [Key|Value] | [].
%% subst(New, Old, Tree) -> Tree.
%% subst-if(New, Test, Tree) -> Tree.
%% subst-if-not(New, Test, Tree) -> Tree.
%% sublis(Alist, Tree) -> Tree.
%% eval(Sexpr) -> Value.
%% macro-function(Name [,Environment]) -> Macro | [].
%% macroexpand(Form [,Environment]) -> Expansion | Form.
%% macroexpand-1(Form [,Environment]) -> Expansion | Form.
%% macroexpand-all(Form [,Environment]) -> Expansion | Form.

is_lfe_bif(acons, 3) -> true;
is_lfe_bif(pairlis, 2) -> true;
is_lfe_bif(pairlis, 3) -> true;
is_lfe_bif(assoc, 2) -> true;
is_lfe_bif('assoc-if', 2) -> true;
is_lfe_bif('assoc-if-not', 2) -> true;
is_lfe_bif(rassoc, 2) -> true;
is_lfe_bif('rassoc-if', 2) -> true;
is_lfe_bif('rassoc-if-not', 2) -> true;
is_lfe_bif(subst, 3) -> true;
is_lfe_bif('subst-if', 3) -> true;
is_lfe_bif('subst-if-not', 3) -> true;
is_lfe_bif(sublis, 2) -> true;
is_lfe_bif(eval, 1) -> true;
is_lfe_bif(eval, 2) -> true;
is_lfe_bif('macro-function', 1) -> true;
is_lfe_bif('macro-function', 2) -> true;
is_lfe_bif(macroexpand, 1) -> true;
is_lfe_bif(macroexpand, 2) -> true;
is_lfe_bif('macroexpand-1', 1) -> true;
is_lfe_bif('macroexpand-1', 2) -> true;
is_lfe_bif('macroexpand-all', 1) -> true;
is_lfe_bif('macroexpand-all', 2) -> true;
is_lfe_bif(_, _) -> false.

acons(K, V, Alist) -> [[K|V]|Alist].

pairlis(Ks, Vs) -> pairlis(Ks, Vs, []).

pairlis([K|Ks], [V|Vs], Alist) ->
    [[K|V]|pairlis(Ks, Vs, Alist)];
pairlis([], [], Alist) -> Alist.

assoc(K, [[K|_]=Pair|_]) -> Pair;
assoc(K, [_|L]) -> assoc(K, L);
assoc(_, []) -> [].

'assoc-if'(Pred, [[K|_]=Pair|L]) ->
    case Pred(K) of
    true -> Pair;
    false -> 'assoc-if'(Pred, L)
    end;
'assoc-if'(_, []) -> [].

'assoc-if-not'(Pred, [[K|_]=Pair|L]) ->
    case Pred(K) of
    false -> Pair;
    true -> 'assoc-if-not'(Pred, L)
    end;
'assoc-if-not'(_, []) -> [].

rassoc(V, [[_|V]=Pair|_]) -> Pair;
rassoc(V, [_|L]) -> rassoc(V, L);
rassoc(_, []) -> [].

'rassoc-if'(Pred, [[_|V]=Pair|L]) ->
    case Pred(V) of
    true -> Pair;
    false -> 'rassoc-if'(Pred, L)
    end;
'rassoc-if'(_, []) -> [].

'rassoc-if-not'(Pred, [[_|V]=Pair|L]) ->
    case Pred(V) of
    false -> Pair;
    true -> 'rassoc-if-not'(Pred, L)
    end;
'rassoc-if-not'(_, []) -> [].

%% subst(New, Old, Tree) -> Tree.

subst(New, Old, Old) -> New;
subst(New, Old, [H|T]) ->
    [subst(New, Old,H)|subst(New, Old, T)];
subst(_, _, Tree) -> Tree.

%% subst-if(New, Test, Tree) -> Tree.

'subst-if'(New, Test, Tree) ->
    case Test(Tree) of
    true -> New;
    false ->
        case Tree of
        [H|T] ->
            ['subst-if'(New, Test, H)|'subst-if'(New, Test, T)];
        _ -> Tree
        end
    end.

%% subst-if-not(New, Test, Tree) -> Tree.

'subst-if-not'(New, Test, Tree) ->
    case Test(Tree) of
    false -> New;
    true ->
        case Tree of
        [H|T] ->
            ['subst-if-not'(New, Test, H)|'subst-if-not'(New, Test, T)];
        _ -> Tree
        end
    end.

%% sublis(AList, Tree) -> Tree.

sublis(Alist, Tree) ->
    case assoc(Tree, Alist) of
    [_|New] -> New;                         %Found it
    [] ->                                   %Not there
        case Tree of
        [H|T] ->
            [sublis(Alist, H)|sublis(Alist, T)];
        _ -> Tree
        end
    end.

eval(Sexpr) -> eval(Sexpr, lfe_env:new()).  %Empty environment.
eval(Sexpr, Env) -> lfe_eval:expr(Sexpr, Env).

'macro-function'(Symb) -> 'macro-function'(Symb, lfe_env:new()).
'macro-function'(Symb, Env) ->
    case lfe_env:get_mbinding(Symb, Env) of
        {yes,Macro} ->
            Macro;
        no -> []
    end.

macroexpand(Form) -> macroexpand(Form, lfe_env:new()).
macroexpand(Form, Env) ->
    case lfe_macro:expand_expr(Form, Env) of
        {yes,Exp} -> Exp;
        no -> Form
    end.

'macroexpand-1'(Form) -> 'macroexpand-1'(Form, lfe_env:new()).
'macroexpand-1'(Form, Env) ->
    case lfe_macro:expand_expr_1(Form, Env) of
        {yes,Exp} -> Exp;
        no -> Form
    end.

'macroexpand-all'(Form) -> 'macroexpand-all'(Form, lfe_env:new()).
'macroexpand-all'(Form, Env) -> lfe_macro:expand_expr_all(Form, Env).

%% Miscellaneous useful LFE functions.

%% format_exception(Class, Error, Stacktrace, SkipFun, FormatFun, Indentation)
%%      -> DeepCharList.
%%  Format an exception. Class, Error and Stacktrace describe the
%%  exception; SkipFun is used to trim the end of stack; FormatFun is
%%  used to format terms; and Indentation is the current column.

format_exception(Cl, Error, St, Sf, Ff, I) ->
    Cs = case Cl of                         %Class type as string
         throw -> "throw";
         exit -> "exit";
         error -> "error"
     end,
    P = "exception " ++ Cs ++ ": ",         %Class description string
    [P,lfe_io:prettyprint1(Error, 10, length(P)+I-1),"\n",
     format_stacktrace(St, Sf, Ff)].

%% format_stacktrace(Stacktrace, SkipFun, FormatFun) -> DeepCharList.
%%  Format a stacktrace. SkipFun is used to trim the end of stack;
%%  FormatFun is used to format terms.

format_stacktrace(St0, Skip, Format) ->
    St1 = reverse(dropwhile(Skip, reverse(St0))),
    Print = fun ({M,F,A}) when is_integer(A) ->    %Pre R15
            lfe_io:format1("  in (~w ~w ~w)\n", [M,F,A]);
        ({M,F,A}) -> ["  in ",Format([':',M,F|A], 5),"\n"];
        %% R15 and later.
        ({M,F,A,_}) when is_integer(A) ->
            lfe_io:format1("  in (~w ~w ~w)\n", [M,F,A]);
        ({M,F,A,_}) -> ["  in ",Format([':',M,F|A], 5),"\n"]
    end,
    map(Print, St1).
