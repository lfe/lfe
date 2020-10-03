%% Copyright (c) 2020 Robert Virding
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

%% File    : scm.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang scheme like macros

-module(scm).

-export(['LFE-EXPAND-EXPORTED-MACRO'/3]).

-export([mbe_syntax_rules_proc/4,mbe_syntax_rules_proc/5,
         mbe_match_pat/3,mbe_get_bindings/3,mbe_expand_pattern/3]).

-import(lists, [member/2,map/2,all/2,any/2]).

%% 'LFE-EXPAND-EXPORTED-MACRO'(Name, Args, Env) -> {yes,Expansion} | no.
%%  Explicitly define this function so we can call the begin, define,
%%  define-syntax and let-syntax macros without having to include
%%  scm.lfe. These are exactly equivalent to those macros.

'LFE-EXPAND-EXPORTED-MACRO'(MacroName, MacroArgs, _Env) ->
    case [MacroName|MacroArgs] of
        %% These are easy.
        ['begin'|Body] ->
            {yes,[progn|Body]};
        ['define',Head|Body] ->
            Exp = case lfe_lib:is_symb_list(Head) of
                      true ->
                          [hd(Head),[],[lambda,tl(Head)|Body]];
                      false ->
                          [Head,[],Body]
                  end,
            {yes,['define-function'|Exp]};
        %% Now for the syntax macros.
        ['define-syntax',Name,Def] ->
            {Meta,Mdef} = exp_syntax(Name, Def),
            {yes,['define-macro',Name,Meta,Mdef]};
        ['let-syntax',Defs|Body] ->
            Fun = fun ([Name,Def]) ->
                          {_,Def} = exp_syntax(Name, Def),
                          [Name,Def]
                  end,
            Mdefs = map(Fun, Defs),
            {yes,['let-macro',Mdefs|Body]};
        [defsyntax,Name|Rules] ->
            {Meta,Mdef} = exp_rules(Name, [], Rules),
            {yes,['define-macro',Name,Meta,Mdef]};
        _ -> no
    end.


%% exp_syntax(Name, Def) -> {Meta,Lambda | MatchLambda}.
%%  N.B. New macro definition is function of 2 arguments, the whole
%%  argument list of macro call, and the current macro environment.

exp_syntax(Name, Def) ->
    case Def of
        [macro|Cls] ->
            Mcls = map(fun ([Pat|Body]) -> [[Pat,'$ENV']|Body] end, Cls),
            {[],['match-lambda'|Mcls]};
        ['syntax-rules'|Rules] ->
            exp_rules(Name, [], Rules)
    end.

%% exp_rules(Name, Keywords, Rules) -> {Meta,Lambda}.
%%  Expand into call function which expands macro an invocation time,
%%  this saves much space and costs us nothing.
%%  N.B. New macro definition is function of 2 arguments, the whole
%%  argument list of macro call, and the current macro environment.

exp_rules(Name, Keywords, Rules) ->
    {[],[lambda,[args,'$ENV'],
         [':',scm,mbe_syntax_rules_proc,
          [quote,Name],[quote,Keywords],[quote,Rules],args]]}.

%% Macro by Example
%% Proper syntax-rules which can handle ... ellipsis by Dorai Sitaram.
%%
%% While we extend patterns to include tuples and binaries as in
%% normal LFE we leave the keyword handling in even though it is
%% subsumed by quotes and not really used.

%% To make it more lispy!
-define(car(L), hd(L)).
-define(cdr(L), tl(L)).
-define(cadr(L), hd(tl(L))).
-define(cddr(L), tl(tl(L))).

-define(mbe_ellipsis(Car, Cddr), [Car,'...'|Cddr]).

is_mbe_symbol(S) ->
    is_atom(S) andalso not is_boolean(S).

%% Tests if ellipsis pattern, (p ... . rest)
%% is_mbe_ellipsis(?mbe_ellipsis(_, _)) -> true;
%% is_mbe_ellipsis(_) -> false.

mbe_match_pat([quote,P], E, _) -> P =:= E;
mbe_match_pat([tuple|Ps], [tuple|Es], Ks) ->    %Match tuple constructor
    mbe_match_pat(Ps, Es, Ks);
mbe_match_pat([tuple|Ps], E, Ks) ->             %Match literal tuple
    case is_tuple(E) of
        true -> mbe_match_pat(Ps, tuple_to_list(E), Ks);
        false -> false
    end;
mbe_match_pat(?mbe_ellipsis(Pcar, _), E, Ks) ->
    case lfe_lib:is_proper_list(E) of
        true ->
            all(fun (X) -> mbe_match_pat(Pcar, X, Ks) end, E);
        false -> false
    end;
mbe_match_pat([Pcar|Pcdr], E, Ks) ->
    case E of
        [Ecar|Ecdr] ->
            mbe_match_pat(Pcar, Ecar, Ks) andalso
                mbe_match_pat(Pcdr, Ecdr, Ks);
        _ -> false
    end;
mbe_match_pat(Pat, E, Ks) ->
    case is_mbe_symbol(Pat) of
        true ->
            case member(Pat, Ks) of
                true -> Pat =:= E;
                false -> true
            end;
        false -> Pat =:= E
    end.

mbe_get_ellipsis_nestings(Pat, Ks) ->
    m_g_e_n(Pat, Ks).

m_g_e_n([quote,_], _) -> [];
m_g_e_n([tuple|Ps], Ks) -> m_g_e_n(Ps, Ks);
m_g_e_n(?mbe_ellipsis(Pcar, Pcddr), Ks) ->
    [m_g_e_n(Pcar, Ks)|m_g_e_n(Pcddr, Ks)];
m_g_e_n([Pcar|Pcdr], Ks) ->
    m_g_e_n(Pcar, Ks) ++ m_g_e_n(Pcdr, Ks);
m_g_e_n(Pat, Ks) ->
    case is_mbe_symbol(Pat) of
        true ->
            case member(Pat, Ks) of
                true -> [];
                false -> [Pat]
            end;
        false -> []
    end.

mbe_ellipsis_sub_envs(Nestings, R) ->
    ormap(fun (C) ->
          case mbe_intersect(Nestings, ?car(C)) of
              true -> ?cdr(C);
              false -> false
          end end, R).

%% Return first value of F applied to elements in list which is not false.
ormap(F, [H|T]) ->
    case F(H) of
        false -> ormap(F, T);
        V -> V
    end;
ormap(_, []) -> false.

mbe_intersect(V, Y) ->
    case is_mbe_symbol(V) orelse is_mbe_symbol(Y) of
        true -> V =:= Y;
        false ->
            any(fun (V0) ->
                        any(fun (Y0) -> mbe_intersect(V0, Y0) end, Y)
                end, V)
    end.

%% mbe_get_bindings(Pattern, Expression, Keywords) -> Bindings.

mbe_get_bindings([quote,_], _, _) -> [];
mbe_get_bindings([tuple|Ps], [tuple|Es], Ks) ->    %Tuple constructor
    mbe_get_bindings(Ps, Es, Ks);
mbe_get_bindings([tuple|Ps], E, Ks) ->        %Literal tuple
    mbe_get_bindings(Ps, tuple_to_list(E), Ks);
mbe_get_bindings(?mbe_ellipsis(Pcar, _), E, Ks) ->
    [[mbe_get_ellipsis_nestings(Pcar, Ks) |
      map(fun (X) -> mbe_get_bindings(Pcar, X, Ks) end, E)]];
mbe_get_bindings([Pcar|Pcdr], [Ecar|Ecdr], Ks) ->
    mbe_get_bindings(Pcar, Ecar, Ks) ++
        mbe_get_bindings(Pcdr, Ecdr, Ks);
mbe_get_bindings(Pat, E, Ks) ->
    case is_mbe_symbol(Pat) of
        true ->
            case member(Pat, Ks) of
                true -> [];
                false -> [[Pat|E]]
            end;
        false -> []
    end.

%% mbe_expand_pattern(Pattern, Bindings, Keywords) -> Form.

mbe_expand_pattern([quote,P], R, Ks) ->
    [quote,mbe_expand_pattern(P, R, Ks)];
mbe_expand_pattern([tuple|Ps], R, Ks) ->
    [tuple|mbe_expand_pattern(Ps, R, Ks)];
mbe_expand_pattern(?mbe_ellipsis(Pcar, Pcddr), R, Ks) ->
    Nestings = mbe_get_ellipsis_nestings(Pcar, Ks),
    Rr = mbe_ellipsis_sub_envs(Nestings, R),
    map(fun (R0) -> mbe_expand_pattern(Pcar, R0 ++ R, Ks) end, Rr) ++
        mbe_expand_pattern(Pcddr, R, Ks);
mbe_expand_pattern([Pcar|Pcdr], R, Ks) ->
    [mbe_expand_pattern(Pcar, R, Ks)|
     mbe_expand_pattern(Pcdr, R, Ks)];
mbe_expand_pattern(Pat, R, Ks) ->
    case is_mbe_symbol(Pat) of
        true ->
            case member(Pat, Ks) of
                true -> Pat;
                false ->
                    case lfe:assoc(Pat, R) of
                        [_|Cdr] -> Cdr;
                        [] -> Pat
                    end
            end;
        false -> Pat
    end.

%% mbe_syntax_rules_proc(Name, Keywords, Rules, Argsym, Keywordsym) ->
%%      Sexpr.
%%  Generate the sexpr to evaluate in a macro from Name and
%%  Rules. When the sexpr is applied to arguments (in Argsym) and
%%  evaluated then expansion is returned.

%% Return sexpr to evaluate.
mbe_syntax_rules_proc(Name, Ks0, Cls, Argsym, Ksym) ->
    Ks = [Name|Ks0],
    %% Don't prepend the macro name to the arguments!
    ['let',[[Ksym,[quote,Ks]]],
     ['cond'] ++
         map(fun (C) ->
                     Inpat = hd(C),
                     Outpat = hd(tl(C)),
                     [[':',lfe_macro,mbe_match_pat,[quote,Inpat], Argsym, Ksym],
                      ['let',
                       [[r,[':',lfe_macro,mbe_get_bindings,
                            [quote,Inpat],Argsym,Ksym]]],
                       [':',lfe_macro,mbe_expand_pattern,
                        [quote,Outpat],r,Ksym]]]
             end, Cls) ++
         [[[quote,true],[':',erlang,error,
                         [tuple,
                          [quote,expand_macro],
                          [cons,[quote,Name],Argsym], %??? Must check this
                          [quote,macro_clause]]]]]].

%% Do it all directly.
mbe_syntax_rules_proc(Name, Ks0, Cls, Args) ->
    Ks = [Name|Ks0],
    case ormap(fun ([Pat,Exp]) ->
                       case mbe_match_pat(Pat, Args, Ks) of
                           true ->
                               R = mbe_get_bindings(Pat, Args, Ks),
                               [mbe_expand_pattern(Exp, R, Ks)];
                           false -> false
                       end
               end, Cls) of
        [Res] -> Res;
        false -> erlang:error({expand_macro,[Name|Args],macro_clause})
    end.
