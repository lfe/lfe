%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(lfe_edlin_expand).

%% A default LFE expand function for edlin, expanding modules and
%% functions. It knows about LFE symbol syntax but as yet only works
%% for (mod:fun ...) and not (: mod fun ...)

-export([expand/1, format_matches/1]).

-import(lists, [reverse/1, nthtail/2, prefix/2]).

%% expand(CurrentBefore) ->
%%     {yes, Expansion, Matches} | {no, Expansion, Matches}
%%  Try to expand the word before as either a module name or a
%%  function name. CurrentBefore is reversed and over_word/3 reverses
%%  the characters it finds. In certain cases possible expansions are
%%  printed.

expand(Bef0) ->
    {Bef1,S1,_} = over_symbol(Bef0, [], 0),
    case Bef1 of
        [$:|Bef2] ->                            %After a ':'
            {Bef3,S2,_} = over_symbol(Bef2, [], 0),
            need_lparen(Bef3, fun () -> expand_function_name(S2, S1) end);
        Bef2 ->
            need_lparen(Bef2, fun () -> expand_module_name(S1) end)
    end.

need_lparen(Bef, Do) ->
    case over_white(Bef, [], 0) of
        {[$(|_],_,_} -> Do();
        {_,_,_} -> {no,[],[]}
    end.

%% expand(Bef0) ->
%%     {Bef1,Word,_} = edlin:over_word(Bef0, [], 0),
%%     case over_white(Bef1, [], 0) of
%%         {[$:|Bef2],_White,_Nwh} ->
%%             {Bef3,_White1,_Nwh1} = over_white(Bef2, [], 0),
%%             {_,Mod,_Nm} = edlin:over_word(Bef3, [], 0),
%%             expand_function_name(Mod, Word);
%%         {_,_,_} ->
%%             expand_module_name(Word)
%%     end.

expand_module_name(Prefix) ->
    match(Prefix, code:all_loaded(), ":").

expand_function_name(ModStr, FuncPrefix) ->
    case to_symbol(ModStr) of
        {ok,Mod} ->
            case erlang:module_loaded(Mod) of
                true ->
                    L = Mod:module_info(),
                    case lists:keyfind(exports, 1, L) of
                        {_, Exports} ->
                            match(FuncPrefix, Exports, " ");
                        _ ->
                            {no,[],[]}
                    end;
                false ->
                    {no,[],[]}
            end;
        error ->
            {no,[],[]}
    end.

%% If it's a quoted symbol, atom_to_list/1 will do the wrong thing.
to_symbol(Str) ->
    case lfe_scan:string(Str) of
        {ok,[{symbol,_,A}],_} -> {ok,A};
        _ -> error
    end.

match(Prefix, Alts, Extra0) ->
    Len = length(Prefix),
    Matches = lists:sort([{S, A} || {H, A} <- Alts,
                                    begin
                                        S = hd(lfe_io:fwrite1("~w", [H])),
                                        prefix(Prefix, S)
                                    end]),
    case longest_common_head([N || {N, _} <- Matches]) of
        {partial, []} ->
            {no, [], Matches};                  %format_matches(Matches)};
        {partial, Str} ->
            case nthtail(Len, Str) of
                [] -> {yes,[],Matches};         %format_matches(Matches)};
                Remain -> {yes,Remain,[]}
            end;
        {complete, Str} ->
            Extra = case {Extra0,Matches} of
                        {" ",[{Str,0}]} -> ")";
                        {_,_} -> Extra0
                    end,
            {yes, nthtail(Len, Str) ++ Extra, []};
        no ->
            {no,[],[]}
    end.

%% Return the list of names L in multiple columns.
format_matches(L) ->
    S = format_col(lists:sort(L), []),
    ["\n" | S].

format_col([], _) -> [];
format_col(L, Acc) -> format_col(L, field_width(L), 0, Acc).

format_col(X, Width, Len, Acc) when Width + Len > 79 ->
    format_col(X, Width, 0, ["\n" | Acc]);
format_col([A|T], Width, Len, Acc0) ->
    H = case A of
            %% If it's a tuple {string(), integer()}, we assume it's an
            %% arity, and meant to be printed.
            {H0, I} when is_integer(I) ->
                H0 ++ "/" ++ integer_to_list(I);
            {H1, _} -> H1;
            H2 -> H2
        end,
    Acc = [io_lib:format("~-*s", [Width,H]) | Acc0],
    format_col(T, Width, Len+Width, Acc);
format_col([], _, _, Acc) ->
    lists:reverse(Acc, "\n").

field_width(L) -> field_width(L, 0).

field_width([{H,_}|T], W) ->
    case length(H) of
        L when L > W -> field_width(T, L);
        _ -> field_width(T, W)
    end;
field_width([H|T], W) ->
    case length(H) of
        L when L > W -> field_width(T, L);
        _ -> field_width(T, W)
    end;
field_width([], W) when W < 40 ->
    W + 4;
field_width([], _) ->
    40.

longest_common_head([]) ->
    no;
longest_common_head(LL) ->
    longest_common_head(LL, []).

longest_common_head([[]|_], L) ->
    {partial, reverse(L)};
longest_common_head(LL, L) ->
    case same_head(LL) of
        true ->
            [[H|_]|_] = LL,
            LL1 = all_tails(LL),
            case all_nil(LL1) of
                true ->
                    {complete, reverse([H|L])};
                false ->
                    longest_common_head(LL1, [H|L])
            end;
        false ->
            {partial, reverse(L)}
    end.

same_head([[H|_]|T1]) -> same_head(H, T1).

same_head(H, [[H|_]|T]) -> same_head(H, T);
same_head(_, [])        -> true;
same_head(_, _)         -> false.

all_tails(LL) -> all_tails(LL, []).

all_tails([[_|T]|T1], L) -> all_tails(T1, [T|L]);
all_tails([], L)         -> L.

all_nil([]) -> true;
all_nil([[] | Rest]) -> all_nil(Rest);
all_nil(_) -> false.

%% over_symbol(Chars, InitialStack, InitialCount) ->
%%      {RemainingChars,CharStack,Count}
%% over_non_symbol(Chars, InitialStack, InitialCount) ->
%%      {RemainingChars,CharStack,Count}
%%  Step over symbol/non-symbol characters pushing the stepped over
%%  ones on the stack.

over_symbol(Cs, Stack, N) ->
    L = length([1 || $| <- Cs]),
    case L rem 2 of
        0 -> over_symbol1(Cs, Stack, N);
        1 -> until_quote(Cs, Stack, N)
    end.

until_quote([$||Cs], Stack, N) ->
    {Cs, [$||Stack], N+1};
until_quote([C|Cs], Stack, N) ->
    until_quote(Cs, [C|Stack], N+1).

over_symbol1([$||Cs], Stack, N) ->
    until_quote(Cs, [$||Stack], N+1);
over_symbol1(Cs, Stack, N) ->
    over_symbol2(Cs, Stack, N).

over_symbol2([C|Cs], Stack, N) ->
    case symbol_char(C) of
        true -> over_symbol2(Cs, [C|Stack], N+1);
        false -> {[C|Cs],Stack,N}
    end;
over_symbol2([], Stack, N) when is_integer(N) ->
    {[],Stack,N}.

%% over_non_symbol([C|Cs], Stack, N) ->
%%     case symbol_char(C) of
%%         true -> {[C|Cs],Stack,N};
%%         false -> over_non_symbol(Cs, [C|Stack], N+1)
%%     end;
%% over_non_symbol([], Stack, N) ->
%%     {[],Stack,N}.

symbol_char($:) -> false;                       %We want to separate on this
symbol_char(C) -> lfe_scan:symbol_char(C).

%% over_white(Chars, InitialStack, InitialCount) ->
%%    {RemainingChars,CharStack,Count}.

over_white([$\s|Cs], Stack, N) ->
    over_white(Cs, [$\s|Stack], N+1);
over_white([$\t|Cs], Stack, N) ->
    over_white(Cs, [$\t|Stack], N+1);
over_white(Cs, Stack, N) when is_list(Cs) ->
    {Cs,Stack,N}.
