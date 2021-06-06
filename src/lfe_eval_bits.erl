%% Copyright (c) 2021 Robert Virding
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

%% File    : lfe_eval_bits.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang interpreter functions for binaries.

%%% We follow Erlang here in many cases even though it is sometimes a
%%% bit strange. In a fun argument where when matching a binary we
%%% import the size of bitseg as a variable from the environment not
%%% just from earlier segments. No other argument variables are
%%% imported.

-module(lfe_eval_bits).

-export([expr_bitsegs/2,match_bitsegs/4]).

-import(lists, [foldl/3,foldr/3]).

-import(lfe_env, [add_vbinding/3,add_vbindings/2,get_vbinding/2,
                  add_fbinding/4,add_fbindings/2,
                  add_ibinding/5]).
-import(orddict, [find/2,fetch/2,store/3,is_key/2]).

-include("lfe.hrl").

-define(STACKTRACE,
        element(2, erlang:process_info(self(), current_stacktrace))).

-define(EVAL_ERROR(Error), erlang:raise(error, Error, ?STACKTRACE)).

%% expr_bitsegs(Bitsegs, EvalFun) -> Binary.
%%  Construct a binary from Bitsegs. This code is taken from
%%  eval_bits.erl. Pass in an evaluator function to be used when
%%  evaluating vale and size expression.

expr_bitsegs(Segs, Eval) ->
    Vsps = get_bitsegs(Segs),
    eval_bitsegs(Vsps, Eval).

get_bitsegs(Segs) ->
    foldr(fun (S, Vs) -> get_bitseg(S, Vs) end, [], Segs).

%% get_bitseg(Bitseg, ValSpecs) -> ValSpecs.
%%  A bitseg is either an atomic value, a list of value and specs, or
%%  a string.

get_bitseg([Val|Specs]=Seg, Vsps) ->
    case lfe_lib:is_posint_list(Seg) of         %Is bitseg a string?
        true ->                                 %A string
            {Sz,Ty} = get_bitspecs([]),
            foldr(fun (V, Vs) -> [{V,Sz,Ty}|Vs] end, Vsps, Seg);
        false ->                                %A value and spec
            {Sz,Ty} = get_bitspecs(Specs),
            case lfe_lib:is_posint_list(Val) of %Is Val a string?
                true -> foldr(fun (V, Vs) -> [{V,Sz,Ty}|Vs] end, Vsps, Val);
                false -> [{Val,Sz,Ty}|Vsps]     %The default
            end
    end;
get_bitseg(Val, Vsps) ->
    {Sz,Ty} = get_bitspecs([]),
    [{Val,Sz,Ty}|Vsps].

%% get_bitspec(Specs) -> {Size,Type}.
%%  Get the error handling as we want it.

get_bitspecs(Ss) ->
    case lfe_bits:get_bitspecs(Ss) of
        {ok,Sz,Ty} -> {Sz,Ty};
        {error,Error} -> eval_error(Error)
    end.

%% eval_bitsegs(VSTys, EvalFun) -> Binary.
%%  The evaluator function is use to evaluate the value and size
%%  fields.

eval_bitsegs(Vsps, Eval) ->
    foldl(fun ({Val,Sz,Ty}, Acc) ->
                  Bin = eval_bitseg(Val, Sz, Ty, Eval),
                  <<Acc/bitstring,Bin/bitstring>>
          end, <<>>, Vsps).

eval_bitseg(Val, Sz, Ty, Eval) ->
    V = Eval(Val),
    eval_exp_bitseg(V, Sz, Eval, Ty).

%% eval_exp_bitseg(Value, Size, EvalSize, {Type,Unit,Sign,Endian}) -> Binary.

eval_exp_bitseg(Val, Size, Eval, Type) ->
    case Type of
        %% Integer types.
        {integer,Un,Si,En} ->
            Sz = Eval(Size),
            eval_int_bitseg(Val, Sz*Un, Si, En);
        %% Unicode types, ignore unused fields.
        {utf8,_,_,_} -> <<Val/utf8>>;
        {utf16,_,_,En} -> eval_utf16_bitseg(Val, En);
        {utf32,_,_,En} -> eval_utf32_bitseg(Val, En);
        %% Float types.
        {float,Un,_,En} ->
            Sz = Eval(Size),
            eval_float_bitseg(Val, Sz*Un, En);
        %% Binary types.
        {binary,Unit,_,_} ->
            if Size == all ->
                    case bit_size(Val) of
                        Sz when Sz rem Unit =:= 0 ->
                            <<Val:Sz/bitstring>>;
                        _ -> badarg_error()
                    end;
               true ->
                    Sz = Eval(Size),
                    <<Val:(Sz*Unit)/bitstring>>
            end
    end.

eval_int_bitseg(Val, Sz, signed, big) -> <<Val:Sz/signed>>;
eval_int_bitseg(Val, Sz, unsigned, big) -> <<Val:Sz>>;
eval_int_bitseg(Val, Sz, signed, little) -> <<Val:Sz/little-signed>>;
eval_int_bitseg(Val, Sz, unsigned, little) -> <<Val:Sz/little>>;
eval_int_bitseg(Val, Sz, signed, native) -> <<Val:Sz/native-signed>>;
eval_int_bitseg(Val, Sz, unsigned, native) -> <<Val:Sz/native>>.

eval_utf16_bitseg(Val, big) -> <<Val/utf16-big>>;
eval_utf16_bitseg(Val, little) -> <<Val/utf16-little>>;
eval_utf16_bitseg(Val, native) -> <<Val/utf16-native>>.

eval_utf32_bitseg(Val, big) -> <<Val/utf32-big>>;
eval_utf32_bitseg(Val, little) -> <<Val/utf32-little>>;
eval_utf32_bitseg(Val, native) -> <<Val/utf32-native>>.

eval_float_bitseg(Val, Sz, big) -> <<Val:Sz/float>>;
eval_float_bitseg(Val, Sz, little) -> <<Val:Sz/float-little>>;
eval_float_bitseg(Val, Sz, native) -> <<Val:Sz/float-native>>.

%% match_bitsegs(BitSegs, Binary PatBindings, Env) -> {yes,PatBindings} | no.
%%  Match Bitsegs against Binary. This code is taken from
%%  eval_bits.erl. Bitspec errors generate an error. Bad matches
%%  result in an error, we use catch to trap it.

match_bitsegs(Segs, Bin, Pbs0, Env) ->
    Psps = get_bitsegs(Segs),
    match_bitsegs(Psps, Bin, [], Pbs0, Env).

match_bitsegs([{Pat,Sz,Ty}|Psps], Bin0, Bbs0, Pbs0, Env) ->
    case match_bitseg(Pat, Sz, Ty, Bin0, Bbs0, Pbs0, Env) of
        {yes,Bin1,Bbs1,Pbs1} ->
            match_bitsegs(Psps, Bin1, Bbs1, Pbs1, Env);
        no -> no
    end;
match_bitsegs([], <<>>, _, Pbs, _) -> {yes,Pbs}; %Reached the end of both
match_bitsegs([], _, _, _, _) -> no.            %More to go

match_bitseg(Pat, Size, Type, Bin0, Bbs0, Pbs0, Env) ->
    Sz = get_pat_bitsize(Size, Type, Bbs0, Pbs0, Env),
    case catch {ok,get_pat_bitseg(Bin0, Sz, Type)} of
        {ok,{Val,Bin1}} ->
            case match_bitexpr(Pat, Val, Bbs0, Pbs0, Env) of
                {yes,Bbs1,Pbs1} -> {yes,Bin1,Bbs1,Pbs1};
                no -> no
            end;
        _ -> no
    end.

get_pat_bitsize(all, {Ty,_,_,_}, _, _, _) ->
    if Ty =:= binary -> all;
       true -> eval_error(illegal_bitsize)
    end;
get_pat_bitsize(undefined, {Ty,_,_,_}, _, _, _) ->
    if Ty =:= utf8; Ty =:= utf16; Ty =:= utf32 -> undefined;
       true -> eval_error(illegal_bitsize)
    end;
get_pat_bitsize(S, _, _, _, _) when is_integer(S) -> S;
get_pat_bitsize(S, _, Bbs, _, Env) when is_atom(S) ->
    %% Variable either in environment or bound in binary.
    case get_vbinding(S, Env) of
        {yes,V} -> V;
        no ->
            case find(S, Bbs) of
                {ok,V} -> V;
                error -> unbound_symb_error(S)
            end
    end.

match_bitexpr(N, Val, Bbs, Pbs, _) when is_number(N) ->
    if N =:= Val -> {yes,Bbs,Pbs};
       true -> no
    end;
match_bitexpr('_', _, Bbs, Pbs, _) -> {yes,Bbs,Pbs};
match_bitexpr(S, Val, Bbs, Pbs, _) when is_atom(S) ->
    %% We know that if variable is in Pbs it will also be in Bbs!
    case find(S, Pbs) of
        {ok,Val} -> {yes,Bbs,Pbs};              %Bound to the same value
        {ok,_} -> no;                           %Bound to a different value
        error ->                                %Not yet bound
            {yes,store(S, Val, Bbs),store(S, Val, Pbs)}
    end;
match_bitexpr(_, _, _, _, _) -> eval_error(illegal_bitseg).

%% get_pat_bitseg(Binary, Size, {Type,Unit,Sign,Endian}) -> {Value,RestBinary}.
%%  This function can signal error if impossible to get specified bit
%%  segment.

get_pat_bitseg(Bin, Size, Type) ->
    case Type of
        %% Integer types.
        {integer,Un,Si,En} ->
            get_int_bitseg(Bin, Size*Un, Si, En);
        %% Unicode types, ignore unused bitsegs.
        {utf8,_,_,_} -> get_utf8_bitseg(Bin);
        {utf16,_,_,En} -> get_utf16_bitseg(Bin, En);
        {utf32,_,_,En} -> get_utf32_bitseg(Bin, En);
        %% Float types.
        {float,Un,_,En} -> get_float_bitseg(Bin, Size*Un, En);
        %% Binary types.
        {binary,Un,_,_} ->
            if Size == all ->
                    0 = (bit_size(Bin) rem Un),
                    {Bin,<<>>};
               true ->
                    TotSize = Size * Un,
                    <<Val:TotSize/bitstring,Rest/bitstring>> = Bin,
                    {Val,Rest}
            end
    end.

get_int_bitseg(Bin, Sz, signed, big) ->
    <<Val:Sz/big-signed,Rest/bitstring>> = Bin,
    {Val,Rest};
get_int_bitseg(Bin, Sz, unsigned, big) ->
    <<Val:Sz/big-unsigned,Rest/bitstring>> = Bin,
    {Val,Rest};
get_int_bitseg(Bin, Sz, signed, little) ->
    <<Val:Sz/little-signed,Rest/bitstring>> = Bin,
    {Val,Rest};
get_int_bitseg(Bin, Sz, unsigned, little) ->
    <<Val:Sz/little-unsigned,Rest/bitstring>> = Bin,
    {Val,Rest};
get_int_bitseg(Bin, Sz, signed, native) ->
    <<Val:Sz/native-signed,Rest/bitstring>> = Bin,
    {Val,Rest};
get_int_bitseg(Bin, Sz, unsigned, native) ->
    <<Val:Sz/native-unsigned,Rest/bitstring>> = Bin,
    {Val,Rest}.

get_utf8_bitseg(Bin) ->
    <<Val/utf8,Rest/bitstring>> = Bin,
    {Val,Rest}.

get_utf16_bitseg(Bin, big) ->
    <<Val/utf16-big,Rest/bitstring>> = Bin,
    {Val,Rest};
get_utf16_bitseg(Bin, little) ->
    <<Val/utf16-little,Rest/bitstring>> = Bin,
    {Val,Rest};
get_utf16_bitseg(Bin, native) ->
    <<Val/utf16-native,Rest/bitstring>> = Bin,
    {Val,Rest}.

get_utf32_bitseg(Bin, big) ->
    <<Val/utf32-big,Rest/bitstring>> = Bin,
    {Val,Rest};
get_utf32_bitseg(Bin, little) ->
    <<Val/utf32-little,Rest/bitstring>> = Bin,
    {Val,Rest};
get_utf32_bitseg(Bin, native) ->
    <<Val/utf32-native,Rest/bitstring>> = Bin,
    {Val,Rest}.

get_float_bitseg(Bin, Sz, big) ->
    <<Val:Sz/float,Rest/bitstring>> = Bin,
    {Val,Rest};
get_float_bitseg(Bin, Sz, little) ->
    <<Val:Sz/float-little,Rest/bitstring>> = Bin,
    {Val,Rest};
get_float_bitseg(Bin, Sz, native) ->
    <<Val:Sz/float-native,Rest/bitstring>> = Bin,
    {Val,Rest}.

%% Error functions.

badarg_error() -> eval_error(badarg).

unbound_symb_error(Sym) ->
    eval_error({unbound_symb,Sym}).

eval_error(Error) ->
    erlang:raise(error, Error, ?STACKTRACE).
