%% Copyright (c) 2011 Robert Virding. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

%%% File    : lfe_bits.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang common functions for binaries.

-module(lfe_bits).

-export([parse_bitspecs/1]).

%% The standard imports
-import(lists, [foldl/3]).

%% Everything default'ed.
-record(spec, {type=default,size=default,unit=default,
	       sign=default,endian=default}).

%% parse_bitspecs(Specs) -> {ok,Size,{Type,Unit,Sign,End}} | {error,Error}.
%% Parse a bitspec and return data. The size field is unevaluated. We
%% only return the first error found, it is of type:
%% {illegal_bitspec,Spec}.

parse_bitspecs(Ss) ->
    try
	{Ty,Sz,Un,Si,En} = parse_bitspecs(Ss, #spec{}),
	{ok,Sz,{Ty,Un,Si,En}}
    catch
	throw:Error -> Error
    end.

parse_bitspecs(Ss, Sp0) ->
    Sp1 = foldl(fun (S, Sp) -> parse_bitspec(S, Sp) end, Sp0, Ss),
    %% Adjust the values depending on type and given value.
    #spec{type=Type,size=Size,unit=Unit,sign=Sign,endian=End} = Sp1,
    apply_defaults(Type, Size, Unit, Sign, End).

%% parse_bitspec(Spec, #spec{}) -> #spec{}.
%%  We also convert synonyms to the standard value.

%% Types.
parse_bitspec(integer, Sp) -> Sp#spec{type=integer};
parse_bitspec(float, Sp) -> Sp#spec{type=float};
parse_bitspec(binary, Sp) -> Sp#spec{type=binary,unit=8};
parse_bitspec(bytes, Sp) -> Sp#spec{type=binary,unit=8};
parse_bitspec(bitstring, Sp) -> Sp#spec{type=binary,unit=1};
parse_bitspec(bits, Sp) -> Sp#spec{type=binary,unit=1};
%% Unicode types.
parse_bitspec('utf8', Sp) -> Sp#spec{type=utf8};
parse_bitspec('utf-8', Sp) -> Sp#spec{type=utf8};
parse_bitspec('utf16', Sp) -> Sp#spec{type=utf16};
parse_bitspec('utf-16', Sp) -> Sp#spec{type=utf16};
parse_bitspec('utf32', Sp) -> Sp#spec{type=utf32};
parse_bitspec('utf-32', Sp) -> Sp#spec{type=utf32};
%% Endianness.
parse_bitspec('big', Sp) -> Sp#spec{endian=big};
parse_bitspec('big-endian', Sp) -> Sp#spec{endian=big};
parse_bitspec('little', Sp) -> Sp#spec{endian=little};
parse_bitspec('little-endian', Sp) -> Sp#spec{endian=little};
parse_bitspec('native', Sp) -> Sp#spec{endian=native};
parse_bitspec('native-endian', Sp) -> Sp#spec{endian=native};
%% Sign.
parse_bitspec(signed, Sp) -> Sp#spec{sign=signed};
parse_bitspec(unsigned, Sp) -> Sp#spec{sign=unsigned};
%% Size and unit, return these as is.
parse_bitspec([size,S], Sp) -> Sp#spec{size=S};
parse_bitspec([unit,U], Sp) when is_integer(U), U > 0, U =< 256 ->
    Sp#spec{unit=U};
parse_bitspec(Spec, _) -> throw({error,{undefined_bittype,Spec}}).

%% apply_defaults(Type, Size, Unit, Sign, Endian) ->
%%     {Type,Size,Unit,Sign,Endian}.
%%  This is taken almost directly from erl_bits.erl.

%% Default type.
apply_defaults(default, Sz, Un, Si, En) ->
    apply_defaults(integer, Sz, Un, Si, En);
%% Default size.
apply_defaults(binary, default, Un, Si, En) ->
    apply_defaults(binary, all, Un, Si, En);
apply_defaults(integer, default, Un, Si, En) ->
    check_unit(Un),
    apply_defaults(integer, 8, Un, Si, En);
apply_defaults(utf8=Ty, default, Un, Si, En) ->
    apply_defaults(Ty, undefined, Un, Si, En);
apply_defaults(utf16=Ty, default, Un, Si, En) ->
    apply_defaults(Ty, undefined, Un, Si, En);
apply_defaults(utf32=Ty, default, Un, Si, En) ->
    apply_defaults(Ty, undefined, Un, Si, En);
apply_defaults(float, default, Un, Si, En) ->
    check_unit(Un),
    apply_defaults(float, 64, 1, Si, En);
%% Default unit.
apply_defaults(binary, Sz, default, Si, En) ->
    apply_defaults(binary, Sz, 8, Si, En);
apply_defaults(integer, Sz, default, Si, En) ->
    apply_defaults(integer, Sz, 1, Si, En);
apply_defaults(float, Sz, default, Si, En) ->
    apply_defaults(float, Sz, 1, Si, En);
%% Default sign.
apply_defaults(Ty, Sz, Un, default, En) ->
    apply_defaults(Ty, Sz, Un, unsigned, En);
%% Default endian.
apply_defaults(Ty, Sz, Un, Si, default) ->
    apply_defaults(Ty, Sz, Un, Si, big);
%% Done.
apply_defaults(Ty, Sz, Un, Si, En) ->
    {Ty,Sz,Un,Si,En}.

check_unit(default) -> ok;
check_unit(_) -> throw({error,bittype_unit}).

val_or_def(default, Def) -> Def;
val_or_def(V, _) -> V.
