%% Copyright (c) 2011-2013 Robert Virding
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

%%% File    : lfe_bits.erl
%%% Author  : Robert Virding
%%% Purpose : Lisp Flavoured Erlang common functions for binaries.

-module(lfe_bits).

-export([parse_bitspecs/1,get_bitspecs/1]).

%% The standard imports
-import(lists, [foldl/3]).

%% Everything default'ed.
-record(spec, {type=default,size=default,unit=default,
               sign=default,endian=default}).

%% get_bitspecs(Specs) -> {ok,Size,{Type,Unit,Sign,End}} | {error,Error}.
%%  Parse a bitspec, apply defaults and return data. The size field is
%%  unevaluated. We only return the first error found.

get_bitspecs(Specs) ->
    try
        #spec{type=Ty0,size=Sz0,unit=Un0,sign=Si0,endian=En0} =
            parse_bitspecs(Specs, #spec{}),
        {Ty,Sz,Un,Si,En} = apply_defaults(Ty0, Sz0, Un0, Si0, En0),
        {ok,Sz,{Ty,Un,Si,En}}
    catch
        throw:Error -> Error
    end.

%% parse_bitspecs(Specs) -> {ok,Size,{Type,Unit,Sign,End}} | {error,Error}.
%%  Parse a bitspec and return data. Unmentioned fields get the value
%%  default. We only return the first error found.

parse_bitspecs(Specs) ->
    case catch parse_bitspecs(Specs, #spec{}) of
        #spec{type=Ty,size=Sz,unit=Un,sign=Si,endian=En} ->
            {ok,Sz,{Ty,Un,Si,En}};
        Error -> Error
    end.

%% parse_bitspecs(Specs, #spec{}) -> #spec{}.
%%  Parse a bitspec and return a #spec{} record. Unmentioned fields
%%  get the value default. Errors throw the tuple {error,Error} and
%%  must be caught.

parse_bitspecs(Ss, Sp0) ->
    foldl(fun (S, Sp) -> parse_bitspec(S, Sp) end, Sp0, Ss).

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
apply_defaults(utf8, default, Un, Si, En) ->
    apply_defaults(utf8, undefined, Un, Si, En);
apply_defaults(utf16, default, Un, Si, En) ->
    apply_defaults(utf16, undefined, Un, Si, En);
apply_defaults(utf32, default, Un, Si, En) ->
    apply_defaults(utf32, undefined, Un, Si, En);
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
