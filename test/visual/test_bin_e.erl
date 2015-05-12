%%% File    : test_bin_e.erl
%%% Author  : Robert Virding
%%% Purpose : Test binaries.

-module(test_bin_e).

-export([a/0,a/1,af/2,afp/2,a/3]).              %Constructors
-export([p1/1,p2/1,p2p/1,p3/1,p4/0]).           %Patterns
-export([b/1,b/2,bb1/2,bb2/2]).                 %Binaries/bitstrings
-export([u/1,u/2]).                             %Unicode types
-export([vs1/2,vs2/2,vs3/2]).                   %Value and size expressions
-export([d1/0,d2/0,d3/0]).                      %Binary constants
-export([sl1/0,sl1/1]).                         %String literals

%% Binary constructors.

a() -> <<1,2,3>>.

a(X) -> <<X:24>>.

af(X, Y) -> <<X:32/float,Y:64/float>>.

afp(X, Y) -> <<X:32/float,Y:40/float>>.         %This will cause an error!

a(X, Y, Z) -> <<X/unsigned,Y:16/big,Z:3/little>>.

%% Patterns.

p1(B) ->
    case B of
        <<X:24,ZZ/binary>> -> [X,ZZ]
    end.

p2(B) ->
    case B of
        <<X:32/float,Y:64/float,ZZ/bitstring>> -> [X,Y,ZZ]
    end.

p2p(B) ->
    case B of                                   %This will cause an error!
        <<X:32/float,Y:40/float,ZZ/bitstring>> -> [X,Y,ZZ]
    end.

p3(B) ->
    case B of
        <<X/unsigned,Y:16/big,Z:3/little,ZZ/bitstring>> ->
            [X,Y,Z,ZZ]
    end.

p4() ->
    Bin = <<2,"AB","CD">>,
    Tup = {2,<<"AB">>,<<"CD">>},
    <<S,B:S/binary,Rest/binary>> = Bin,
    %% Test equality
    (Tup =:= {S,B,Rest}) and (Tup =:= p4_1(<<2,"AB","CD">>)).

p4_1(<<S,B:S/binary,Rest/binary>>) ->
    {S,B,Rest}.

%% Binaries/bitstrings.

b(Bin) ->
    <<Bin/binary,Bin:16/bitstring,Bin/binary>>.

b(B1, B2) ->
    <<B1/bitstring,B2:3/bitstring-signed,B2/bitstring>>.

bb1(B, N) ->
    case B of
    <<B1:N/binary,B2:16/bitstring,B3/binary>> ->
        [B1,B2,B3]
    end.

bb2(B, N) ->
    case B of
        <<B1:N/bitstring,B2:3/bitstring-signed,B3/bitstring>> ->
            [B1,B2,B3]
    end.

%% Unicode types.

u(X) ->
    <<X/utf8,X/utf16,X/utf32>>.

u(X, Y) ->
    <<X/utf8-big,Y/utf32-little,X/utf16-little-signed>>.

%% Value and size expressions

vs1(X, Y) ->
    Y1 = Y+1,
    {<<X:Y,Y:Y1>>,<<X:Y,Y:(Y+1)>>}.

vs2(X, Y) ->
    {<<(2*X):Y>>,                               %Just value expr
     <<X:(Y+8)>>,                               %Just size expr
     <<(2*X):(Y+8)>>                            %Both value and size expr
    }.

vs3(X, Y) ->
    <<(2*X):Y,                                  %Just value expr
      X:(Y+8),                                  %Just size expr
      (2*X):(Y+8)>>.                            %Both value and size expr

%% Binary constants

d1() -> <<1,2,3>>.

d2() -> <<1.5/float,2.0:32/float,3.0/float-little>>.

d3() -> <<1,2,3>>.

%% String literals.

sl1() ->
    <<"abc","едц">>.

sl1(Bin) ->
    <<"abc",Rest/binary>> = Bin,
    Rest.
