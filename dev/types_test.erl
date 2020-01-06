-module(types_test).

-export([a/0]).

%%-type({sune,bad,type}).                       %Sneaky non-typedef attribute

-include("include_types.hrl").

%%-type({bert,{type,union,[yes,no]},calle}).    %Sneaky non-typedef attribute

a() -> 42.
