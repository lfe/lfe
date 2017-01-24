-module(specs_test).

-export([s1/0,s2/1,s3/2,s4/2,s5/2,s6_1/2,s6_2/2]).
-export([is3/1]).

-include("include_specs.hrl").

s1() -> 42.

s2(_) -> 42.

s3(a, 19) -> "hej".

s4(18, y) -> {ok,"123"}.

s5(_, _) -> 19.

s6_1(_, _) -> 23.

s6_2(_, _) -> 23.

is3(1) -> 1.
