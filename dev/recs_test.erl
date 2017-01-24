-module(recs_test).

-export([r1/0,r2/0,r3/1,r4/1]).

-include("include_recs.hrl").

r1() -> #urec{}.

r2() -> #trec{}.

%% Some simple access functions.
r3(U) -> U#urec.b.

r4(T) -> T#urec{b=foo:bar(1),c=zip:zap(42)}.
