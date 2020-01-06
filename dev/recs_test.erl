-module(recs_test).

-compile([export_all]).

%% Defines records #urec{a,b,c}, #trec{a,b,c}.
-include("include_recs.hrl").

%% Make records functions.
r1() -> #urec{}.

r2(B) -> #trec{b=foo:bar(B)}.

%% Some access functions.
r3(U) -> U#urec.b.

r4(T) -> T#urec{b=foo:bar(1)}.			%Setting one

r5(T) -> T#urec{b=foo:bar(1),c=zip:zap(42)}.	%Setting some

%% Some access functions for a big record.

-record(brec, {a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z}).

r6() -> #brec{}.

r7(T) -> T#brec.m.

r8(T) -> T#brec{l=foo:bar(1)}.

r9(T) -> T#brec{j=foo:bar(1),p=zip:zap(42)}.

r10(T) -> T#brec{a=foo:bar(2),j=foo:bar(1),p=zip:zap(42),z=zip:zap(1)}.
