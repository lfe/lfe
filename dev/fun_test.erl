%% Test generation and calling funs.

-module(fun_test).

-export([a/3,b/3]).

a(X, Y, Z) -> [fun local_1/1, fun local_2/2(Y, Z)].

b(X, Y, Z) -> fun a:b/3.			%All literals

%% Error cases.

%% c(X, Y, Z) -> F = local_3, Ar = 3, fun F/Ar.

%% d(X, Y, Z) -> fun local/3.

%% e(X, Y, Z) -> fun X:Y/-4.

local_1(A) -> A.

local_2(A, B) -> {A,B}.

%% local_3(A, B, C) -> {A,B,C}.
