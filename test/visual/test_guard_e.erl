-module(test_guard_e).

-compile([export_all]).

seq(X, Y, Z) when X > 0, is_integer(Z) -> 1;
seq(X, Y, Z) when X > 0, element(Y, Z) =:= 10 ->
    2;
seq(X, Y, Z) when X > 0, element(Y, Z) =:= 10, element(Y+1, Z) =:= 10 ->
    3;
seq(X, Y, Z) when (X > 0) and ((element(Y, Z) =:= 10) and (element(Y+1, Z) =:= 10)) ->
    4;
seq(X, Y, Z) -> 999.

lit(X, Y, Z) when true -> 1;
lit(X, Y, Z) when false -> 2;
lit(X, Y, Z) when 'X', 'Y' -> 3;
lit(X, Y, Z) when (X and Y) -> 4;
lit(X, Y, Z) when 67, 'Z' -> 5;
lit(X, Y, Z) when 67 and 'Z' -> 6;
lit(X, Y, Z) when 'X', 89, 'Z' -> 7;
lit(X, Y, Z) when X, 89, {'Z'} -> 8;
lit(X, Y, Z) when X and (89 and Z) -> 9;
lit(X, Y, Z) -> 999.
