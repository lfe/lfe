-module(test_pat_e).

-export([a/2,b/2,c/1,e/2,g/2]).

a(1,_) ->
    F = fun ([1,_]) -> 1;
            ([a,4]) -> {1,a,4}
        end,
    {1,F};
a(X, Y) ->
    case [X,Y] of
        [a,4] -> {1,a,4};
        [a,_]=[P1|Ps] -> {2,a,P1,Ps};
        [_,_] -> {3,anything}
    end.

b(a, 4) -> {1,a,4};
b(a, _) -> {2,a,anything};
b(_, _) -> {3,anything}.

c(X) ->
    case X of
        "1234" -> string;
        _ -> other
    end.

e(X, Y) ->
    case [X|Y] of
        [a,M] -> {1,old,a,M};
        [M|N] -> {2,old,M,N};
        [M,N|O] -> {3,old,M,N,O};
        [M|[N|O]] -> {4,old,M,N,O};
        [M|[N,O]] -> {5,old,M,N,O}
    end.

g(X, Y) ->
    case [X|Y] of
        [a,M]=[a,M] -> {1,'old/old',M};
        [M|N]=[N|M] -> {2,'old/old',M,N};
        [M1,N1|O1]=[M2,N2|O2] -> {3,'old/old',M1,N1,O1}
    end.
