-module(test_pat_e).

-export([a/2]).

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
