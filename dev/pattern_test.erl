-module(pattern_test).

-compile([export_all]).

%% Testing multiple recurring variables.

%% Testing with cons and list.
rv(X, [X|Xs]) -> [c_1,X,Xs];
rv([X|_], [X|Xs]) -> [c_2,X,Xs];
rv([X|_], [X,_]) -> [c_3,X];
rv([X,X], [X,Y]) -> [c_4,X,Y];
%% Testing with tuples.
rv(X, {X,Y,Z}) -> [t_1,X,Y,Z];
rv({X,_,_}, {X,Y,Z}) -> [t_2,X,Y,Z];
rv({_,X,X}, {X,Y,Z}) -> [t_3,X,Y,Z];
%% Catch all.
rv(X, Y) -> [last,X,Y].

%% Testing aliases that match.

alias(X) ->
    case X of
	[A,B] = [H|T] -> [c_1,A,B,H,T];
	Other -> [last,Other]
    end.
