-module(test_lc_e).

-compile([export_all]).

a(X, Y) ->
    [ V || V <- X,
           V /= Y ].

c(X, Y) ->
    [ {B,V} || <<B/float>> <= Y, {V} <- X ].

d(X, Y) ->
    << <<(B*V):16>> || <<B>> <= Y, {V} <- X >>.

%% d(X, Y) ->
%%     << <<(B*V)/float>> || <<B/float>> <= Y, {V} <- X >>.
