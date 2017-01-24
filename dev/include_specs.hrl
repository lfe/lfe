%% File for testing expansion of function specifications.
%% We specify functions a/0, b/1, c/2, d/2.

-type st() :: {st,list(),list()}.

-spec s1() -> integer().

-spec s2(any()) -> 42.

-spec s3(atom(), integer()) -> string().

-spec s4(atom(), integer()) -> string() ;
        (integer(), atom()) -> st().

-spec s5(X, Y) -> {ok,Z} when X :: integer(),
                              Y :: atom(),
                              Z :: any().

-spec s6_1(fun((any()) -> boolean()), list()) -> list().

-spec s6_2(Pred, list()) -> list() when Pred :: fun((any()) -> boolean()).

%% Bad func specs.
%%-spec s2(any()) -> any().                       %Respecing b/1
%%-spec is2(X, Y) -> Z when X :: integer().       %Undefined constraints Y,Z
%%-spec is3(X) -> Y when X :: atom(), Y :: atom(), Z :: integer();
%%         (A) -> B when A :: integer().
