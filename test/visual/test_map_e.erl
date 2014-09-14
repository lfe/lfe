-module(test_map_e).

-compile(export_all).

make(X,Y,Z) ->
    {#{a => X, b => Y, c => {ok,test_map_e:foo(Z)}},
     #{b => Y, c => {ok,test_map_e:foo(Z)}, a => X},
     #{<<1,2,3>> => X},
     #{<<1:16,"åäö"/utf8>> => Y}}.

set(Map, V1, V2) ->
    M = Map#{a => V1, b => test_map_e:foo(V2)}.

update(Map, V1, V2) ->
    M = Map#{a := V1, b := test_map_e:foo(V2)}.

mixed(Map, V1, V2) ->
    M = Map#{a => V1, b := test_map_e:foo(V2), c => 3}.

guard(Map, X) when #{a=>X} == Map -> 1;
guard(Map, X) when Map#{a=>1} == X -> 2;
guard(Map, X) when Map#{a:=1} == X -> 3.

match(#{a := X}) -> X;
match(#{a := 1, b := Y}) -> Y;
match(#{c := {X,Y}}) -> {X,Y};
match(#{{d,e} := Z}) -> Z.

foo(X) ->
    [X,X].
