-module(test_map_e).

-compile(export_all).

literal() ->
    #{a => 1, b => [2,3,4], c => {a,b}}.

make(X,Y,Z) ->
    {#{a => X, b => Y, c => {ok,test_map_e:foo(Z)}},
     #{b => Y, c => {ok,test_map_e:foo(Z)}, a => X},
     #{<<1,2,3>> => X},
     #{<<1:16,"åäö"/utf8>> => Y}}.

lit_make(1) -> #{};
lit_make(2) -> #{a => 1};
lit_make(3) -> #{a => 1, b => 2};
lit_make(4) -> #{a => {1,2}};
lit_make(5) -> #{{1,2} => a}.

set(Map, V1, V2) ->
    Map#{a => V1, b => test_map_e:foo(V2)}.

update(Map, V1, V2) ->
    Map#{a := V1, b := test_map_e:foo(V2)}.

mixed(Map, V1, V2) ->
    Map#{a => V1, b := test_map_e:foo(V2), c => 3}.

guard(Map, X) when #{a=>X} == Map -> 1;
guard(Map, X) when Map#{a=>1} == X -> 2;
guard(Map, X) when Map#{a:=1} == X -> 3.

match(#{a := 1, b := Y}) -> Y;
match(#{a := X}) -> X;
match(#{c := {X,Y}}) -> {X,Y};
match(#{{d,e} := Z}) -> Z.

lit_match(#{a := 1, b := 2}) -> 1;
lit_match(#{a := 1}) -> 2;
lit_match(#{a := {1,2}}) -> 3;
lit_match(#{{1,2} := z}) -> 4;
lit_match(#{}) -> 5.                            %Catch-all

foo(X) ->
    [X,X].
