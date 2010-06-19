-module(test_rec_e).

-compile(export_all).

-record(point, {x=0,y=element(3, now())}).

a(X, Y, R) ->
    [#point{x=now()},
     #point{x= <<34,X/float,(Y+3)/float>>}].

b(X, Y, R) ->
    #point{x=Xx,y=Yy} = #point{x=X},
    [Xx,Yy,#point{y=R}].

-record(circle,{center=#point{},radius=0}).

c(X, Y, R) ->
    C = #point{x=42},
    #circle{center=C,radius=R}.
