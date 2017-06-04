-module(record_test).

-compile(export_all).

-record(point, {x=0,y=element(3, date())}).

a(X, Y, R) ->
    [#point{x=date()},
     #point{x= <<34,X/float,(Y+3)/float>>}].

b(X, Y, R) ->
    #point{x=Xx,y=Yy} = #point{x=X},
    [Xx,Yy,#point{y=R}].

-record(circle,{center=#point{},radius=0}).

c(X, Y, R) ->
    C = #point{x=42},
    #circle{center=C,radius=R}.

d(P, C) ->
    {P#point.x,C#circle.radius}.

e(P, C) ->
    {case P of
         #point{x=X} -> X;
         _ -> error({badrecord,point})
     end,
     case C of
         #circle{radius=Radius} -> Radius;
         _ -> error({badrecord,circle})
     end}.
