%% File for testing expansion of type declarations.

-export_type([o1/1]).

%% Some normal attributes.

-dummy(foo).
-dummystr("abc").
-dummyier([foo]).
-dummyiest([foo,bar,baz]).

%% Some records.

-record(r1, {a=hello,b}).
-record(r2, {a=hello :: atom(),b :: #r1{},c}).

%% Defining types 

-type t1() :: list(integer()).

-type integer(X) :: {not_integer,atom(),X}.

-opaque o1(X) :: {not_atom,integer(),integer(X)}.

-type t3(X,Y) :: {not_atom,bert:integer(),bert:integer(X),bert:integer(X, Y)}.

-type t4() :: atom() | pid() | 42.

-type t5() :: #r1{}.

-type t6() :: #r1{a :: integer(),b :: list()}.

-type t7() :: fun((atom(),atom()) -> {t7,integer()}).

-type t8() :: fun((...) -> t8).                 %Any arity

-type t9() :: #{}.                              %Empty map

-type t10() :: #{integer() => atom(), atom() := pid()}.

-type t11() :: unicode:unicode_binary().        %UTF-8 string

-type t12(X) :: {X, _}.

%% Binary type declarations.

-type t13() :: binary().

-type t14() :: bitstring().

-type t15() :: <<>>.

-type t16() :: <<_:256>>.

-type t17() :: <<_:_*256>>.

-type t18() :: <<_:42,_:_*84>>.

%% Range type definitions.

-type t20() :: 1..42.

%% Bad type defs.
%% -type t10() :: ok.                              %Redefining t10
%% -type integer() :: {integer,atom()}.            %Redefining type integer()
%% -type it1(X) :: {not_atom,integer(),X,Y}.       %Singleton type var Y
%% -type it2() :: sune().                          %Unknown type sune()
%% -type it3(A, 1) :: list(A).                     %Bad parameter list
%% -type it4(A, B) :: X.                           %Singleton type vars A, B, X
%% -type t19() :: <<_>>.                           %Illegal binary format
