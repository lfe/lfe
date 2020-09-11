%% Copyright (c) 2014-2020 Robert Virding
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% File    : lfe.hrl
%% Author  : Robert Virding
%% Purpose : Common definitions.

%% We do a lot of quoting!
-define(Q(E), [quote,E]).
-define(BQ(E), [backquote,E]).
-define(C(E), [comma,E]).
-define(C_A(E), ['comma-at',E]).

%% Some commonly used macros.

%% Define IS_MAP/1 macro for is_map/1 bif.
-ifdef(HAS_MAPS).
-define(IS_MAP(T), is_map(T)).
-else.
-define(IS_MAP(T), false).
-endif.

%% Define CATCH to handle deprecated get_stacktrace/0
-ifdef(NEW_STACKTRACE).
-define(CATCH(C, E, S), C:E:S ->).
-else.
-define(CATCH(C, E, S), C:E -> S = erlang:get_stacktrace(),).
-endif.

%% Bloody useful
-define(IF(Test,True,False), case Test of true -> True; false -> False end).
