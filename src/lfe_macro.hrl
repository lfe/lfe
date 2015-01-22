%% Copyright (c) 2013 Robert Virding
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

%% File    : lfe_macro.erl
%% Author  : Robert Virding
%% Purpose : Lisp Flavoured Erlang macro expander.

%% We do a lot of quoting!
-define(Q(E), [quote,E]).
-define(BQ(E), [backquote,E]).
-define(UQ(E), [unquote,E]).
-define(UQ_S(E), ['unquote-splicing',E]).

%% Macro expander state.
-record(mac, {expand=true,          %Expand everything
              module='-no-module',  %Current module
              line=1,               %Line no of current form
              vc=0,                 %Variable counter
              fc=0,                 %Function counter
              file=[],              %File name
              opts=[],              %Compiler options
              ipath=[],             %Include path
              errors=[],            %Errors
              warnings=[]           %Warnings
              }).
