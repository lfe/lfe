%% Copyright (c) 2014-2015 Robert Virding
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

%% File    : lfe_comp.hrl
%% Author  : Robert Virding
%% Purpose : Common compiler definitions.

%% Common compiler information

-record(cinfo, {file=[],                        %File name
                opts=[],                        %Compiler options
                ipath=[],                       %Include path
                mod=none                        %Module name
               }).

-record(module, {name=[],                       %Module name
                 code=none,                     %Module code
                 warnings=[],                   %Module warnings
                 docs=[]                        %Module docs
                }).

%% Bloody useful
-define(IF(Test,True,False), case Test of true -> True; false -> False end).

%% ?WHEN_OPT(Option, Options, Fun) -> ok.
%% ?UNLESS_OPT(Option, Options, Fun) -> ok.
%%  Call Fun when Option is/is not a member of Options.

-define(WHEN_OPT(Opt,Opts,Fun), ?IF(member(Opt, Opts), Fun(), ok)).

%% -define(UNLESS_OPT(Opt,Opts,Fun), ?IF(member(Opt, Opts), ok, Fun())).

-define(DEBUG(Format,Args,Opts),
        ?WHEN_OPT(debug_print, Opts,
                  fun () -> lfe_io:format(Format, Args) end)).
