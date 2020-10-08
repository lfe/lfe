%% Copyright (c) 2020 Robert Virding
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

%% File    : lfe_docs.hrl
%% Author  : Robert Virding
%% Purpose : Common documentation-related definitions.

%% The "Docs" format from EEP 48: Documentation storage and format
%%
%% {docs_v1,
%%  Anno :: erl_anno:anno(),
%%  BeamLanguage :: atom(),
%%  Format :: mime_type(),
%%  ModuleDoc :: #{DocLanguage := DocValue} | none | hidden,
%%  Metadata :: map(),
%%  Docs ::
%%    [{{Kind, Name, Arity},
%%      Anno :: erl_anno:anno(),
%%      Signature :: [binary()],
%%      Doc :: #{DocLanguage := DocValue} | none | hidden,
%%      Metadata :: map()
%%     }]} when DocLanguage :: binary(),
%%              DocValue :: binary() | term()

-define(NATIVE_FORMAT,<<"application/erlang+html">>).
-define(LFE_FORMAT,   <<"text/markdown">>).

-define(CURR_DOC_VERSION, {1,0,0}).

%% The Docs v1 record.
-record(docs_v1, {anno,
                  beam_language,
                  format,
                  module_doc,
                  metadata,
                  docs = []
                 }).

-type docs_v1() :: #docs_v1{}.
