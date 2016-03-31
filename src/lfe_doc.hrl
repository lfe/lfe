%% Copyright (c) 2016 Eric Bailey
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

%% File    : lfe_doc.erl
%% Author  : Eric Bailey
%% Purpose : Common documentation-related definitions.

%% TODO: Actually define this, if possible.
-type pattern() :: [atom() | term()].

%% de{fun,macro} docs.
-record(doc, {type     = error(missing_type)  :: function | macro,
              exported = false                :: boolean(),
              name     = error(missing_name)  :: atom(),
              arity    = error(missing_arity) :: non_neg_integer(),
              patterns = [[]]                 :: [pattern()],
              doc      = <<"">>               :: binary()
             }).

-type doc() :: #doc{}.

%% For the BEAM beam chunk, "LDoc".
-record(lfe_docs_v1, {docs=[] :: [doc()]        %Function/macro docs
                      %% moduledoc=ModuleDoc         %Module doc
                      %% callback_docs=CallbackDocs, %Callback docs
                      %% type_docs=TypeDocs          %Type docs
                     }).
