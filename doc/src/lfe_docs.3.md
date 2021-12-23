% lfe_docs(3)
% Robert Virding
% 2016


# NAME

lfe_docs - Lisp Flavoured Erlang (LFE) documentation handling.


# SYNOPSIS

This module provides functions to parse docstrings in LFE module
sources in EEP48 format.

# EXPORTS

**make_chunk(Mod, CompilerInfo) -> {ok,DocsChunk}**

Parse a module's docstrings and return a documentation chunk.

**make_docs_info(Mod, CompilerInfo) -> {ok,DocsInfo}**

Parse a module's docstrings and return the documentation info.

**get_module_docs(Module | Binary) -> {ok,DocsInfo} | {error,Error}**

Extract the documentation from a module documentation chunk and return
it in the documentation format of the current Erlang version.

# SEE ALSO

**lfe_comp(3)**, **lfe_macro(3)**
