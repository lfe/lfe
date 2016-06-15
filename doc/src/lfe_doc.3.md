% lfe_doc(3)
% Eric Bailey
% 2016


# NAME

lfe_doc - Lisp Flavoured Erlang (LFE) documentation parser.


# SYNOPSIS

This module provides functions to parse docstrings in LFE module sources.

There is no guarantee that the internal formats will not change but
the interface functions should stay the same.

# EXPORTS

**extract_module_docs(Mod, CompilerInfo) -> {ok,ModDocs} | {error,Errors,[]}**

Parse a module's docstrings and return module documentation structure.

**save_module_docs(Beam, ModDocs, CompilerInfo) -> Beam**

Add the "LDoc" chunk containing ModDocs to a module's .beam binary.

**get_module_docs(Module | Binary) -> {ok,DocChunk} | {error,Error}**

Extract the documentation chunk from a module. The chunk will be
converted to an internal format.

**module_doc(DocChunk) -> [DocString]**

**mf_docs(DocChunk) -> [MacFuncDoc]**

**mf_doc_type(MacFuncDoc) -> function | macro**

**function_docs(DocChunk) -> [FuncDoc]**

**macro_docs(DocChunk) -> [MacDoc]**

Extract fields from the module documentation chunk.

**function_name(FuncDoc) -> Name**

**function_arity(FuncDoc) -> Arity**

**function_line(FuncDoc) -> Line**

**function_patterns(FunDoc) -> [Pattern]**

**function_doc(FuncDoc) -> [DocString]**

Extract fields from a function documentation structure.

**macro_name(MacDoc) -> Name**

**macro_line(MacDoc) -> Line**

**macro_patterns(FunDoc) -> [Pattern]**

**macro_doc(MacDoc) -> [DocString]**

Extract fields from a macro documentation structure.

# ERROR INFORMATION

The ``Errors`` mentioned above is a list of standard ``ErrorInfo`` structures.
It has the following format:

**{ErrorLine,Module,ErrorDescriptor}**

A string describing the error is obtained with the following call:

```
Module:format_error(ErrorDescriptor)
```


# SEE ALSO

**lfe_comp(3)**, **lfe_macro(3)**
