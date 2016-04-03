% lfe_doc(3)
% Eric Bailey
% 2016


# NAME

lfe_doc - Lisp Flavoured Erlang (LFE) documentation parser.


# SYNOPSIS

This module provides functions to parse docstrings in LFE module sources.

The functions herein are used internally by the compiler.
There is no guarantee the API will not change dramatically in future.

an interface to the standard LFE
docuementation . The compiler can handle files which contain multiple
modules. It can generate either new files which contain the
object code, or return binaries which can be loaded directly.


# EXPORTS

**module(Mod) -> Mod | {error,Errors,[]}**

Parse a module's docstrings and populate Mod#module.docs.

**patterns(LambdaForm) -> 'no' | {'yes',Arity,Patterns}**

Given a ``{match-,}lambda`` form, attempt to return its patterns (or arglist).

N.B. A guard is appended to its pattern and ``Patterns`` is a list of lists.

**add_docs_module(Mod) -> Mod**

Add the "LDoc" chunk to a module's .beam binary.


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
