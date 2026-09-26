% lfe_lfe(3)
% Robert Virding
% 2026

# NAME

lfe_lfe - Predefined LFE Lisp functions

# SYNOPSIS

This module provides a set of standard lisp functions which are included in the LFE system. These functions are predefined so there is no need to provide the `lfe` module when they are used.

# EXPORTS

## The standard association list functions.

**`(acons key value list)`**

**`(pairlis keys values {{list}})`**

**`(assoc key list)`**

**`(assoc-if test list)`**

**`(assoc-if-not test list)`**

**`(rassoc value list)`**

**`(rassoc-if test list)`**

**`(rassoc-if-not test list)`**

## The standard substitution functions.

**`(subst new old tree)`**

**`(subst-if new test tree)`**

**`(subst-if-not new test tree)`**

**`(sublis alist tree)`**

## Macro expansion functions

These are functions for explicitly expanding macros in an
expression. If an optional environmant is not included then only the
default built-in macros will be expanded.

**`(macroexpand-1 expr {{environment}})`**

If the top-level of `expr` is a macro call then one round of expansion
will be done, otherwise `expr` will be returned.

**`(macroexpand expr {{environment}})`**

If the top-level of `expr` is a macro call then that macro will be
repeatedly expanded until it is no longer a macro call, otherwise
`expr` will be returned.


**`(macroexpand-all expr {{environment}})`**

Returns the expansion from the expression `expr` where all macro calls
have been expanded with `macroexpand`.

## Evaluate expressions

**`(eval expr {{envionment}})`**

Evaluate the expression `expr`. Note that only the pre-defined
lisp functions, erlang BIFs and exported functions can be
called. Also no local variables can be accessed. To access
local variables the expr to be evaluated can be wrapped in a
`let` defining these.

For example if the data we wish to evaluate is in the variable
`expr` and it assumes there is a local variable `foo` which it
needs to access then we could evaluate it by calling:

```
(eval `(let ((foo ,foo)) ,expr))
```

## Current LFE version

**`(version)`**

Returns the current LFE version as a string.
