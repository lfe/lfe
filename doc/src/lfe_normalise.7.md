% lfe_normalise(7)
% Robert Virding
% 2024

# NAME

lfe_normalise - LFE normalised internal compiler format

# SYNOPSIS

This in an internal module of the compiler which converts the input forms after macro expansion for to a set of normalised forms, ``norms``, which are used internally in the compiler. These are generally not directly used by the user.

# NORMALISED FORMS

These are defined normalised forms.

## Predefined attributes

**`(module line-number module-name)`**

**`(export line-number exported-functions | all)`**

**`(import line-number imports)`**

**`(rename line-number renames)`**

**`(moduledoc line-number docs)`**

**`(compile line-number options)`**

**`(vsn line-number vsn)`**

**`(on_load line-number function)`**

**`(nifs functions)`**

**`(alias line-number module alias)`**

**`(type line-number type type-def)`**

**`(opaque line-number type type-def)`**

**`(export-type line-number types)`**

**`(alias line-number aliases)`**

**`(module-alias line-number aliases)`**

**`(record line-number record-name record-fields)`**

**`(struct line-number struct-fields)`**

**`(spec line-number spec-function specs)`**       spec-function -> (name arity)

**`(macro line-number macro-name macro-def)`**

**`(function line-number function-name function-def)`**

## General attribute

**`(attribute line-number name value)`**           attribute name -> atom

**`(- line-number name value)`**                   attribute name -> atom

Basically everything which isn't a predefined form becomes an attribute.

# USE IN CODE

Norms written without the line number can actually be used in code. So for example we could write the code

```
(defmodule this-module
  (export (a 0) (c 3))
  )
```
as

```
(module this-module)

(export ((a 0) (c 3)))
```
