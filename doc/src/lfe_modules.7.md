% lfe_modules(7)
% Robert Virding
% 2026

# MODULES

## Module Syntax

LFE code is divided into modules. A module consists of a sequence of
attributes and function declarations, each being a valid form.

## Module Attributes

A *module attribute* defines a certain property of a module. A module
attribute consists of a tag and a value. The `tag` is an atom and the
`value` must be a literal term.

Several module attributes have predefined meanings. Some of them have
arity two, but user-defined module attributes must have arity one.

### Pre-Defined Module Attributes

**`(module module-name)`**

Module declaration, defining the name of the module. The name
`module-name`, an atom, is to be same as the file name minus the extension
`.lfe`. Otherwise code loading does not work as intended.

This attribute is to be specified first and is the only mandatory attribute.

**`(export functions)`**

Exported functions. Specifies which of the functions, defined within
the module, that are visible from outside the module.

`functions` is a list `((name-1 arity-1) ... (name-n arity-n))`, where each `name-i` is an atom and `arity-i` an integer. It can also be just the atom `all` which means that all functions are exported.

**`(import module functions)`**

Importing allows us to call "local" functions where the call will be
changed to a call to the function in another module, so the call
`(do-stuff x y)` will become the call `(the-module:do-stuff x y)`.

`functions` is a list `(name-1 name-2 ... name-n)` of the functions
which will be changed to a call to the given module.

**`(rename module renames)`**

Like `import` but it allows us to rename the function being called, so
in this case the call `(funny-name x y)` could become
`(the-module:good-name x y)`.

**`(moduledoc documentation)`**

The user documentation for this module. The allowed values for
`documentation` are the same as for `doc` attribute.

**`(compile options)`**

Compiler options. `options` is a single option or a list of
options. This attribute is added to the option list when compiling the
module. See module compile in Compiler.

**`(vsn vsn)`**

Module version. `vsn` is any literal term and can be retrieved using `beam_lib:version/1`.

If this attribute is not specified, the version defaults to the MD5
checksum of the module.

**`(on_load functions)`**

This attribute names a function that is to be run automatically when a
module is loaded.

**`(nifs functions)`**

Specifies which of the functions, defined within the module, that may
be loaded as NIFs with `erlang:load_nif/2`.

### Behaviour Module Attribute

It is possible to specify that the module is the callback module for a
*behaviour*:

**`(behaviour behaviour)`**

The atom `behaviour` gives the name of the behaviour, which can be a
user-defined behaviour or one of the following OTP standard
behaviours:

```
gen_server
gen_statem
gen_event
supervisor
```

The spelling `behavior` is also accepted.

### Record and Struct definitions

The same syntax as for module attributes is used for record and struct
definitions:

**`(record name fields)`**

**`(struct fields)`**

### Setting File and Line

The same syntax as for module attributes is used for changing the
pre-defined macros `FILE` and `LINE`:

**`(file name line)`**

### Types and function specifications

A similar syntax as for module attributes is used for specifying types
and function specifications:

**`(type type-name type-def)`**

**`(opaque type-name type-def)`**

**`(spec function function-spec)`**

For a description of these see the `lfe-types` documentation.

### Documentation attributes

The module attribute `(doc documentation)` is used to provide user
documentation for a function/type/callback.

```
(doc "Example documentation")
(function example (lambda () 'ok))
```

The attribute should be placed just before the entity it documents.

**`(doc documentation)`**

### LFE specific attributes

**`(alias module-name alias)`**

Provide alias names for modules. The `alias` can be used in code as an
alternative to `module-name`. A typical use is to provide short names for
modules with long names.

**`(module-alias aliases)`**

Provide alias names for modules. The alias can be used in code as an
alternative to modulename. A typical use is to provide short names for
modules with long names. `aliases` is a list:

```((real-module-name-1 alias-1) ... (real-module-name-n alias-n))```.

**`(export-macro macros)`**

Export the macros so they can be callable from other modules.

`macros` is a list `(name-1 ... name-n)`, where each `name-i` is an atom. It can also be just the atom `all` which means that all macros are exported.

### User-defined attributes

User-defined module attributes must have arity one. There are two ways
of creating user-defined attributes, either using the more literal
`attribute` tag or going more Erlangy with `-`.

**`(attribute tag value)`**

**`(- tag value)`**

## Comments

Comments come in two forms: line comments and block comments.

Line comments start with a semicolon ``;`` and finish with the end of the
line.

Block comments are written as ``#| comment text |#`` where the comment text
may span multiple lines but my not contain another block comment,
i.e. it may not contain the character sequence ``#|``.

## Older Forms

```
(defmodule module-name
    attributes)
```
