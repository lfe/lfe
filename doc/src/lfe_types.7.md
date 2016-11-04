% lfe_types(7)
% Robert Virding
% 2016

# NAME

lfe_types - LFE Types and Functions Specifications

# TYPES

This is a description of the type syntax.


  | LFE type                     | Erlang type                  |
  |------------------------------|------------------------------|
  | `(none)`                       | `none()`                       |
  | `(any)`                        | `any()`                        |
  | `(atom)`                       | `atom()`                       |
  | `(integer)`                    | `integer()`                    |
  | `(float)`                      | `float()`                      |
  | `...`                          | `...`                          |
  | `(lambda any <type>)`          | `fun((...) -> <type>)`         |
  | `(lambda () <type>)`           | `fun(() -> <type>)`            |
  | `(lambda (<tlist>) <type>)`    | `fun((<tlist>) -> <type>)`     |
  | `(map)`                        | `map()`                        |
  | `(map <pairlist>)`             | `#{<pairlist>}`                |
  | `(tuple)`                      | `tuple()`                      |
  | `(tuple <tlist>)`              | `{<tlist>}`                    |
  | `(UNION <tlist>)`              | `<type> | <type>`              |

Apart from the predefined types in the Erlang type system we also have
the following predefined types which cannot be redefined: `UNION`,
`call` and `lambda`.

## Type Declarations of User-Defined Types

**(deftype type-name type-def)**

**(defopaque (type-name) type-def)**

**(deftype (type-name par1 par2) type-def)**

**(defopaque (type-name par1 par2) type-def)**

For unparameterised types the parentheses around the type name are optional. An example:

```
(deftype (foo) (tuple 'foo (integer) (list)))
```

## Type Information in Record Declarations

**(defrecord rec (field1 default1 type1) (field2 default2) field3)**

Fields with type annotations *MUST* give a default value and fields
without type annotations get the default type `(any)`.

# SPECIFICATIONS

## Type specifications of User-Defined Functions

**(defspec (func-name arity) function-spec ...)**

where

```
function-spec = ((arg-types) ret-type)
function-spec = ((arg-types) ret-type constraint ...)
constraint = (var var-type)
```

For multiple types add more function specs. For example from the docs:

```
(defspec (foo 1) (((pos_integer)) (pos_integer)))

(defspec (foo 1)
  (((pos_integer)) (pos_integer))
  (((integer) (integer))))
```

Or with constraints:

```
(defspec (id 1) ((X) X (X (tuple))))

(defspec (foo 1)
  (((tuple X (integer))) X (X (atom)))
  (((list Y)) Y (Y (number))))
```

Note that a constraint variable doesn't need to start with an
upper-case like an Erlang variable, though in some case it may be
easier to read.