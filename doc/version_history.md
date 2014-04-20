# LFE Version History

## v0.8

A lot of stuff. Amongst other things:

Can define functions and macros in the shell.

Improved reading of Erlang .hrl files, for example can read the
eunit.hrl file.

Can now call functions in other modules using the syntax

``(foo:bar 1 2 3)``

The older syntax ``(: foo bar 1 2 3)`` is still valid.

Now support unnested block comments of the form ``#| ... |#``.

Many new examples.

## v0.7

Added Travis-CI support. You can use this ``.travis.yml`` and add lfe to
http://travis-ci.org. (Ward Bekker)

First version of Query List Comprehensions.

Add access to current macro environment through the variable ``$ENV``
within macros which allows explicit macro expansion.

First version of match-specification generator.

Add ``ets/mnesia`` match patterns to records.

Arithmetic functions ``+`` ``-`` ``*`` ``/`` and comparison functions
``>`` ``>=`` ``<`` ``=<`` ``==`` ``/=`` ``=:=`` ``=/=`` now
take multiple arguments. This is experimental and as yet only implemented as
macros.

Handle compiler option ``warnings_as_errors`` in the same way as the
vanilla Erlang compiler.

Improved ``++`` macro; it can now be used in patterns.

List/binary comprehensions generate fewer unnecessary compiler
warnings.

Added new example file with core macros implemented in LFE.

New patterns with explicit constructors! We now follow the invariant
that constructors and patterns should look the same. It is still
possible to use old patterns but ``cons`` and ``list`` are now "reserved
words" like ``tuple`` and ``binary``.

Allow guards with generators in list/binary comprehensions. N.B. can
cause unexpected behaviour with binary generators.

## v0.6

Allow literal strings in binaries, both as plain values and as values
with specs, so ``(binary "abc" ("åäö" utf-8))`` is valid. In the second
case the spec is applied to each character. Works with lists as well.
Add big, little and native as synonyms to big-endian, little-endian
and native-endian.

Now have working ``Makefile``, ``Emakefile`` and ``.app`` file.

Guards are now a sequence of tests, ``(when test test ...)``. The
structure of guard tests has been fixed and is now more logical as
tests. For example ``(if ...)`` is now allowed.

Add temporary fix to ensure that guards are compiled correctly. It
is less efficient but should be correct. It will be removed when no
longer necessary.

Read and output based integers.

Improve prettyprinting of ``defuns``.

Many internal improvements.

## v0.5

Added macro ``list*``.

Added a new shell command to set variables in the shell.

Cleaned up compiler options and made them more like the vanilla
compiler.

Added unicode types to binaries: utf-8, utf-16 and utf-32.

Shell and compiler now print error data in LFE instead of vanilla.
Shell error reporting is still pretty basic, not as "beautiful" as
vanilla.

Cleaned up i/o functions and added formatted output function. It still
uses vanilla command characters.

All i/o goes through the module ``lfe_io```. The functions which return
the corresponding printable string end in 1. So ``lfe_io:format`` prints
formatted output while ``lfe_io:format1`` returns the string.

Improved pretty-printing.

Many internal improvements.

## v0.4

This will be the last development version for Erlang R12B-5 and older,
all future development will for R13B. If there is enough interest I
may start a separate branch for R12B.

Parameterized modules.

Added ``(export all)`` attribute to module definition.

Added new records which allow giving default values as in vanilla Erlang.
Records are still compatible with vanilla Erlang but now more pratical
to use. NOTE this change is not backwards compatible as syntax for
``(make- ...)`` and ``(match- ...)`` have changed. Also added general
multiple ``(set- ...)`` macro.

``(eval-when-compile ...)`` added as a top-level form which allows
functions to be defined when compiling the forms. These are useful for
more complex macros.

Better and more documention. The documentation is still normal text
files as Edoc and are not in agreement on how things should work.

## v0.3

This is the first version with the modified internal core forms and
macro intefaces for the new CL-inspired style and the older Scheme-inspired
style.

Two new modules have been added:

``lfe_boot`` allows you start Erlang with the LFE shell running and still
have ^G enabled and ``user_drv`` running. Use it as follows:

``
erl -noshell -noinput -s lfe_boot start
``

NOTE order of commands important, must be ``-noshell -noinput``! Add
``-pa`` to find modules if necessary.

``lfe_gen`` is a trial interface for using LFE for dynamic code
generation. LFE is much easier to generate as an Erkang list than
Erlang forms. This module helps with defining and compiling a module. Note,
that while it works, this module is very experimental and may change.

## v0.2

The really *BIG* change is that LFE is now Lisp-2 like Common Lisp,
i.e. functions and variables reside in different name spaces,
instead being a Lisp-1 like Scheme where they reside in the same
name space. The reason for this change is that the ErlangVM does
keep variables and functions separate and while Core Erlang tries to
hide this fact it does not fully succeed. In fact, it is actually
impossible to do this given Erlang's property of being able to have
many functions of the same name but with different arites.

While this is not as elegant and forces the use of funcall to call
functions bound to variables it works better.

It is not an irrevocable change but I would need really convincing
arguments to change it back.

Being a Lisp-2 has introduced some new core forms to handle them:
``flet``, ``flet*``, ``fletrec`` and ``funcall``. ``letrec`` has been
removed.

The handling of macros has been cleaned up.

Schemes, R5RS, handling of ellipsis '...' in syntax-rules has been
added. This really simplifies writing some macros in a elegant way.

The interpreter, ``lfe_eval``, can now handle the complete language.

In patterns both tuples and binaries/bitstrings use the general
constructor form, the constant form now only matches a literal tuple
or binary. For example:

```cl

  (tuple 'ok a b)
  ; this is eqivalent to {ok,A,B}

  #('ok a b)
  ; this is eqivalent to {[quote,ok],a,b}

  (binary (f float (size 32)) (rest binary))
  ; this is eqivalent to <<F:32/float,Rest:binary>>

```

Even though this may be longer and, in some cases, more difficult to
"see" I prefer it as it is consistent.

Patterns can now have aliases, ``(= pat-1 pat-2)``. These are checked in
the linter and non-matching patterns are classed as an error. In
future releases they should become warnings.

There is now an LFE shell which evaluates expressions and prints the
results. It is still a bit primitive and doesn't use processes as it
should in the same manner as the standard Erlang shell does. But it
does have one very nice feature, you can slurp in a source file and
run evaluate all the functions in the shell. Any macros defined in
the file are also available.

It is not yet possible to define functions/macros in the shell but
that should use soon be possible. You should also then be able to do
regurgítate which would write all the definitions out to a file.

Running a shell other than the standard erlang one is a bit
difficult so I have included a patched version of user_drv.erl from
the standard kernel app which allows you to start specific shells,
both local and remote. You can either put it into the distribution
or leave it where you run the LFE system and it will be picked
up. As far as I can see it is "safe" to use.

There are two versions of the interpreter, one written in Erlang,
the standard, and a compatible one written in LFE. Lfe_eval.lfe is
not written in a consistent manner and can be seen as an example of
the various styles of LFE programming, for example using
``match-lambda`` or ``lambda`` and ``case``.

As before there are a number of test files included as example code
in lieu of better documentation. They are also useful to see the
Core code generated by the LFE compiler, just add an option
[dcore]. N.B. Not all the test files compile, but this is on purpose
to test linter.

There is now a lisp prettyprinter in ``lfe_io``. Unfortunately the io
functions in ``lfe_io`` are not always obviously named from a lisp
viewpoint.

