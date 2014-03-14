# LFE

LFE, Lisp Flavoured Erlang, is a lisp syntax front-end to the Erlang
compiler. Code produced with it is compatible with "normal" Erlang
code. An LFE evaluator and shell is also included.

## Important Note

A quick fix has been added to compensate for some incompatibilites in
Core Erlang between the old R12B and the new R13B. The fixes are found
in the file ```lfe_codegen.erl```. For one fix code must be chosen depending
on whether the systems runs on R12B or R13B, this is the (tiny)
function ```c_fname/3``` near the end of the file. Choose the right version
of the function. The ```.beam``` file in ebin is for R13B.

I will try to make a better fix soon. Sorry about that.

## Release Notes

[doc/release_notes.md](doc/release_notes.md)

## Version History

[doc/version_history.md](doc/version_history.md)
