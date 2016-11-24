# LFE

[![Travis](https://img.shields.io/travis/rvirding/lfe.svg)](https://travis-ci.org/rvirding/lfe)
[![Hex.pm version](https://img.shields.io/hexpm/v/lfe.svg)](https://hex.pm/packages/lfe)
[![Hex.pm downloads](https://img.shields.io/hexpm/dt/lfe.svg)](https://hex.pm/packages/lfe)
[![Hex.pm weekly downloads](https://img.shields.io/hexpm/dw/lfe.svg)](https://hex.pm/packages/lfe)
[![Hex.pm daily downloads](https://img.shields.io/hexpm/dd/lfe.svg)](https://hex.pm/packages/lfe)

LFE, Lisp Flavoured Erlang, is a lisp syntax front-end to the Erlang
compiler. Code produced with it is compatible with "normal" Erlang
code. An LFE evaluator and shell is also included.


## Building

To compile LFE, simple clone it and compile:

```shell
$ git clone https://github.com/rvirding/lfe.git
$ cd lfe
$ make compile
```

LFE requires Erlang be installed on the system and that the ``erl`` binary is
in ``$PATH``.


## Installation

Should you wish to have LFE available system-wide, you can run
the following ``make`` target:

```shell
$ make install
```

By default this will create the programs ``lfe``, ``lfec``, ``lfedoc`` and
``lfescript`` in ``/usr/local/bin``. This can be changed by defining the
``make`` variable ``PREFIX`` to point to the desired parent directory.

Note that the ``install`` target will also install the LFE man pages in the
appropriate ``$(PREFIX)/share/man/man*`` directories. This can be changed by
defining the ``make`` variable ``MANINSTDIR`` to point to the desired top
``man`` directory.

So:

```shell
$ make install PREFIX=/Users/rv/ MANINSTDIR=/Users/rv/man
```

will put the programs in ``/Users/rv/bin`` and the man pages in the
``/Users/rv/man/man*`` directories.


## REPL

If you're running LFE from a git clone working dir, you can start the REPL
like so after compiling:

```shell
$ ./bin/lfe
Erlang 17 (erts-6.0) [source] [64-bit] [smp:8:8] ...

LFE Shell V6.0 (abort with ^G)
>
```

If you have installed LFE, then you may start the REPL from any location:

```shell
$ lfe
Erlang 17 (erts-6.0) [source] [64-bit] [smp:8:8] ...

LFE Shell V6.0 (abort with ^G)
>
```

Likewise, you may run an LFE shell script in the same style as shell scripts
with:

```shell
$ ./bin/lfe script-name script-arg-1 ...
```

or

```shell
$ lfe script-name script-arg-1 ...
```

## Usage

The [docs site](http://lfe.github.io/docs.html) has several places to explore
that will show you how to start using LFE. However, here's a quick taste:

* start up an LFE REPL as demonstrated above
* then, do something like this:
```cl
> (* 2 (+ 1 2 3 4 5 6))
42
> (* 2 (lists:foldl #'+/2 0 (lists:seq 1 6)))
42
```

## Docker Support

LFE now supports Docker. To get started, simply do the following, once you
have Docker set up on your machine:

```bash
$ docker pull lfex/lfe
```

Alternatively, you could build the image yourself:

```bash
$ cd lfe
$ docker build .
```

Here are a couple of simple usage examples:

```bash
$ docker run lfex/lfe
42
```

```bash
$ docker run -i -t lfex/lfe lfe
Erlang/OTP 18 [erts-7.0] [source-4d83b58] [64-bit] [smp:8:8] ...

LFE Shell V7.0 (abort with ^G)
>
```

That last command will dump you into the LFE REPL on a running container
of the ``lfex/lfe`` Docker image. For more information on using Docker
with LFE, be sure to read the
[tutorial](http://blog.lfe.io/tutorials/2014/12/07/1837-running-lfe-in-docker/).


## Documentation

* [Quick Start](https://lfe.gitbooks.io/quick-start/content/)
* LFE [user guide](doc/user_guide.txt)
* [version history](doc/src/version_history.md)

Files with more technical details:

* [lfe.txt](doc/lfe.txt)
* [lfescript.txt](doc/lfescript.txt)
* [lfe_bits.txt](doc/lfe_bits.txt)
* [lfe_clj.txt](doc/lfe_clj.txt)
* [lfe_comp.txt](doc/lfe_comp.txt)
* [lfe_doc.txt](doc/lfe_doc.txt)
* [lfe_gen.txt](doc/lfe_gen.txt)
* [lfe_io.txt](doc/lfe_io.txt)
* [lfe_lib.txt](doc/lfe_lib.txt)
* [lfe_macro.txt](doc/lfe_macro.txt)

If you would like to make changes to the LFE documentation and then regenerate
the docs, you'll want to read the instructions here:

* [Updating LFE Documentation](doc/src/updating_docs.md)
