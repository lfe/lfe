# LFE

[![Build Status](https://github.com/lfe/lfe/workflows/ci%2Fcd/badge.svg)](https://github.com/lfe/lfe/actions)
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
$ git clone https://github.com/lfe/lfe.git
$ cd lfe
$ make compile
```

LFE requires Erlang be installed on the system and that the ``erl`` binary is
in ``$PATH``.

## Running the Tests

To run the full suite of tests for LFE, simply use the following:

```sh
make tests
```

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
```
```text
Erlang/OTP 26 [erts-14.0.2] [source] [64-bit] [smp:10:10] [ds:10:10:10] [async-threads:1] [jit] [dtrace]

   ..-~.~_~---..
  (      \\     )    |   A Lisp-2+ on the Erlang VM
  |`-.._/_\\_.-':    |   Type (help) for usage info.
  |         g |_ \   |
  |        n    | |  |   Docs: http://docs.lfe.io/
  |       a    / /   |   Source: http://github.com/lfe/lfe
   \     l    |_/    |
    \   r     /      |   LFE v2.2.0 (abort with ^G)
     `-E___.-'

lfe>
```

If you have installed LFE, then you may start the REPL from any location:

```shell
$ lfe
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
lfe> (* 2 (+ 1 2 3 4 5 6))
42
lfe> (* 2 (lists:foldl #'+/2 0 (lists:seq 1 6)))
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

To bring up the LFE REPL:

```bash
$ docker run -it lfex/lfe
```

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
* [lfe_docs.txt](doc/lfe_docs.txt)
* [lfe_gen.txt](doc/lfe_gen.txt)
* [lfe_io.txt](doc/lfe_io.txt)
* [lfe_lib.txt](doc/lfe_lib.txt)
* [lfe_macro.txt](doc/lfe_macro.txt)

If you would like to make changes to the LFE documentation and then regenerate
the docs, you'll want to read the instructions here:

* [Updating LFE Documentation](doc/src/updating_docs.md)

## Join the Community

[LFE on Slack](https://lfe.slack.com), join by requesting an invite [here](https://erlef.org/slack-invite/lfe)

[LFE Forum - Erlang Forums](https://erlangforums.com/lfe)

## Maintainers

### Cutting Releases

Steps:

1. Update the version in `src/lfe.app.src`
1. Create the release tags
1. Create a release on Github
1. Publish to hex.pm

Once the `app.src` has been updated with the version for the release, you can create and push the tags (to Github) with the following:

``` shell
make tags
```

That will create the number-only version as well as the "v"-prefixed version.

For now, the process of creating a release on Github is manual:

1. Go to https://github.com/lfe/lfe/releases
1. Click "Draft new release"
1. Select the correct tag from the drop-down "Choose a tag"
1. Click "Generate release notes"
1. Click "Publish release"

Lastly, to publish LFE to hex.pm, you need to have rebar3 installed on our system and an entry for the hex plugin in your system `rebar.config` file. With that in place, publish a new release to hex.pm requires only the following:

``` shell
make publish
```

