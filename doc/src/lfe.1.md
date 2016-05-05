% lfe(1)
% Robert Virding
% 2008-2016


# NAME

lfe - Lisp Flavoured Erlang (LFE) shell

# SYNOPSIS

``lfe`` is a simple LFE repl (read-eval-print loop) in which you can
enter sexprs which then are evaluated and the value printed. You can
also define local functions and macros as well as set variables. It
can read commands either from the standard input or from a file.

The LFE repl is implemented in the module ``lfe_shell``.

# BUILT-IN SHELL FUNCTIONS

These are defined as normal functions and macros and can be
called from anywhere in the shell. They can even be redefined.
They can also be explicitly called (: lfe_shell ...).

**(c File [Options])**

Compile and load an LFE file. Assumes default extension ``.lfe``.

**(cd Dir)**

Change the working directory.

**(ec File [Options])**

Compile and load an Erlang file.

**(flush)**

Flush any messages sent to the shell.

**(i)**

Print information about the currently running processes in the system.

**(l Module ...)**

Load modules.

**(m Module ...)**

Print out module information, if no modules are given then print
information about all modules.

**(pid x y z)**

Create a pid from x, y, z.

**(p Expr)**

**(pp Expr)**

Print/prettyprint a value to full depth.

**(pwd)**

Print the current working directory.

**(regs)**

Print information about the registered processes in the system.

**(: c Command Arg ...)**

**(c:Command Arg ...)**

All the commands in the standard Erlang shell can be reached in this way.


# BUILT-IN SHELL COMMANDS

These are special forms which are only recognised at the
top-level in shell input. The cannot be redefined.

**(reset-environment)**

Resets the environment to its initial state. This will clear all
variables, functions an macros that have been set.

**(set Pattern Expr)**

**(set Pattern (when Guard) Expr)**

Evaluate Expr and match the result with Pattern
binding variables in it. These variables can then be
used in the shell and also rebound in another set.

**(slurp File)**

Slurp in a source LFE file and makes all functions and
macros defined in the file available in the
shell. Only one file can be slurped at a time and
slurping a new file basically does an unslurp first.

**(unslurp)**

Revert back to the state before the last slurp
removing all function and macro definitions both in
the slurped file and defined in the shell since then.

**(run File)**

Execute all the shell commands in File. All defined
variables, functions and macros will be saved in the
environment if there are no errors.


# SHELL FUNCTIONS AND MACROS

Functions and macros can be defined in the shell. These will
only be local to the shell and cannot be called from
modules. The forms are the standard forms for defining
functions and macros.

**(defun Fun ...)**

Define a function in the shell.

**(defmacro Macro ...)**

Define a macro in the shell.


# BUILT-IN SHELL VARIABLES

**``+``, ``++``, ``+++``**

The three previous expressions input.

**``*``, ``**``, ``***``**

The values of the previous 3 expressions.

**``-``**

The current expression input.


# SHELL ENVIRONMENT

The shell maintains an environment of local function and macro
definitions, and variable bindings. The environment can be
accessed using the built-in shell variable $ENV. This can be
useful when calling functions like macroexpand and
macro-function which unless an explicit environment is given
will only search the default environment.


# STARTING THE LFE SHELL

After installing the best way is probably to start Erlang
directly running the LFE shell with:

```
lfe [flags]
```

From a normal Erlang shell the best way to start the shell is
by calling:

```
17> lfe_shell:server().
```

Giving the user switch commands:

```
--> s lfe_shell
--> c
```

will create a job running the LFE shell and connect to
it. This also works when starting a remote shell.


# RUNNING LFE SHELL SCRIPTS

The LFE shell can also be directly called to run LFE shell
scripts with:

```
lfe [flags] file [args]
```

This will start the shell, run a script with LFE shell
commands and then terminate the shell. The following built-in
variables are also bound:

**script-name**

The name of the script file as a string.

**script-args**

A list of the arguments to the script as strings. If
no arguments have been given then this will be an
empty list.


# RUNNING A HEADLESS SHELL

LFE comes with a ``gen_server`` shell in the ``lfe_server`` module.
This allows the developer to start an LFE server which solely
supports a programmatic interface, not an interactive one (it
does not write to stdout, including not displaying a start-up
banner). All input is sent to the server via the ``send/1``
function defined for the server's API.

When calling ``send/1``, the developer passes a quoted LFE
expression which is then handled by ``lfe_server:handle_call/3``.
A successfull call results in a returned evaluation of the
quoted expression that was called. An error currently causes
the process server to die, thus losing REPL state.

Usage looks like the following:

```
> (lfe_server:start)
#(ok <0.35.0>)
> (lfe_server:send '(defun adder (a b) (+ a b)))
adder
> (lfe_server:send '(adder 10 20))
30
> (lfe_server:send '(set val (adder 10 20)))
30
> (lfe_server:send 'val)
30
```

The ``lfe_server`` server process may be easily added to a
supervision tree, and in fact this is encouraged for any
serious use of a headless LFE shell server. It goes without
saying that any number of servers may be started in a
supervision tree, each with their own dedciated LFE
environment.


# SEE ALSO

**lfescript(1)**, **lfe_guide(7)**
