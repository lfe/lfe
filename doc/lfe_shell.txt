MODULE

	lfe_shell

MODULE SUMMARY

	Lisp Flavoured Erlang (LFE) shell

DESCRIPTION

	There is now a simple LFE shell in which you can enter sexprs
	which are evaluated and the value printed. There are no user
	shell variables (scoping and lack of setq)

	Built-in shell functions:

	(c File [Options])
		Compile and load an LFE file. Assumes default
		extension .lfe.

	(l Module ...)
		Load modules.

	(m Module ...)
		Print out module information, if no modules are given
		then print information about all modules.

	(ec File [Options])
		Compile and load an Erlang file.

	(slurp File)
		Slurp in a source LFE file and makes all functions and
		macros defined in the file available in the
		shell. Only one file can be slurped at a time and
		slurping a new file removes all data about the
		previous one.

	(unslurp)
		Remove all function and macro definitions except the
		default ones.

	(set Pattern Expr)
		Evaluate Expr ad match the result with Pattern binding
		variables in it. These variables can then be used in
		the shell and also rebound in another set.

	(: c Command Arg ...)
		All the commands in the standard Erlang shell can be
		reached in this way.

	Builtin shell variables:

	+/++/+++
		The three previous expressions input.
	*/**/***
		The values of the previous 3 expressions.
	-
		The current expression input.

Starting the LFE shell

	The best way is probably to start Erlang directly running the
	LFE shell with:

	erl -noshell -noinput -s lfe_boot start

	This can easily be put in a shell script.

	From a normal Erlang shell the best way to start the shell is
	by calling:

	17> lfe_shell:server().

	Giving the user switch commands:

	--> s lfe_shell
	--> c

	will create a job running the LFE shell and connect to
	it. This also works when starting a remote shell.
