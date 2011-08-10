MODULE

	lfe_bits

MODULE SUMMARY

	Lisp Flavoured Erlang (LFE) common binary functions

DESCRIPTION

	This module contains a collection of library functions for for
	handling binaries. They are generally not called by the user.

EXPORTS

parse_bitspecs(Specs) ->
	{ok,Size,{Type,Unit,Sign,Endian}} |
	{error,Error}.

	Parse a bitspec and return the data. Unmentioned fields get
	the value 'default'.

get_bitspecs(Specs) ->
	{ok,Size,{Type,Unit,Sign,Endian}} |
	{error,Error}.

	Parse a bitspec, apply defaults and return the
	data. Unmentioned fields get the value 'default'.

Error Information

	The following error values are returned:

	{undefined_bittype,Type}
	bittype_unit
