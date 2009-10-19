MODULE

	lfe_comp

MODULE SUMMARY

	Lisp Flavoured Erlang (LFE) compiler

DESCRIPTION

	This module provides an interface to the standard LFE
	compiler. It can generate either a new file which contains the
	object code, or return a binary which can be loaded directly.

EXPORTS

file(FileName) -> CompRet

	Is the same as file(FileName, [report]).

file(FileName, Options) -> CompRet

	where
	  CompRet = ModRet | BinRet | ErrRet
	  ModRet = {ok,ModuleName} | {ok,ModuleName,Warnings}
	  BinRet = {ok,ModuleName,Binary} | {ok,ModuleName,Binary,Warnings}
	  ErrRet = error | {error,Errors,Warnings}

	Compile an LFE file, either writing the generated module to a
	file or returning it as a binary. The generated module is
	ready to be loaded into Erlang.

	The currently recognised options are:

	binary
		Return the binary of the module and do not save it in
		a file.

	to_exp
		Print a listing of the macro expanded LFE code in the
		file <File>.expand. No object file is produced. Mainly
		useful for debugging and interest.

	to_core0
	to_core
		Print a listing of the Core Erlang code before/after
		being optimised in the file <File>.core. No object
		file is produced. Mainly useful for debugging and
		interest.

	to_kernel
		Print a listing of the Kernel Erlang code in the file
		<File>.kernel. No object file is produced. Mainly
		useful for debugging and interest.

	to_asm
		Print a listing of the Beam code in the file
		<File>.S. No object file is produced. Mainly
		useful for debugging and interest.

	{outdir,Dir}
		Save the generated files in directory Dir instead of
		the current directory.

	report
		Print the errors and warnings as they occur.

	return
		Return an extra return field containing Warnings on
		success or the errors and warnings in
		{error,Errors,Warnings} when there are errors.

	debug_print
		Causes the compiler to print a lot of debug
		information.

	If the binary option is given then options that produce a
	listing file will cause the internal format for that compiler
	pass to be returned.

	Both Warnings and Errors have the following format:

	[{FileName,[ErrorInfo]}]

	ErrorInfo is described below. When generating Errors and
	Warnings the line number is the line of the start of the form
	in which the error occurred. The file name has been included
	here to be compatible with the Erlang compiler. As yet there
	is no extra information about included files.

forms(Forms) -> CompRet

	Is the same as forms(Forms, [report]).

forms(Forms, Options) -> CompRet

	where
	  Forms = [sexpr()]
	  CompRet = BinRet | ErrRet
	  BinRet = {ok,ModuleName,Binary} | {ok,ModuleName,Binary,Warnings}
	  ErrRet = error | {error,Errors,Warnings}

	Compile the forms as an LFE module returning a binary. This
	function takes the same options as lfe_comp:file/1/2. When
	generating Errors and Warnings the "line number" is the index
	of the form in which the error occured.

format_error(Error) -> Chars

	Uses an ErrorDescriptor and returns a deep list of characters
	which describes the error. This function is usually called
	implicitly when an ErrorInfo structure is processed. See
	below.

Error Information

	The ErrorInfo mentioned above is the standard ErrorInfo
	structure which is returned from all IO modules. It has the
	following format:

	{ErrorLine,Module,ErrorDescriptor}
    
	A string describing the error is obtained with the following call:

	apply(Module, format_error, ErrorDescriptor)
