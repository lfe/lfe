lfe_macro(3)                                                      lfe_macro(3)



NAME
       lfe_macro - Lisp Flavoured Erlang (LFE) macro expander

SYNOPSIS
       This  module  provides an interface to the LFE macro expander.  The ex‐
       pander is used by the LFE compile and in the shell but can also be used
       by applications explicitly wanting to handle a file.

DATA TYPES
       sexpr()

       An LFE s-expression, a list structure.

       filesexpr() = {Sexpr,Line}

       This  is  the format returned by lfe_io:parse_file/1 and is used by the
       compiler to give better error information.

       env()

       This  is  an  macro  and   evaluation   environment   as   created   by
       lfe_lib:new_env().

       mac_state()

       This is the internal state used by the macro expander.

EXPORTS
       expand_expr(Sexpr, Env) -> {yes,Exp} | no.

       expand_expr_1(Sexpr, Env) -> {yes,Exp} | no.

       where

              Sexpr = Exp = sexpr()
              Env = env()

       Test  if the top s-expression here is a macro call, if so expand it and
       return {yes,Expansion}, if not then return no.  expand_expr/2 will  ex‐
       pand  the  top  s-expression  as much as possible while expand_expr_1/2
       will only try it once.  These functions use the  macro  definitions  in
       the environment and the standard pre-defined macros.

       expand_expr_all(Sexpr, Env) -> Sexpr.

       where

              Sexpr = sexpr()
              Env = env()

       Expand  all macros in Sexpr either using the definitions in Env or just
       the default macros.  Note that any eventual new macro definitions  will
       be lost.

       expand_form_init(Deep, Keep) -> MacState

       where

                Deep = boolean()
                Keep = boolean()
                MacState = mac_state()

       Create an internal macro state.  Deep determines whether the form is to
       be expanded internally at depth and Keep whether macro definition forms
       are to be kept.

       expand_fileforms([FileForm], Env, Deep, Keep) -> ExpRet

       expand_fileforms([FileForm], Env, MacState) -> ExpRet

       where

                FileForm = filesexpr()
                Env = env()
                Deep = boolean()
                Keep = boolean()
                MacState = mac_state()
                ExpRet = {yes,[FileSexpr],Env,Warnings} |
                         {error,Errors,Warnings}

       expand a sequence of file forms.

       expand_form(Form, Line, Env, MacState) -> RetState

       expand_fileform(FileForm, Env, MacState) -> RetState

       where

                Form = sexpr()
                FileForm = filesexpr()
                Line = integer()
                Env = env()
                MacState = mac_state()
                RetState = {ok,Form,Env,Macstate} |
                           {error,Errors,Warnings,MacState}

       Expand a file form using the environment and macro state.

SEE ALSO
       lfe_comp(3), lfe_gen(3)

AUTHORS
       Robert Virding.



                                   2008-2020                      lfe_macro(3)
