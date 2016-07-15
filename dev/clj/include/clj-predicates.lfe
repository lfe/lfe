(defmacro in? (item collection)
  `(orelse ,@(lists:map
               (lambda (x)
                 `(=:= (quote ,x) ,item))
               `(,@(cadr collection)))))

(defmacro not-in? (item collection)
  `(not (in? ,item ,collection)))

;;; The following allow developers to use (include-lib ...) on this file and
;;; pull in the functions from the passed module, making them available to
;;; call as if they were part of the language.
(defmacro generate-predicate-wrappers ()
  `(progn ,@(kla:wrap-mod-funcs 'clj-p)))

(generate-predicate-wrappers)

(defun loaded-predicates ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
