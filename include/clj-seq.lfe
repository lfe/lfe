(defmacro get-in args
  (let* ((rargs (lists:reverse args))
         (data (car rargs))
         (keys (lists:reverse (cdr rargs))))
    `(apply #'clj-seq:get-in/2 (list ,data (list ,@keys)))))

;;; The following allow developers to use (include-lib ...) on this file and
;;; pull in the functions from the passed module, making them available to
;;; call as if they were part of the language.
(defmacro generate-sequence-wrappers ()
  `(progn ,@(kla:wrap-mod-funcs 'clj-seq)))

(generate-sequence-wrappers)

(defun loaded-seq ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
