;;; Threading macros.
(defmacro ->      args `(clj:->      ,@args))
(defmacro ->>     args `(clj:->>     ,@args))
(defmacro as->    args `(clj:as->    ,@args))
(defmacro cond->  args `(clj:cond->  ,@args))
(defmacro cond->> args `(clj:cond->> ,@args))
(defmacro some->  args `(clj:some->  ,@args))
(defmacro some->> args `(clj:some->> ,@args))

;;; Conditional macros.
(defmacro condp    args `(clj:condp    ,@args))
(defmacro if-not   args `(clj:if-not   ,@args))
(defmacro when-not args `(clj:when-not ,@args))
(defmacro not=     args `(clj:not=     ,@args))

(defun loaded-clj ()
  "This is just a dummy function for display purposes when including from the
  REPL (the last function loaded has its name printed in stdout).

  This function needs to be the last one in this include."
  'ok)
