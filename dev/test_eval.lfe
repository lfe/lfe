(defmodule test_eval
  (export (a 1) (b 1))
  (import))

;; Test when a macro is evaluated.
(defmacro xxx (a)
  (: io fwrite '"xxx: ~p\n" (list a))
  `(list ,(xx a 2)))

(defmacro yyy (a)
  `(let ((z ,(yy a 1)))
     (list z)))

;; Helper functions can be defined after macros, as long as before use.
(eval-when-compile
  (defun yy (a b)
    (list 'tuple a b))
  )

(eval-when-compile
  ;; xx can call yy as it is defined before xx.
  (defun xx (a b)
    (yy a b))
  )

(defun a (m)
  (xxx m))

(defun b (m)
  (yyy m))
