(defmodule test_eval
  (export (a 1) (b 1))
  (import))

(eval-when-compile
 (defun yy (a b)
   (list 'tuple a b))
 )

;; Test when a macro is evaluated.
(defmacro xxx (a)
  (: io fwrite '"xxx: ~p\n" (list a))
  `(list ,a))

(defmacro yyy (a)
  `(let ((z ,(yy a 1)))
     (list z)))

(defun a (m)
  (xxx m))

(defun b (m)
  (yyy m))
