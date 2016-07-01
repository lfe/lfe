;; Test generation and calling funs.

(defmodule fun-test
  (export (a 3)
          (a-1 3)
          (b 3)
          ))

(defun a (x y z)
  (list #'local_1/1 (funcall #'local_2/2 y z)))

(defun a-1 (x y z)
  (list #'local_1/1 (funcall (lambda (m n) (local_2 m n))  y z)))

(defun b (x y z)                        ;All literals
  (function a b 3))

;; (defun b (x y z)
;;   (function x y z))

(defun local_1 (a) a)

(defun local_2 (a b) (tuple a b))
