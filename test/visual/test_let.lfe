;; File    : test_let.el
;; Author  : Robert Virding
;; Purpose : Test cases for let, let* and funcall.

(defmodule test_let
  (export (flip 2) (a 1) (b 2) (c 2) (d 2) (e 2) (f 2) (g 2))
  (import (from lists (reverse 1) (reverse 2))
          (from ordsets (is_element 2))
          (rename ordsets ((is_element 2) in))))

(defun flip (x y)
  (let ((x y)
        (y x))
    (list x y)))

(defun a (x)
  (let* ((a (list x))
         (a (cons x a))
         (a (cons x a)))
    a))

(defun b (x y)
  (let ((m (+ x y))
        (n (xxx))
        (o (yyy x y)))
    (list m n o)))

;; Test multiply defined variables.
;; (defun t1 (x y)
;;   (let ((m (list x y))
;;         (n (cons x z))
;;         ((list n o) (list x y)))
;;     (list m n o)))

(defun c (x y)
  (funcall x y 1))

(defun d (x y)
  (list
   (let ((o (lambda (a b) (list a b))))
     (funcall o (* x y) (+ x y)))
   (list x y)))

;; Test funcalling lambdas and match-lambdas directly.
(defun e (x y)
  (list
   (funcall (lambda (a b) (list a b)) (* x y) (+ x y))
   (list x y)))

(defun f (x y)                          ;Arg mismatch, push error to runtime
  (funcall (lambda (a b c) (list a b)) (* x y)))

(defun g (x y)
  (funcall (match-lambda
             (('a n) (tuple 'a n))
             (('b n) (tuple 'b n))
             ((m n) (when (> m n)) 'bigger))
           x (* y 2)))

(defun xxx () '"a string")

(defun yyy (x y)
  (lambda (a) (+ (* a x) y)))
