;; File    : test_flet.el
;; Author  : Robert Virding
;; Purpose : Test cases for flet, flet* and fletrec.

(defmodule test_flet
  (export (a 2) (b-1 1) (b-2 1) (c 1) (d 1) (e 1) (f 1) (f 2) (g 2) (h 1))
  ;; (export (t2 2))
  (import (from lists (reverse 1) (reverse 2))
          (from ordsets (is_element 2))
          (rename ordsets ((is_element 2) in))))

;; Test multiply defined functions.
;; (defun t1 (x y)
;;   (flet ((o (a) (+ x a))
;;          (o (a b) (+ (+ a b) x))
;;          (o (c) (* 100 (* c y))))
;;     (list (o x) (o y) (o x y))))

;; (defun t2 (x y)
;;   (fletrec ((o (a) (+ x a))
;;             (o (a b) (+ (+ a b) x))
;;             (o (c) (* 100 (* c y))))
;;     (list (o x) (o y) (o x y))))

(defun a (x y)
  (flet ((o (y) (list y y)))
    (list 'o (o x))))

;; Which (o 1) do we get in the "recursive" call?
(defun b-1 (n)
  (flet ((o (n)
            (if (=< n 1) 1
                (* n (o (- n 1))))))
    (o n)))

(defun b-2 (n)
  (fletrec ((o (n)
               (if (=< n 1) 1
                   (* n (o (- n 1))))))
    (o n)))

(defun o (n) (- 0 n))

;; Mutually recursive functions in fletrec.
(defun c (n)
  (fletrec ((o1 (n)
                (if (=< n 1) 1
                    (* n (o2 (- n 1)))))
            (o2 (n)
                (if (=< n 1) 1
                    (* n (o1 (- n 1))))))
    (o1 n)))

;; Check we get the RIGHT binding for o in fletrec.
(defun d (n)
  (let ((o n))
    (list o
      (fletrec ((o (n)
                   (if (=< n 1) 1
                       (* n (o (- n 1))))))
        (o n)))))

;; These are different f's, f/1 and f/2!
(defun e (n)
  (fletrec ((f ((x) (+ x n)))           ;Generate match-lambda here
            (f (x y) (+ x (+ y n))))
    (list (f n) (f n 10))))

(defun f (n)
  (flet ((f ((x) (+ x n)))              ;Generate match-lambda here
         (f (x y) (+ x (+ y n))))
    (list (f n) (f n 10))))

(defun g (x y)
  (flet* ((f (a) (list a y))
          (f ((a) (list a (f a))))
          (f (a) (list a (f a))))
    (f x)))

(defun f
  (('a 1) (tuple 'a 1))
  (('b 2) (tuple 'b 2))
  ((x y) (when (> x y)) 'bigger))

(defun b () '"a string")

;; Find the right f?
(defun h (l)
  (flet ((f (e) (if (is_number e) (+ e 5) (tuple e 5))))
    (: lists map (fun f 1) l)))
