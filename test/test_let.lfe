;; File    : test_let.el
;; Author  : Robert Virding
;; Purpose : Test cases for let and let*.

(define-module test_let
  (export (a 2) (b 2) (c 2) (d 2) (e 1) (f 1) (f 2))
  (import (from lists (reverse 1) (reverse 2))
	  (from ordsets (is_element 2))
	  (rename ordsets ((is_element 2) in))))

(define (a x y)
  (lambda (a) (+ (* a x) y)))

(define (b x y)
  (funcall x y 1))

;; Test multiply defined variables.
;; (define (t1 x y)
;;   (let ((m (list x y))
;; 	(n (cons x z))
;; 	((n o) (list x y)))
;;     (list m n o)))

(define (c x y)
  (let ((o (lambda (y) (list y y))))
    (list o (funcall o x))))

(define (d x y)
  (funcall (lambda (a) (+ a a)) x))

(define (e x)
  (let* ((a (list x))
	 (a (cons x a))
	 (a (cons x a)))
    a))

(define (f x) 'boom)

(define f
  (match-lambda
    (('a 1) (tuple 'a 1))
    (('b 2) (tuple 'b 2))
    ((x y) (when (> x y)) 'bigger)))

(define (b) '"a string")
