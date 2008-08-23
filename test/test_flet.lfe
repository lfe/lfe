;; File    : test_flet.el
;; Author  : Robert Virding
;; Purpose : Test cases for flet, flet* and fletrec.

(define-module test_flet
  (export (a 2) (b-1 1) (b-2 1) (c 1) (d 1) (e 1) (f 1) (f 2) (g 2))
  ;; (export (t2 2))
  (import (from lists (reverse 1) (reverse 2))
	  (from ordsets (is_element 2))
	  (rename ordsets ((is_element 2) in))))

;; Test multiply defined functions.
;; (define (t1 x y)
;;   (flet ((o (lambda (a) (+ x a)))
;; 	 (o (lambda (a b) (+ (+ a b) x)))
;; 	 (o (lambda (c) (* 100 (* c y)))))
;;     (list (o x) (o y) (o x y))))

;; (define (t2 x y)
;;   (fletrec ((o (lambda (a) (+ x a)))
;; 	    (o (lambda (a b) (+ (+ a b) x)))
;; 	    (o (lambda (c) (* 100 (* c y)))))
;;     (list (o x) (o y) (o x y))))

(define (a x y)
  (flet ((o (lambda (y) (list y y))))
    (list 'o (o x))))

;; Which (o 1) do we get in the "recursive" call?
(define (b-1 n)
  (flet ((o (lambda (n)
	      (if (=< n 1) 1
		  (* n (o (- n 1)))))))
    (o n)))

(define (b-2 n)
  (fletrec ((o (lambda (n)
		 (if (=< n 1) 1
		     (* n (o (- n 1)))))))
    (o n)))

(define (o n)
  (- 0 n))

;; Mutually recursive functions in fletrec.
(define (c n)
  (fletrec ((o1 (lambda (n)
		  (if (=< n 1) 1
		      (* n (o2 (- n 1))))))
	    (o2 (lambda (n)
		  (if (=< n 1) 1
		      (* n (o1 (- n 1)))))))
    (o1 n)))

;; Check we get the RIGHT binding for o in fletrec.
(define (d n)
  (let ((o n))
    (list o
	  (fletrec ((o (lambda (n)
			 (if (=< n 1) 1
			    (* n (o (- n 1)))))))
	    (o n)))))

;; These are different f's, f/1 and f/2!
(define (e n)
  (fletrec ((f (match-lambda ((x) (+ x n))))
	    (f (lambda (x y) (+ x (+ y n)))))
    (list (f n) (f n 10))))

(define (f n)
  (flet ((f (match-lambda ((x) (+ x n))))
	 (f (lambda (x y) (+ x (+ y n)))))
    (list (f n) (f n 10))))

(define (g x y)
  (flet* ((f (lambda (a) (list a y)))
	  (f (match-lambda ((a) (list a (f a)))))
	  (f (lambda (a) (list a (f a)))))
    (f x)))

(define f
  (match-lambda
    (('a 1) (tuple 'a 1))
    (('b 2) (tuple 'b 2))
    ((x y) (when (> x y)) 'bigger)))

(define (b) '"a string")
