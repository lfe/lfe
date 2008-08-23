;; File    : test_try.el
;; Author  : Robert Virding
;; Purpose : Test cases for catch and try.

(define-module test_try
  (export (a 2) (b 1) (b 2) (c 2) (d 2) (e 2) (f 2) (g 2) (h 2))
  (import (from lists (reverse 1) (reverse 2))
	  (from ordsets (is_element 2))))

(define (a x y) (catch (+ x y) (b y)))

(define (a-1 x) (list 'x x))

;; Testing just catch.
(define (b x y)
  (try
    (begin
      (yyy x y)
      (zzz y))
    (catch
	;; Pattern MUST be tuple of 3 elements here!
	;; (tuple TYPE VALUE IGNORE-THIS)
	((tuple 'error n o) (tuple 'this-is-error n))
	((tuple 'throw n o) (tuple 'this-is-throw n))
	((tuple _ n o) (tuple 'this-is-default n)))))

;; Testing using case and catch.
(define (c x y)
  (try (begin
	 (yyy x y)
	 (zzz y))
       (case
	   ('sune #(value sune))
	 ('bert #(value bert)))
       (catch
	 ;; Pattern MUST be tuple of 3 elements here!
	 ;; (tuple TYPE VALUE IGNORE-THIS)
	 ((tuple 'error n o) (tuple 'this-is-error n))
	 ((tuple 'throw n o) (tuple 'this-is-throw n))
	 ((tuple _ n o) (tuple 'this-is-default n)))))

(define (d x y)
  (try (begin
	 (yyy x y)
	 (zzz y))
       (after )))

;; Testing just after.
(define (e x y)
  (try
    (begin
      (yyy x y)
      (zzz y))
    (after (yyy 'this-is-after (list x y))
	   'this-is-after)))

;; Testing using case and after.
(define (f x y)
  (try
    (begin
      (yyy x y)
      (zzz y))
    (case
	('sune #(value sune))
      ('bert #(value bert)))
    (after (yyy 'this-is-after (list x y))
	   'this-is-after)))

;; Testing using catch and after.
(define (g x y)
  (try
    (begin
      (yyy x y)
      (zzz y))
    (catch
      ;; Pattern MUST be tuple of 3 elements here!
      ;; (tuple TYPE VALUE IGNORE-THIS)
      ((tuple 'error n o) (tuple 'this-is-error n))
      ((tuple 'throw n o) (tuple 'this-is-throw n))
      ((tuple _ n o) (tuple 'this-is-default n)))
    (after (yyy 'this-is-after (list x y))
	   'this-is-after)))

;; Testing using all case, catch and after.
(define (h x y)
  (try
    (begin
      (yyy x y)
      (zzz y))
    (case
	('sune #(value sune))
      ('bert #(value bert)))
    (catch
      ;; Pattern MUST be tuple of 3 elements here!
      ;; (tuple TYPE VALUE IGNORE-THIS)
      ((tuple 'error n o) (tuple 'this-is-error n))
      ((tuple 'throw n o) (tuple 'this-is-throw n))
      ((tuple _ n o) (tuple 'this-is-default n)))
    (after (yyy 'this-is-after (list x y))
	   'this-is-after)))

(define b (lambda (x) '"a string"))

(define (yyy x y)
  (: io fwrite '"(yyy ~w ~w)\n" (list x y)))

(define (zzz x)
  (: foo bar x))
