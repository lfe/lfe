;;; File    : test_bin.lfe
;;; Author  : Robert Virding
;;; Purpose : Test binaries.

(define-module test_bin
  (export (a 0) (a 1) (a 2) (ap 2) (a 3) (aa1 1) (aa2 1) (aa2p 1) (aa3 1))
  (export (b 1) (b 2) (bb1 2) (bb2 2))
  (export (c 2)))

(define (a)
  (binary 1 2 3))

(define (a x) (binary (x (size 24))))

(define (a x y)
  (binary (x float (size 32)) (y float (size 64))))

(define (ap x y)
  (binary (x float (size 32)) (y float (size 40))))

(define (a x y z)
  (binary (x unsigned) (y (size 16) big-endian) (z (size 3) little-endian)))

(define (aa1 b)
  (case b
    ((binary (x (size 24)) (zz binary)) (list x zz))))

(define (aa2 b)
  (case b
    ((binary (x float (size 32)) (y float (size 64)) (zz bitstring))
     (list x y zz))))

(define (aa2p b)
  (case b
    ((binary (x float (size 32)) (y float (size 40)) (zz bitstring))
     (list x y zz))))

(define (aa3 b)
  (case b
    ((binary (x unsigned) (y (size 16) big-endian) (z (size 3) little-endian)
	     (zz bitstring))
     (list x y z zz))))

(define (b bin)
  (binary (bin binary) (bin (size 16) bitstring) (bin binary)))

(define (b b1 b2)
  (binary (b1 bitstring) (b2 (size 3) bitstring signed) (b2 bitstring)))

(define (bb1 b n)
  (case b
    ((binary (b1 binary (size n)) (b2 (size 16) bitstring) (b3 binary))
     (list b1 b2 b3))))

(define (bb2 b n)
  (case b
    ((binary (b1 bitstring (size n)) (b2 (size 3) bitstring signed)
	     (b3 bitstring))
     (list b1 b2 b3))))

;; (define (c x y)
;;   (binary (x (size y)) (y (size (+ y 1)))))

(define (c x y)
  (let ((y1 (+ y 1)))
    (binary (x (size y)) (y (size y1)))))
