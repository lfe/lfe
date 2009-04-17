;;; File    : test_bin.lfe
;;; Author  : Robert Virding
;;; Purpose : Test binaries.

(defmodule test_bin
  (export (a 0) (a 1) (a 2) (ap 2) (a 3) (aa1 1) (aa2 1) (aa2p 1) (aa3 1))
  (export (b 1) (b 2) (bb1 2) (bb2 2))
  (export (c 2)))

(defun a ()
  (binary 1 2 3))

(defun a (x) (binary (x (size 24))))

(defun a (x y)
  (binary (x float (size 32)) (y float (size 64))))

(defun ap (x y)				;This will cause an error!
  (binary (x float (size 32)) (y float (size 40))))

(defun a (x y z)
  (binary (x unsigned) (y (size 16) big-endian) (z (size 3) little-endian)))

(defun aa1 (b)
  (case b
    ((binary (x (size 24)) (zz binary)) (list x zz))))

(define (aa2 b)				;Old style
  (case b
    ((binary (x float (size 32)) (y float (size 64)) (zz bitstring))
     (list x y zz))))

(defun aa2p (b)				;This will cause an error!
  (case b
    ((binary (x float (size 32)) (y float (size 40)) (zz bitstring))
     (list x y zz))))

(defun aa3 (b)
  (case b
    ((binary (x unsigned) (y (size 16) big-endian) (z (size 3) little-endian)
	     (zz bitstring))
     (list x y z zz))))

(defun b (bin)
  (binary (bin binary) (bin (size 16) bitstring) (bin binary)))

(define (b b1 b2)			;Old style
  (binary (b1 bitstring) (b2 (size 3) bitstring signed) (b2 bitstring)))

(defun bb1 (b n)
  (case b
    ((binary (b1 binary (size n)) (b2 (size 16) bitstring) (b3 binary))
     (list b1 b2 b3))))

(defun bb2 (b n)
  (case b
    ((binary (b1 bitstring (size n)) (b2 (size 3) bitstring signed)
	     (b3 bitstring))
     (list b1 b2 b3))))

(defun c (x y)
  (tuple (let ((y1 (+ y 1)))
	   (binary (x (size y)) (y (size y1))))
	 (binary (x (size y)) (y (size (+ y 1))))))
