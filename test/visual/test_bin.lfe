;;; File    : test_bin.lfe
;;; Author  : Robert Virding
;;; Purpose : Test binaries.

(defmodule test_bin
  (export (a 0) (a 1) (af 2) (afp 2) (a 3)      ;Constructors
          (p1 1) (p2 1) (p2p 1) (p3 1) (p4 0))  ;Patterns
  (export (b 1) (b 2) (bb1 2) (bb2 2))          ;Binaries/bitstrings
  (export (u 1) (u 2))                          ;Unicode types
  (export (vs1 2) (vs2 2) (vs3 2))              ;Value and size expressions
  (export (d1 0) (d2 0) (d3 0))                 ;Binary constants
  (export (sl1 0) (sl1 1) (sl2 0) (sl2 1))      ;String literals
  )

;; Binary constructors.

(defun a () (binary 1 2 3))

(defun a (x) (binary (x (size 24))))

(defun af (x y)
  (binary (x float (size 32)) (y float (size 64))))

(defun afp (x y)                        ;This will cause an error!
  (binary (x float (size 32)) (y float (size 40))))

(defun a (x y z)
  (binary (x unsigned) (y (size 16) big-endian) (z (size 3) little-endian)))

;; Patterns.

(defun p1 (b)
  (case b
    ((binary (x (size 24)) (zz binary)) (list x zz))))

(define (p2 b)                          ;Old style
  (case b
    ((binary (x float (size 32)) (y float (size 64)) (zz bitstring))
     (list x y zz))))

(defun p2p (b)                          ;This will cause an error!
  (case b
    ((binary (x float (size 32)) (y float (size 40)) (zz bitstring))
     (list x y zz))))

(defun p3 (b)
  (case b
    ((binary (x unsigned) (y (size 16) big-endian) (z (size 3) little-endian)
             (zz bitstring))
     (list x y z zz))))

(defun p4 ()
  (let* ((bin #b(2 "AB" "CD"))
     (tup #(2 #b("AB") #b("CD")))
     ((binary s (b binary (size s)) (rest binary)) bin))
    ;; Test equality
    (=:= tup (tuple s b rest) (p4-1 bin))))

(defun p4-1
  ([(binary s (b binary (size s)) (rest binary))] (tuple s b rest)))

;; Binaries/bitstrings.

(defun b (bin)
  (binary (bin binary) (bin (size 16) bitstring) (bin binary)))

(define (b b1 b2)                       ;Old style
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

;; Unicode types.

(defun u (x)
  (binary (x utf-8) (x utf-16) (x utf-32)))

(defun u (x y)
  (binary (x utf-8 big-endian) (y utf-32 little-endian)
          (x utf-16 signed little-endian)))

;; Value and size expressions

(defun vs1 (x y)
  (tuple (let ((y1 (+ y 1)))
           (binary (x (size y)) (y (size y1))))
         (binary (x (size y)) (y (size (+ y 1))))))

(defun vs2 (x y)
  (tuple (binary ((* 2 x) (size y)))            ;Just value expr
         (binary (x (size (+ y 8))))            ;Just size expr
         (binary ((* 2 x) (size (+ y 8))))))    ;Both value and size expr

(defun vs3 (x y)
  (binary ((* 2 x) (size y))            ;Just value expr
          (x (size (+ y 8)))            ;Just size expr
          ((* 2 x) (size (+ y 8)))))    ;Both value and size expr

;; Binary constants

(defun d1 ()
  #b(1 2 3))

(defun d2 ()
  #b((1.5 float) (2.0 float (size 32)) (3.0 float little-endian)))

(defun d3 ()
  #b(1 2 3))

;; String literals

(defun sl1 ()
  (binary "abc" "едц"))

(defun sl1 (bin)
  (let (((binary "abc" (rest binary)) bin))
    rest))

(defun sl2 ()
  (binary "abc" ("едц" utf-8)))

(defun sl2 (bin)
  (let (((binary "abc" ("едц" utf-8) (rest binary)) bin))
    rest))
