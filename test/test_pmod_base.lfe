(defmodule (test_pmod_base x y)
  (export (m1 2) (m2 1) (m3 1) (m4 1))
  (export all)
  (import (from lists (reverse 1))))

(defun m1
  (('a a) (xxx 1 x))
  (('b a) (xxx 2 x)))

(defun m2 (z)
  (flet ((xxx (z) (tuple x y z)))
    (xxx z)))

(defun m3 (z) (tuple (b1 z) (reverse z) this))

(defun m4 (z) (: para_2 a1 z))

(defun b1 (z) (length z))

(defun b2 (z) (tuple x y (length z)))

(defun xxx (a b) (cons a b))
