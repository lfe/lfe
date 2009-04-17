(defmodule (test_pmod_ext x y)
  (extends test_pmod_base)
  (export (a1 2) (a2 1) (a3 1) (a4 1))
  (import (from lists (reverse 1))))

(defun a1
  (('a a) (xxx 1 x))
  (('b a) (xxx 2 x)))

(defun a2 (z)
  (flet ((xxx (z) (tuple x y z)))
    (xxx z)))

(defun a3 (z) (tuple (b1 z) (reverse z) this base))

(defun a4 (z) (: para_2 a1 z))

(defun b1 (z) (length z))

(defun b2 (z) (tuple x y (length z)))

(defun xxx (a b) (cons a b))
