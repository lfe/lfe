(defmodule specs-test
  (export (s1 0) (s2 1) (s3 2) (s4 2) (s5 2) (s6-1 2) (s6-2 2))
  (export (is3 1))
  (spec #(bad_spec)))                   ;Non spec attribute

;;(include-file "include_specs.hrl")
(include-file "include-specs.lfe")

(defun s1 () 42)

(defun s2 (x) 42)

(defun s3
  (['a 19] "hej"))

(defun s4
  ([18 'y] (tuple "123")))

(defun s5 (x y) 19)

(defun s6-1 (x y) 23)

(defun s6-2 (x y) 23)

(defun is3 (a) a)
