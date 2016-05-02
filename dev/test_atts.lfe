;; Test that the attributes are handled as they should be.
;; - doc is ignored
;; - export all propagates properly

(defmodule test_atts
  (doc "The doc string should be ignored!")
  (export (foo 1))
  (export all)				;This should propagate
  (export (bar 1)))

(defun foo (x)
  (list x))

(defun bar (x)
  (tuple x))
