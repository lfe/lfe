;; Test that the attributes are handled as they should be.
;; - doc is ignored
;; - export all propagates properly

(defmodule test_atts
  "The real doc string."
  (doc "This deprecated doc string should be ignored!")
  (doc "Another deprecated doc string which should be ignored!")
  (another "attribute")
  (export (foo 1))
  (export all)                          ;This should propagate
  (export (bar 1)))

(defun foo (x)
  "Foo has a doc string."
  (list x))

(extend-module
  ((doc "More docs"))
  ((doc "Yet another deprecated doc string which should be ignored!")))

(extend-module
  ()
  ((doc "More deprecated docs!" "Many in one doc!."
        "Three doc strings in one doc in fact.")))

(defun bar (x)
  (tuple x))
