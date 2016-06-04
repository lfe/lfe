;; Test that the function docs are handled as they should be.

(defmodule test_docs
  "Testing function docs.
   This line and the next are indented 3.
   We have the functions one/3, two/3, three/3, four/3, five/3, six/3"
  (export (one 3) (two 3) (three 3)
          (four 3) (five 3) (six 3)))

(defun one (x y z)
  "A simple one line doc string."
  (list x y z ))

(defun two (x y z)
  "x y z
   3 lines where the 2nd is indented 3
   and the 3rd has the same indentation."
  (list x y z))

(defun three (x y z)
  "x y z
    4 lines where this is indented 4
    this is indented 4 as well
  while this is one is indented only 2
      and this one is indented 6."
  (list x y z))

(defun four (x y z)
  "x y z
   1 line indented 3

   indented 3 just skipped a blank line."
  (list x y z))

(defun five (x y z)
  "

   x y z
   there were 2 blank lines before the arg line
   which was indented 3

   as is this one after a blank line."
  (list x y z))

(define-function six
  ((doc "x y z
         3 lines where the 2nd is indented 9
         and the 3rd has the same indentation.")
   (doc "This is the second doc string of 4"
        "where this is in the same doc as 2")
   (doc "x y z
    1 line indented 4

    indented 4 just skipped a blank line."))
  (lambda (x y z) (list x y z)))
  