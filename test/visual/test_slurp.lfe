(defmodule test_slurp
  (export )
  (import (from lists (map 2))
          (rename ordsets ((add_element 2) os-add))))
;;          (rename lists ((map 2) foldl))))

(defun fac (n)
  (if (> n 0)
    (* (fac (- n 1)) n)
    1))

(defun funny (x)
  (list 'funny x -))                    ;Value of - when function defined

(defun fac1 (n)
  (fletrec ((f (n)
               (if (> n 0)
                 (* (f (- n 1)) n)
                 1)))
    (f n)))
