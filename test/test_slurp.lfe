(define-module test_slurp
  (export )
  (import (from lists (map 2))
	  (rename ordsets ((add_element 2) os-add))))
;; 	  (rename lists ((map 2) foldl))))

(define (fac n)
  (if (> n 0)
    (* (fac (- n 1)) n)
    1))

(define (funny x)
  (list 'funny x -))			;Value of - when function defined

(define (fac1 n)
  (fletrec ((f (lambda (n)
		 (if (> n 0)
		   (* (f (- n 1)) n)
		   1))))
    (f n)))
