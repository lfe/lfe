(defmodule test_guard
  (export (b 1) (c 2) (e 2) (f 2) (if-test 2)))

(defun b (x) x)

(define (b) '"a string")        ;Old style

(defun c (x y)
  (case (b x)
    ((tuple 'ok z) (when (> z 5)) (d '|(> z 5)| z))
    ((tuple 'ok z) (when z) (d 'z z))
    ((tuple 'ok z) (when (+ z z)) (d '|(+ z z)| z))
    ((tuple 'ok z) (when (== z 'true)) (d '|(== z true)| z))
    ((tuple 'ok z) (when (and z 'true)) (d '|(and z true)| z))
    ((tuple 'ok z) (when (and z x)) (d '|(and z x)| z))
    ((tuple 'ok z) (when (or (or (> z 5) x) (/= z 7))) (d 'or z))
    ((tuple 'ok z) (when (and (and (> z 5) x) (/= z 7))) (d 'and z))
    ((tuple 'ok z) (when (orelse (> z 5) x (/= z 7))) (d 'orelse z))
    ((tuple 'ok z) (when (andalso (> z 5) x (/= z 7))) (d 'andalso z))
    ((tuple 'ok z) (d 'nul z))
    (#(1 2) (d '|#(1 2)| 'z))))

(defun d (x y) (list (b) x y))

(defun e (x y)
  (case (b x)
    (#(ok z) (d '|#(ok z)| 'z))
    ((tuple 'ok z) (when (andalso (> (+ z 1) 5)
                  (orelse x (> (+ z 1) 3))
                  (/= z 7)))
     (d 'andalso z))))

(defun f (x y)
  (case x
    ((tuple 'ok b) (when (=:= b (binary 1 2 3))) 'yes)
    (_ 'no)))

(defun if-test
  ([x y] (when (if (> (+ x y) 10) 'true
		   'false)) 1)
  ([x y] 2))
