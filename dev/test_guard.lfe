(defmodule test_guard
  (export all))

(defun b (x) x)

(define (b) '"a string")                ;Old style

(defun c (x y)
  (case (b x)
    ((tuple 'ok z) (when (> z 5)) (d '|(> z 5)| z))
    ;;((tuple 'ok z) (when z) (d 'z z))
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

(defun seq
  ([x y z] (when (> x 0) (is_integer z)) 1)
  ([x y z] (when (> x 0) (=:= (element y z) 10))
   2)
  ([x y z] (when (> x 0) (=:= (element y z) 10) (=:= (element (+ y 1) z) 10))
   3)
  ([x y z] (when (and (> x 0)
		      (and (=:= (element y z) 10)
			   (=:= (element (+ y 1) z) 10))))
   4)
  ([x y z] 999))

(defun lit
  ([x y z] (when 'true) 1)
  ([x y z] (when 'false) 2)
  ([x y z] (when 'x 'y) 3)
  ([x y z] (when (and x y)) 4)
  ([x y z] (when '67 'z) 5)
  ([x y z] (when (and '67 'z)) 6)
  ([x y z] (when 'x '89 'z) 7)
  ([x y z] (when x 89 #(z)) 8)
  ([x y z] (when (and x (and 89 z))) 9)
  ([x y z] 999))
