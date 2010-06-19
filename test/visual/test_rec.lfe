(defmodule test_rec
  ;;(export (a 3))
  (export all)
  )

(defrecord point (x 0) (y (element 3 (now))))

(defun a (x y r)
  (list
   (make-point x (now))
   (make-point x (binary 34 (x float) ((+ y 3) float)))))

  ;;  (let ((n (now)))
  ;;    (element 2 n)))
  ;;   (make-point x x))

(defun b (x y r)
  (let (((match-point y yy x xx)(make-point x x)))
    (list xx yy (make-point y r))))

(defrecord circle (center (make-point)) (radius 0))

(defun c (x y r)
  (let ((c (make-point x 42)))
    (make-circle center c radius r)))
