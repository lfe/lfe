(defmodule record-test
  (export all)
  )

(defrecord point (x 0) (y (element 3 (date))))

(defun a (x y r)
  (list (make-point x (date))
        (make-point x (binary 34 (x float) ((+ y 3) float)))))

(defun b (x y r)
  (let (((match-point y yy x xx) (make-point x x)))
    (list xx yy (make-point y r))))

(defrecord circle (center (make-point)) (radius 0))

(defun c (x y r)
  (let ((c (make-point x 42)))
    (make-circle center c radius r)))

(defun d (p c)
  (tuple (point-x p) (circle-radius c)))

(defun e (p c)
  (tuple (case p
           ((match-point x x) x)
           (_ (error #(badrecord point))))
         (case c
           ((match-circle radius radius) radius)
           (_ (error #(badrecord circle))))))
