(defmodule test_rec
  ;;(export (a 3))
  (export all)
  )

(defrecord point (x 0) (y (element 3 (now))))

(defun a (x y r)
  (setelement 2 (now) 42))

;;  (let ((n (now)))
;;    (element 2 n)))
;;  (make-point x x))

;;  (let (((match-point y yy x xx)(make-point x x)))
;;    (list xx yy (make-point y r))))

;; (defrecord circle (center (make-point)) (radius 0))

;; (defun a (x y r)
;;   (let ((c (make-point x 42)))
;;     (make-circle center c x 5)))
