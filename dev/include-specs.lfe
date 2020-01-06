;; File for testing expansion of function specifications.
;; We specify functions a/0, b/1, c/2, d/2.

(deftype st (tuple 'st (list) (list)))

(defspec (s1 0) (() (integer)))

(defspec (s2 1) (((any)) 42))

(defspec (s3 2)
  (((atom) (integer)) (string))
  )

(defspec (s4 2)
  (((atom) (integer)) (string))
  (((integer) (atom)) (st)))

(defspec (s5 2)
  ((x y) (tuple 'ok z) ((x (integer)) (y (atom)) (z (any)))))

(defspec (s6-1 2)
  ([(lambda ((any)) (boolean)) (list)] (list)))

(defspec (s6-2 2)
  ([pred (list)] (list) [(pred (lambda ((any)) (boolean)))]))

;; Bad func specs
;;(defspec (s2 1) (((any)) (any)))        ;Respecing (s2 1)
;;(defspec (is2 2)                        ;Undefined constraints y, z
;;  ((x y) z ((x (integer)))))
;;(defspec (is3 1)
;;  ([x] y [(x (atom)) (y (atom)) (z (integer))])
;;  ([a] b [(a (integer))]))
