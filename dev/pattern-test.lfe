(defmodule pattern-test
  (export all))

;; Testing multiple recurring variables.

(defun rv
  ;; Testing with cons and list.
  ([x (cons x xs)] (list 'c-1 x xs))
  ([(cons x _) (cons x xs)] (list 'c-2 x xs))
  ([(cons x _) (list x _)] (list 'c-3 x))
  ([(list x x) (list x y)] (list 'c-4 x y))
  ;; Testing with tuples.
  ([x (tuple x y z)] (list 't-1 x y z))
  ([(tuple x _ _) (tuple x y z)] (list 't-2 x y z))
  ([(tuple _ x x) (tuple x y z)] (list 't-3 x y z))
  ;; Testing with both list and tuples.
  ([(list x _ _) (tuple x y z)] (list 'lt-1 x y z))
  ([(list x _ z) (tuple x y z)] (list 'lt-2 x y z))
  ([(cons x _) (tuple x y z)] (list 'lt-3 x y z))
  ;; Catch all.
  ([x n] (list 'last x n)))

;; Testing aliases that match.

(defun alias (x)
  (case x
    ((= (list a b) (cons h t)) (list 'c-1 a b h t))
    (other (list 'last other))))
