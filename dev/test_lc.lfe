(defmodule test_lc
  (export (a 2) (b 2) (c 2) (d 2)))

(defun a (x y)
  (lc ((<- v x)
       (/= v y))
    v))

(defun b (x y)
  (lc ((<- v x)
       (?= (cons y z) v))               ;Match bind variables
    (list v y z)))

(defun c (x y)
  (lc ((<= (b float) y)                 ;Only bitseg needed, no wrapping
       (<- (tuple v) x))
    (tuple b v)))

(defun d (x y)
  (bc ((<= b y)                         ;Only bitseg needed, no wrapping
       (<- (tuple v) x))
    ((* b v) (size 16))))               ;Only bitseg needed, no wrapping

;; (defun d (x y)
;;   (bc ((<= (b float) y)
;;        (<- (tuple v) x))
;;     ((* b v) float)))
