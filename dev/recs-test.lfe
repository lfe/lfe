(defmodule recs-test
  (export all))

;; Defines records #urec{a,b,c}, #trec{a,b,c}.
;;(include-file "include_recs.hrl")
(include-file "include-recs.lfe")

;; Make records functions.
(defun r1 () (make-urec))

(defun r2 (b) (make-trec b (foo:bar b)))

;; Some access functions.
(defun r3 (u) (urec-b u))

(defun r4a (t)				;Setting one
  (set-trec-b t (foo:bar 1)))

(defun r4b (t)
  (set-trec t b (foo:bar 1)))

(defun r5 (t)				;Setting some
  (set-trec t b (foo:bar 1) c (zip:zap 42)))


;; Some access functions for a big record.

(defrecord brec a b c d e f g h i j k l m n o p q r s t u v w x y z)

(defun r6 () (make-brec))

(defun r7 (t) (brec-m t))

(defun r8a (t)				;Setting one
  (set-brec-l t (foo:bar 1)))

(defun r8b (t)
  (set-brec t l (foo:bar 1)))

(defun r9 (t)				;Setting many
  (set-brec t j (foo:bar 1) p (zip:zap 42)))

(defun r10 (t)
  (set-brec t a (foo:bar 2) j (foo:bar 1) p (zip:zap 42) z (zip:zap 1)))
