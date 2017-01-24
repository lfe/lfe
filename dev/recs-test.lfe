(defmodule recs-test
  (export (r1 0) (r2 0) (r3 1) (r4 1)))

;;(include-file "include_recs.hrl")
(include-file "include-recs.lfe")

(defun r1 () (make-urec))

(defun r2 () (make-trec))

;; Some simple access functions.
(defun r3 (u) (urec-b u))

(defun r4 (t) (set-trec t b (foo:bar 1) c (zip:zap 42)))
