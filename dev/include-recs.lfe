;; Include file defining records for testing the expansion of records.

;; Untyped and types record definitions.

(defrecord urec
  (a 49)
  b
  (c (x:y 1)))

(defrecord trec
  (a 49 (integer))
  (b 'undefined (list (string)))
  (c (x:y 1) (tuple)))
