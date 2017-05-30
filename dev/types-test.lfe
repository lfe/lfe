;; Test the handling of types and record definitions.

(defmodule types-test
  (export all))

;; Include erlang types and record definitions.
;; (include-file "include_types.hrl")
(include-file "include-types.lfe")

(defun a () 42)
