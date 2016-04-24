(defmodule example
  (doc "This is an example module.")
  (export all)
  (export-macro add varargs forty-two?))

(defmacro add (x y)
  "Add `x` and `y`."
  `(+ ,x ,y))

(defmacro varargs args
  "This is a varags macro."
  `'ok)

(defmacro forty-two?
  "Return `'true` iff `x` is 42."
  (`(,x) (when (=:= x 42)) `'yes)
  (`(,x) (when (< x 42))   `'small)
  (`(,x) (when (> x 42))   `'big))

(defmacro tricky
  "This is a tricky varargs macro."
  ([x]       'one)
  ([x y]     'two)
  ([x y . z] 'many))

(defun subtract (x y)
  "Subtract `y` from `x`."
  (- x y))

(defun matchfun
  "This is a function with pattern clauses."
  ([x y z] (when (< x y) (< y z))     'lt)
  ([x y z] (when (> x y) (> y z))     'gt)
  ([x y z] (when (=:= x y) (=:= y z)) 'eq)
  ([_ _ _]                            'idk))

(defmodule another-example
  ;; Deliberately no doc here.
  (export all))

(defun divmod (n d)
  ;; Deliberately no docstring here.
  (tuple (div n d) (rem n d)))
