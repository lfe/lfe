(defmodule example
  "This is an example module."
  (export (matchfun 3))
  (export-macro add varargs forty-two? tricky))

(defrecord foo bar baz)

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
  (`(,x)         'one)
  (`(,x ,y)      'two)
  (`(,x ,y . ,z) 'many))

(define-function subtract
  ((doc "Subtract `y` from `x`."))      ;The function meta data
  (lambda (x y) (- x y)))

(extend-module
  ()
  ((export (subtract 2))))

(defun matchfun
  "This is a function with pattern clauses."
  ([x y z] (when (< x y) (< y z))     'lt)
  ([x y z] (when (> x y) (> y z))     'gt)
  ([x y z] (when (=:= x y) (=:= y z)) 'eq)
  ([_ _ _]                            'idk))


(defmodule another-example)

(defun divmod (n d)
  ;; Deliberately no docstring here.
  (tuple (div n d) (rem n d)))

(extend-module
  ((doc "This is another example."))
  ((export (divmod 2))))
