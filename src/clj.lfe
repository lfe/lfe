;; Copyright (c) 2015-2016 Robert Virding
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; File    : clj.lfe
;; Author  : Tim Dysinger, Duncan McGreggor, Eric Bailey
;; Purpose : LFE Clojure interface library.

(defmodule clj
  "LFE Clojure interface library."
  (export
   ;; Function composition.
   (comp 2) (comp 3) (comp 1) (comp 0)
   ;; Partial application.
   (partial 2)
   ;; Other functions.
   (identity 1))
  ;; Threading macros
  (export-macro -> ->> as-> cond-> cond->> some-> some->>))

;;; Function composition.

(defun comp
  "Function composition.
  If the second argument is a function, compose `f` and `g`.
  Otherwise, compose a list of functions `fs` and apply the result to `x`."
  ((f g) (when (is_function g))
   (lambda (x)
     (funcall f (funcall g x))))
  ((fs x)
   (funcall (comp fs) x)))

(defun comp (f g x)
  "Equivalent to `(funcall (comp f g) x)`."
  (funcall (comp f g) x))

(defun comp (fs)
  "Compose a list of functions right to left."
  (lists:foldr #'comp/2 #'identity/1 fs))

(defun comp ()
  "Equivalent to `#'identity/1`."
  #'identity/1)


;;; Partial application.

(defun partial
  "Partial application.
  Given a function `f`, and an argument or list of arguments, return a function
  that applies `f` to the given argument(s) plus (an) additional argument(s)."
  ((f args-1) (when (is_list args-1))
   (match-lambda
     ((args-2) (when (is_list args-2))
      (apply f (++ args-1 args-2)))
     ((arg)
      (apply f (++ args-1 `(,arg))))))
  ((f arg-1)
   (match-lambda
     ((args) (when (is_list args))
      (apply f (cons arg-1 args)))
     ((arg-2)
      (funcall f arg-1 arg-2)))))


;;; Other functions

(defun identity (x)
  "Identity function."
  x)


;;; Threading macros

;; Macro helper functions
(eval-when-compile
  (defun ->*
    ([`(,x)]                     x)
    ([`(,x ,(= `(fun . ,_) f))] `(funcall ,f ,x))
    ([`(,x (quote (,y . ,ys)))] `(list* ',y ,x ',ys))
    ([`(,x (quote ,y))]         `(list ',y ,x))
    ([`(,x (,sexp . ,sexps))]   `(,sexp ,x ,@sexps))
    ([`(,x ,sexp)]              `(list ,sexp ,x))
    ([`(,x ,sexp . ,sexps)]
     (->* (cons (->* (list x sexp)) sexps))))
  (defun ->>*
    ([`(,x)]                      x)
    ([`(,x ,(= `(fun . ,_) f))] `(funcall ,f ,x))
    ([`(,x (quote (,y . ,ys)))] `(list* ',y ',@ys (list ,x)))
    ([`(,x (quote ,y))]         `(list ',y ,x))
    ([`(,x (,f . ,sexps))]      `(,f ,@sexps ,x))
    ([`(,x ,sexp)]              `(list ,sexp ,x))
    ([`(,x ,sexp . ,sexps)]
     (->>* (cons (->>* (list x sexp)) sexps))))
  (defun as->*
    ([`(,x ,_)]           x)
    ([`(,x ,name ,sexp)] `(let ((,name ,x)) ,sexp))
    ([`(,x ,name ,sexp . ,sexps)]
     (as->* (list* (as->* (list x name sexp)) name sexps))))
  (defun cond->*
    ([`(,x)]              x)
    ([`(,x ,_)]           (error "cond-> requires test/sexp pairs."))
    ([`(,x ,test ,sexp)] `(if ,test ,(->* (list x sexp)) ,x))
    ([`(,x ,test ,sexp . ,clauses)]
     (cond->* (cons (cond->* (list x test sexp)) clauses))))
  (defun cond->>*
    ([`(,x)]              x)
    ([`(,x ,_)]           (error "cond->> requires test/sexp pairs."))
    ([`(,x ,test ,sexp)] `(if ,test ,(->>* (list x sexp)) ,x))
    ([`(,x ,test ,sexp . ,clauses)]
     (cond->>* (cons (cond->>* (list x test sexp)) clauses))))
  (defun some->*
    ([`(,x)] x)
    ([`(,x ,sexp)]
     (case x
       ('undefined 'undefined)
       (|-X-|       (->* (list |-X-| sexp)))))
    ([`(,x ,sexp . ,sexps)]
     (some->* (cons (some->* (list x sexp)) sexps))))
  (defun some->>*
    ([`(,x)] x)
    ([`(,x ,sexp)]
     (case x
       ('undefined 'undefined)
       (|-X-|       (->>* (list |-X-| sexp)))))
    ([`(,x ,sexp . ,sexps)]
     (some->>* (cons (some->>* (list x sexp)) sexps)))))

(defmacro -> args
  "x . sexps
  Thread `x` through `sexps`. Insert `x` as the second item in the first `sexp`,
  making a list of it if it is not a list already. If there are more `sexps`,
  insert the first `sexp` as the second item in second `sexp`, etc."
  (->* args))

(defmacro ->> args
  "x . sexps
  Thread `x` through `sexps`. Insert `x` as the last item in the first `sexp`,
  making a list of it if it is not a list already. If there are more `sexps`,
  insert the first `sexp` as the last item in second `sexp`, etc."
  (->>* args))

(defmacro as-> args
  "expr name . sexps
  Bind `name` to `expr`, evaluate the first `sexp` in the lexical context of
  that binding, then bind `name` to that result, repeating for each successive
  `sexp` in `sexps`, returning the result of the last `sexp`."
  (as->* args))

(defmacro cond-> args
  "expr . clauses
  Given an `expr`ession and a set of `test`/`sexp` pairs, thread `x` (via `->`)
  through each `sexp` for which the corresponding `test` expression is `true`.
  Note that, unlike `cond` branching, `cond->` threading does not short circuit
  after the first `true` test expression."
  (cond->* args))

(defmacro cond->> args
  "expr . clauses
  Given an `expr`ession and a set of `test`/`sexp` pairs, thread `x` (via `->>`)
  through each `sexp` for which the corresponding `test` expression is `true`.
  Note that, unlike `cond` branching, `cond->>` threading does not short circuit
  after the first `true` `test` expression."
  (cond->>* args))

(defmacro some-> args
  "x . sexps
  When `x` is not `undefined`, thread it into the first `sexp` (via `->`),
  and when that result is not `undefined`, through the next, etc."
  (some->* args))

(defmacro some->> args
  "x . sexps
  When `x` is not `undefined`, thread it into the first sexp (via `->>`),
  and when that result is not `undefined`, through the next, etc."
  (some->>* args))
