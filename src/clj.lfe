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
   (compose 2) (compose 3) (compose 1) (compose 0)
   ;; Partial application.
   (partial 2)
   ;; Other functions.
   (identity 1)))

;;; Function composition.

(defun compose
  "Function composition.
  If the second argument is a function, compose `f` and `g`.
  Otherwise, compose a list of functions `fs` and apply the result to `x`."
  ((f g) (when (is_function g))
   (lambda (x)
     (funcall f
       (funcall g x))))
  ((fs x)
   (funcall (compose fs) x)))

(defun compose (f g x)
  "Equivalent to `(funcall (compose f g) x)`."
  (funcall (compose f g) x))

(defun compose (fs)
  "Compose a list of functions right to left."
  (lists:foldr #'compose/2 #'identity/1 fs))

(defun compose ()
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
