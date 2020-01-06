;; Copyright (c) 2008-2015 Robert Virding
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

;; File    : core-macros.lfe
;; Author  : Robert Virding
;; Purpose : Some of LFE core macrosas they could have been implemented in LFE.

;; This file contains some of the core LFE macros as they could have
;; been implemented in LFE. They are implemented in lfe_macro.erl
;; today as there is yet no way to compile macros, and to ensure that
;; they are always available. These are shown mostly without comments.
;;
;; For some macros we give two versions, an all-in-one version and a
;; recursive more syntax pattern based expansion. This to show
;; different styles of doing the same thing.

(defmacro caar (x) `(car (car ,x)))
(defmacro cadr (x) `(car (cdr ,x)))
(defmacro cdar (x) `(cdr (car ,x)))
(defmacro cddr (x) `(cdr (cdr ,x)))

(defmacro ++
  ;; Cases with quoted lists.
  ((cons (list 'quote (cons a as)) es) `(cons ',a (++ ',as . ,es)))
  ((cons (list 'quote ()) es) `(++ . ,es))
  ;; Cases with explicit cons/list/list*.
  ((cons (list 'list* a) es) `(++ ,a . ,es))
  ((cons (cons 'list* (cons a as)) es) `(cons ,a (++ (list* . ,as) . ,es)))
  ((cons (cons 'list (cons a as)) es) `(cons ,a (++ (list . ,as) . ,es)))
  ((cons (list 'list) es) `(++ . ,es))
  ((cons (list 'cons h t) es) `(cons ,h (++ ,t . ,es)))
  ((cons () es) `(++ . ,es))
  ;; Default cases with unquoted arg.
  ((list e) e)
  ((cons e es) `(call 'erlang '++ ,e (++ . ,es)))
  (() ()))

(defmacro :
  ((list* m f as) `(call ',m ',f . ,as)))

(defmacro ?
  ((list to default)
   `(receive (omega omega) (after ,to ,default)))
  ((list to) `(? ,to (exit 'timeout)))
  (() `(receive (omega omega))))

(defmacro list*
  ((list e) e)
  ((cons e es) `(cons ,e (list* . ,es)))
  (() ()))

;; (defmacro let*
;;   ((cons vbs b)
;;    (: lists foldr
;;      (lambda (vb rest) `(let (,vb) ,rest)) `(progn . ,b) vbs)))

(defmacro let*
  ((cons (cons vb vbs) b) `(let (,vb) (let* ,vbs . ,b)))
  ((cons () b) `(progn . ,b))
  ((cons vb b) `(let ,vb . b)))         ;Pass error to let

(defmacro flet*
  ((cons (cons fb fbs) b) `(flet (,fb) (flet* ,fbs . ,b)))
  ((cons () b) `(progn . ,b))
  ((cons fb b) `(flet ,fb . b)))        ;Pass error to flet

(defmacro cond
  ((list (cons 'else b)) `(progn . ,b))
  ((cons (cons (list '?= p e) b) cond)
   `(case ,e (,p . ,b) (_ (cond . ,cond))))
  ((cons (cons (list '?= p (= (cons 'when _) g) e) b) cond)
   `(case ,e (,p ,g . ,b) (_ (cond . ,cond))))
  ((cons (cons test b) cond) `(if ,test (progn . ,b) (cond . ,cond)))
  (() `'false))

(defmacro andalso
  ((list e) e)
  ((cons e es) `(if ,e (andalso . ,es) 'false))
  (() `'true))

(defmacro orelse args            ;Using single argument
  (case args
    ((list e) e)
    ((cons e es) `(if ,e 'true (orelse . ,es)))
    (() `'false)))

;; This version of backquote is almost an exact copy of a quasiquote
;; expander for Scheme by André van Tonder. It is very compact and
;; with some cons/append optimisations we have added produces quite
;; reasonable code.

(defmacro backquote (e) (bq-expand e 0))

(eval-when-compile
  (defun bq-expand (exp n)
    ;; Note that we cannot *use* backquote or any macros using
    ;; backquote in here! It will cause us to loop.
    (fletrec ((bq-app                           ;Optimise append
               ([(list '++ l) r] (bq-app l r))  ;Catch single comma-at
               ([() r] r)
               ([l ()] l)
               ([(list 'list l) (cons 'list r)] (cons 'list (cons l r)))
               ([(list 'list l) r] (list 'cons l r))
               ([l r] (list '++ l r)))
              (bq-cons                  ;Optimise cons
               ([(list 'quote l) (list 'quote r)] (list 'quote (cons l r)))
               ([l (cons 'list r)] (cons 'list (cons l r)))
               ([l ()] (list 'list l))
               ([l r] (list 'cons l r))))
      (case exp
        ((list 'backquote x)
         ;; `(list 'backquote ,(bq-expand x (+ n 1)))
         (list 'list (list 'quote 'backquote) (bq-expand x (+ n 1))))
        ((list 'comma x) (when (> n 0))
         (bq-cons (list 'quote 'comma) (bq-expand x (- n 1))))
        ((list 'comma x) (when (=:= n 0)) x)
        ((cons 'comma-at x) (when (> n 0))
         (bq-cons (list 'quote 'comma-at) (bq-expand x (- n 1))))
        ;; The next two cases handle splicing into a list.
        ((cons (cons 'comma x) y) (when (=:= n 0))
         (bq-app (cons 'list x) (bq-expand y 0)))
        ((cons (cons 'comma-at x) y) (when (=:= n 0))
         (bq-app (cons '++ x) (bq-expand y 0)))
        ((cons x y)                     ;The general list case
         (bq-cons (bq-expand x n) (bq-expand y n)))
        (x (when (is_tuple x))
           ;; Tuples need some smartness for efficient code to handle
           ;; when no splicing so as to avoid list_to_tuple.
           (case (bq-expand (tuple_to_list x) n)
             ((cons 'list es) (cons 'tuple es))
             ((= (cons 'cons _) e) (list 'list_to_tuple e))))
        (x (when (is_atom x)) (list 'quote x))
        (x x))                          ;Self quoting
      )))
