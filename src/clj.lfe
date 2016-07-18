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
  ;; Threading macros.
  (export-macro -> ->> as-> cond-> cond->> some-> some->>)
  ;; Conditional macros.
  (export-macro condp if-not when-not not=)
  ;; Predicate macros.
  (export-macro
   tuple? atom? binary? bitstring? bool? float? function? func?
   integer? int? number? record? reference? map? undefined? undef? nil?
   true? false? odd? even? zero? pos? neg? identical?)
  (export
   ;; Function composition.
   (comp 2) (comp 3) (comp 1) (comp 0)
   ;; Partial application.
   (partial 2)
   ;; Predicate functions.
   (string? 1) (unicode? 1)
   (list? 1) (set? 1) (dict? 1) (proplist? 1) (proplist-kv? 1) (queue? 1)
   (empty? 1) (every? 2) (all? 2) (any? 2) (not-any? 2) (element? 2)
   ;; Other functions.
   (identity 1) (constantly 1)))

;;; Threading macros.

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


;;; Conditional macros.

(defmacro condp
  "Usage: `(condp pred expr . clauses)`

  Given a binary predicate, an expression and a set of clauses of the form:

      test-expr result-expr

      test-expr >> result-fn

  where `result-fn` is a unary function, if `(pred test-expr expr)` returns
  anything other than `undefined` or `false`, the clause is a match.

  If a binary clause matches, return `result-expr`.  If a ternary clause
  matches, call `result-fn` with the result of the predicate and return the
  result.

  If no clause matches and a single default expression is given after the
  clauses, return it. If no default expression is given and no clause matches,
  return a tuple of the form:

      #(error \"No matching clause: {{expr}}\")"
  (`(,pred ,expr . ,clauses)
   (fletrec ((falsey? (x) `(lists:member ,x '(undefined false)))
             (emit
              ([pred expr `(,a >> ,c . ,more)]
               `(let ((|-P-| (funcall ,pred ,a ,expr)))
                  (if ,(falsey? '|-P-|)
                    ,(emit pred expr more)
                    (funcall ,c |-P-|))))
              ([pred expr `(,a ,b . ,more)]
               `(if ,(falsey? `(funcall ,pred ,a ,expr))
                  ,(emit pred expr more)
                  ,b))
              ([pred expr `(,a)] a)
              ([pred expr ()]
               `#(error
                  ,(lists:flatten
                    ;; Consider avoiding lfe_io_pretty:term/1, since
                    ;; LFE may not be available at runtime.
                    `("No matching clause: " . ,(lfe_io_pretty:term expr)))))))
     (emit pred expr clauses))))

(defmacro if-not
  "If `test` evaluates to `false`, evaluate and return `then`, otherwise `else`,
  if supplied, else `undefined`."
  (`(,test ,then) `(if-not ,test ,then 'undefined))
  (`(,test ,then . (,else))
   `(if (not ,test) ,then ,else)))

(defmacro when-not
  "If `test` evaluates to `false`, evaluate `body` in an implicit `progn`,
  otherwise if `test` evaluates to `true`, return `undefined`."
  (`(,test . ,body)
   `(if ,test 'undefined (progn ,@body))))

(defmacro not=
  "Same as `(not (== ...))`."
  (`(,x)            'false)
  (`(,x ,y . ,more) `(not (== ,x ,y ,@more))))


;;; Predicate macros.

(defmacro tuple? (x)
  "Return `'true` if `x` is a tuple."
  `(is_tuple ,x))

(defmacro atom? (x)
  "Return `'true` if `x` is an atom."
  `(is_atom ,x))

(defmacro binary? (x)
  "Return `'true` if `data` is a binary."
  `(is_binary ,x))

(defmacro bitstring? (x)
  "Return `'true` if `x` is a bitstring."
  `(is_bitstring ,x))

(defmacro boolean? (x)
  "Return `'true` if `x` is a boolean."
  `(is_boolean ,x))

(defmacro bool? (x)
  "Return `'true` if `x` is a boolean."
  `(is_boolean ,x))

(defmacro float? (x)
  "Return `'true` if `x` is a float."
  `(is_float ,x))

(defmacro function?
  "Return `'true` if `f` is a function."
  (`(,f)    `(is_function ,f))
  (`(,f ,n) `(is_function ,f ,n)))

(defmacro func?
  "Return `'true` if `f` is a function."
  (`(,f)    `(is_function ,f))
  (`(,f ,n) `(is_function ,f ,n)))

(defmacro integer? (x)
  "Return `'true` if `x` is an integer."
  `(is_integer ,x))

(defmacro int? (x)
  "Return `'true` if `x` is an integer."
  `(is_integer ,x))

(defmacro number? (x)
  "Return `'true` if `x` is a number."
  `(is_number ,x))

(defmacro record?
  "Return `'true` if `x` is a tuple and its first element is `record-tag`."
  ;; "`record-tag` must be an atom. Return `'true` if `x` is a tuple,
  ;; its first element is `record-tag`, and its size is `size`."
  (`(,x ,record-tag)       `(is_record ,x ,record-tag))
  (`(,x ,record-tag ,size) `(is_record ,x ,record-tag ,size)))

(defmacro reference? (x)
  "Return `'true` if `x` is a reference."
  `(is_reference ,x))

(defmacro map? (x)
  "Return `'true` if `data` is a map.
  Return `'false` on versions of Erlang without maps."
  `(is_map ,x))

(defmacro undefined? (x)
  "Return `'true` if `x` is the atom `'undefined`."
  `(=:= 'undefined ,x))

(defmacro undef? (x)
  "Return `'true` if `x` is the atom `'undefined`."
  `(=:= 'undefined ,x))

(defmacro nil? (x)
  "Return `'true` if `x` is the atom `'nil` or the empty list."
  `(orelse (=:= 'nil ,x)
           (=:=  ()  ,x)))

(defmacro true? (x)
  "Return `'true` if `x` is the atom `'true`."
  `(=:= 'true ,x))

(defmacro false? (x)
  "Return `'true` if `x` is the atom `'false`."
  `(=:= 'false ,x))

(defmacro odd? (x)
  "Return `'true` if `x` is odd."
  `(=:= 1 ,(rem x 2)))

(defmacro even? (x)
  "Return `'true` if `x` is even."
  `(=:= 0 ,(rem x 2)))

(defmacro zero? (x)
  "Return `'true` if `x` is zero."
  `(== 0 ,x))

(defmacro pos? (x)
  "Return `'true` if `x` is greater than zero."
  `(> ,x 0))

(defmacro neg? (x)
  "Return `'true` if `x` is less than zero."
  `(< ,x 0))

(defmacro identical? (x y)
  "Return `'true` if `x` is exactly equal to `y`."
  `(=:= ,x ,y))


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


;;; Predicate functions.

(defun string? (data)
  "Return `'true` if `data` is a flat list of printable characters."
  (io_lib:printable_list data))

(defun unicode? (data)
  "Return `'true` if `data` is a flat list of printable Unicode characters."
  (io_lib:printable_unicode_list data))

(defun list? (data)
  "Return `'true` if `data` is a list and not a string."
  (andalso (is_list data) (not (string? data))))

(defun set? (data)
  "Return `'true` if `data` is appears to be a (possibly ordered) set."
  (orelse (sets:is_set data)
          (ordsets:is_set data)))

(defun dict?
  "Return `'true` if `data` is a dictionary."
  ((data) (when (=:= 'dict (element 1 data)))
   'true)
  ((_)
   'false))

(defun proplist?
  "Return `'true` if `lst` is a list where [[proplist-kv?/1]] returns `'true`
  for all elements in `lst`."
  ((lst) (when (is_list lst))
   (lists:all #'proplist-kv?/1 lst))
  ((_)
   'false))

(defun proplist-kv?
  "Return `'true` if a given term is a key/value tuple or an atom."
  ((`#(,key ,_)) (when (is_atom key))
   'true)
  ((bool-key) (when (is_atom bool-key))
   'true)
  ((_)
   'false))

(defun queue? (x)
  "Return `'true` if `x` is a queue."
  (queue:is_queue x))

(defun empty? (x)
  "Return `'true` if `x` is the empty list, tuple, map, dictionary, queue, or
  general balanced tree."
  (orelse (=:= () x) (=:= #() x)
          (andalso (map? x) (=:= 0 (call 'maps 'size x)))
          (andalso (dict? x) (dict:is_empty x))
          (andalso (queue? x) (queue:is_empty x))
          (gb_sets:is_empty x)))

(defun every? (pred lst)
  "Return `'true` if `(pred x)` returns `'true` for every `x` in `lst`."
  (lists:all pred lst))

(defun all? (pred lst)
  "Return `'true` if `(pred x)` returns `'true` for every `x` in `lst`."
  (lists:all pred lst))

(defun any? (pred lst)
  "Return `'true` if `(pred x)` returns `'true` for any `x` in `lst`."
  (lists:any pred lst))

(defun not-any? (pred lst)
  "Return `'false` if `(pred x)` returns `'true` for any `x` in `lst`."
  (not (lists:any pred lst)))

(defun element?
  "Return `'true` if `elem` is an element of `data`, where `data` is a list,
  set or ordset."
  ((elem data) (when (is_list data))
   (lists:member elem data))
  ((elem data)
   (cond
    ((sets:is_set data)
     (sets:is_element elem data))
    ((ordsets:is_set data)
     (ordsets:is_element elem data))
    ('true 'false))))


;;; Other functions.

(defun identity (x)
  "Identity function."
  x)

(defun constantly (x)
  "Return a unary function that returns `x`.
  N.B. This is like Haskell's `const` rather than Clojure's `constantly`."
  (lambda (_) x))
