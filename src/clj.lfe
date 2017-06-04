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
  ;; Function macros.
  (export-macro defn defn- fn)
  ;; Threading macros.
  (export-macro -> ->> as-> cond-> cond->> some-> some->> doto)
  ;; Conditional macros.
  (export-macro if-let iff-let condp if-not iff when-not not=)
  ;; Predicate macros.
  (export-macro
   tuple? atom? binary? bitstring? boolean? bool? float? function? func?
   integer? int? number? record? reference? map? undefined? undef? nil?
   true? false? falsy? odd? even? zero? pos? neg? identical?)
  ;; Other macros.
  (export-macro str lazy-seq conj)
  ;; Clojure-inspired if macro.
  (export-macro if))

(defmacro HAS_MAPS () (quote (erl_internal:bif 'is_map 1)))

;;; Function macros.

(defmacro defn
  "name [arg ...] {{doc-string}} body
   name {{doc-string}} ([argpat ...] body) ...)
  Define and automatically export a function."
  (`[,name . ,rest]
   (let* ((|-DEFUN-| `(defun ,name ,@rest))
          ;; This is basically lfe_doc:get_function_patterns/1.
          (|-ARITY-|  (case (lists:last (lfe:macroexpand-1 |-DEFUN-|))
                        (`(match-lambda (,|-PAT-| . ,_) . ,_)
                         (length |-PAT-|))
                        (`(lambda ,|-ARGS-| . ,_)
                         (length |-ARGS-|)))))
     `(progn ,|-DEFUN-| (extend-module () ((export (,name ,|-ARITY-|))))))))

(defmacro defn- args
  "name [arg ...] {{doc-string}} body
   name {{doc-string}} ([argpat ...] body) ...)
  Equivalent to `defun`."
  `(defun ,@args))

(defmacro fn args
  "Equivalent to `lambda`."
  `(lambda ,@args))

;;; Threading macros.

;; Macro helper functions
(eval-when-compile
  (defn- ->*
    ([`(,x)]                     x)
    ([`(,x ,(= `(fun . ,_) f))] `(funcall ,f ,x))
    ([`(,x (quote (,y . ,ys)))] `(list* ',y ,x ',ys))
    ([`(,x (quote ,y))]         `(list ',y ,x))
    ([`(,x (,sexp . ,sexps))]   `(,sexp ,x ,@sexps))
    ([`(,x ,sexp)]              `(list ,sexp ,x))
    ([`(,x ,sexp . ,sexps)]
     (->* (cons (->* (list x sexp)) sexps))))
  (defn- ->>*
    ([`(,x)]                      x)
    ([`(,x ,(= `(fun . ,_) f))] `(funcall ,f ,x))
    ([`(,x (quote (,y . ,ys)))] `(list* ',y ',@ys (list ,x)))
    ([`(,x (quote ,y))]         `(list ',y ,x))
    ([`(,x (,f . ,sexps))]      `(,f ,@sexps ,x))
    ([`(,x ,sexp)]              `(list ,sexp ,x))
    ([`(,x ,sexp . ,sexps)]
     (->>* (cons (->>* (list x sexp)) sexps))))
  (defn- as->*
    ([`(,x ,_)]           x)
    ([`(,x ,name ,sexp)] `(let ((,name ,x)) ,sexp))
    ([`(,x ,name ,sexp . ,sexps)]
     (as->* (list* (as->* (list x name sexp)) name sexps))))
  (defn- cond->*
    ([`(,x)]              x)
    ([`(,x ,_)]           (error "cond-> requires test/sexp pairs."))
    ([`(,x ,test ,sexp)] `(case ,test
                            ('false     ,x)
                            ('undefined ,x)
                            (_          ,(->* (list x sexp)))))
    ([`(,x ,test ,sexp . ,clauses)]
     (cond->* (cons (cond->* (list x test sexp)) clauses))))
  (defn- cond->>*
    ([`(,x)]              x)
    ([`(,x ,_)]           (error "cond->> requires test/sexp pairs."))
    ([`(,x ,test ,sexp)] `(case ,test
                            ('false     ,x)
                            ('undefined ,x)
                            (_          ,(->>* (list x sexp)))))
    ([`(,x ,test ,sexp . ,clauses)]
     (cond->>* (cons (cond->>* (list x test sexp)) clauses))))
  (defn- some->*
    ([`(,x)]        x)
    ([`(,x ,sexp)] `(if (=:= 'undefined ,x) 'undefined ,(->* (list x sexp))))
    ([`(,x ,sexp . ,sexps)]
     (some->* (cons (some->* (list x sexp)) sexps))))
  (defn- some->>*
    ([`(,x)]        x)
    ([`(,x ,sexp)] `(if (=:= 'undefined ,x) 'undefined ,(->>* (list x sexp))))
    ([`(,x ,sexp . ,sexps)]
     (some->>* (cons (some->>* (list x sexp)) sexps))))
  (defn- emit
    ([pred expr `(,a >> ,c . ,more)]
     `(case (funcall ,pred ,a ,expr)
        ('false     ,(emit pred expr more))
        ('undefined ,(emit pred expr more))
        (|-P-|       (funcall ,c |-P-|))))
    ([pred expr `(,a ,b . ,more)]
     `(case (funcall ,pred ,a ,expr)
        ('false     ,(emit pred expr more))
        ('undefined ,(emit pred expr more))
        (_          ,b)))
    ([pred expr `(,a)]  a)
    ([pred expr  ()]   `(error 'no-matching-clause (list ,expr))))
  (defn- condp* ([`(,pred ,expr . ,clauses)] (emit pred expr clauses))))

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
  through each `sexp` for which the corresponding `test` expression is truthy,
  i.e. neither `'false` nor `'undefined`.
  Note that, unlike `cond` branching, `cond->` threading does not short circuit
  after the first truthy test expression."
  (cond->* args))

(defmacro cond->> args
  "expr . clauses
  Given an `expr`ession and a set of `test`/`sexp` pairs, thread `x` (via `->>`)
  through each `sexp` for which the corresponding `test` expression is truthy,
  i.e. neither `'false` nor `'undefined`.
  Note that, unlike `cond` branching, `cond->>` threading does not short circuit
  after the first thruthy `test` expression."
  (cond->>* args))

(defmacro some-> args
  "x . sexps
  When `x` is not `'undefined`, thread it into the first `sexp` (via `->`),
  and when that result is not `'undefined`, through the next, etc."
  (some->* args))

(defmacro some->> args
  "x . sexps
  When `x` is not `'undefined`, thread it into the first `sexp` (via `->>`),
  and when that result is not `'undefined`, through the next, etc."
  (some->>* args))

(defmacro doto
  "Evaluate all given `sexps` and functions in order,
  for their side effects, with the value of `x` as the first argument
  and return `x`."
  (`(,x . ,sexps)
   `(let ((,'x* ,x))
      ,@(lists:map
          (match-lambda
            ([`(,f . ,args)] `(,f ,'x* ,@args))
            ([f]             `(,f ,'x*)))
          sexps)
      ,'x*)))

;;; Conditional macros.

(defmacro if-let args
  "((patt test)) then {{else}}
  If `test` evaluates to anything other than `'false` or `'undefined`,
  evaluate `then` with `patt` bound to the value of `test`,
  otherwise `else`, if supplied, else `'undefined`."
  (flet ((exp-if-let (patt test then else)
           `(case ,test
              ('false     ,else)
              ('undefined ,else)
              (,patt      ,then))))
    (case args
      ((list (list patt test) then) (exp-if-let patt test then `'undefined))
      ((list (list patt test) then else) (exp-if-let patt test then else)))))

(defmacro iff-let
  "((patt test)) . body
  When `test` evaluates to anything other than `'false` or `'undefined`,
  evaluate `body` with `patt` bound to the value of `test`,
  otherwise return `'undefined`."
  (`(((,patt ,test)) . ,body)
   `(case ,test
      ('false     'undefined)
      ('undefined 'undefined)
      (,patt      ,@body))))

(defmacro condp args
  "pred expr . clauses
  Given a binary predicate, an expression and a set of clauses of the form:

      test-expr result-expr

      test-expr >> result-fn

  where `result-fn` is a unary function, if `(pred test-expr expr)` returns
  anything other than `'undefined` or `'false`, the clause is a match.

  If a binary clause matches, return `result-expr`. If a ternary clause
  matches, call `result-fn` with the result of the predicate and return the
  result.

  If no clause matches and a single default expression is given after the
  clauses, return it. If no default expression is given and no clause matches,
  throw a `no-matching-clause` error."
  (condp* args))

(defmacro if-not args
  "test then [else]
  If `test` evaluates to `'false` or `'undefined`, evaluate and return `then`,
  otherwise `else`, if supplied, else `'undefined`."
  (flet ((exp-if-not (test then else)
           `(case ,test
              ('false     ,then)
              ('undefined ,then)
              (_          ,else))))
    (case args
      ((list test then)      (exp-if-not test then `'undefined))
      ((list test then else) (exp-if-not test then else)))))

(defmacro iff
  "test . body
  Like Clojure's `when`.
  If `test` evaluates to anything other than `'false` or `'undefined`,
  evaluate `body` in an implicit `progn`. Otherwise, return `'undefined`."
  (`(,test . ,body)
   `(case ,test
      ('false     'undefined)
      ('undefined 'undefined)
      (_          (progn ,@body)))))

(defmacro when-not
  "test . body
  If `test` evaluates to `'false` or `'undefined`, evaluate `body`
  in an implicit `progn`. Otherwise return `'undefined`."
  (`(,test . ,body)
   `(case ,test
      ('false     (progn ,@body))
      ('undefined (progn ,@body))
      (_          'undefined))))

(defmacro not=
  "Same as `(not (== ...))`."
  (`(,x)            `'false)
  (`(,x ,y . ,more) `(not (== ,x ,y ,@more))))


;;; Predicate macros.

(defmacro tuple? (x)
  "Return `'true` if `x` is a tuple."
  `(is_tuple ,x))

(defmacro atom? (x)
  "Return `'true` if `x` is an atom."
  `(is_atom ,x))

(defmacro binary? (x)
  "Return `'true` if `x` is a binary."
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
  "Return `'true` if `f` is a function.
  If `n` is given, return whether `f` is an `n`-ary function."
  (`(,f)    `(is_function ,f))
  (`(,f ,n) `(is_function ,f ,n)))

(defmacro func?
  "Return `'true` if `f` is a function.
  If `n` is given, return whether `f` is an `n`-ary function."
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
  "Return `'true` if `x` is a tuple and its first element is `record-tag`.
  If `size` is given, check that `x` is a `record-tag` record of size `size`.

  N.B. `record?/2` may yield unexpected results, due to difference between the
  Erlang and LFE compilers. As such, whenever possible, prefer `record?/3`."
  ;; NOTE: record-tag must be an atom
  (`(,x ,record-tag)       `(is_record ,x ,record-tag))
  (`(,x ,record-tag ,size) `(is_record ,x ,record-tag ,size)))

(defmacro reference? (x)
  "Return `'true` if `x` is a reference."
  `(is_reference ,x))

(defmacro map? (x)
  "Return `'true` if `data` is a map.
  Return `'false` on versions of Erlang without maps."
  (if (HAS_MAPS)
    `(call 'erlang 'is_map ,x)
    `'false))

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

(defmacro falsy? (x)
  "Return `'true` if `x` is one of the atoms `'false` and `'undefined`."
  `(orelse (=:= 'false ,x) (=:= 'undefined ,x)))

(defmacro odd? (x)
  "Return `'true` if `x` is odd."
  `(not (clj:even? ,x)))

(defmacro even? (x)
  "Return `'true` if `x` is even."
  `(clj:zero? ,(band 1 x)))

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

;;; Other macros.

(defmacro str args
  "Construct a string from an arbitrary number of scalar values."
  `(lists:flatmap
     (lambda (arg)
       (clj:cond-> arg
         (not (clj:string? arg)) (lfe_io:print1)))
     (list ,@args)))

(defmacro lazy-seq
  "Return a (possibly infinite) lazy sequence from a given lazy sequence `seq`
  or a finite lazy sequence from given list `seq`.
  A lazy sequence is treated as finite if at any iteration it produces
  the empty list, instead of a cons cell with data as the head and a
  nullary function for the next iteration as the tail."
  (`(()) ())
  (`(,seq)
   `(lambda ()
      (let ((,'seq* ,seq))
        (if (is_function ,'seq* 0)
          (funcall ,'seq*)
          (fletrec ((-lazy-seq
                     ((()) ())
                     ((`(,h . ,t))
                      (cons h (lambda () (-lazy-seq t))))))
            (-lazy-seq ,'seq*)))))))

(defmacro conj
  "conj[oins] a value onto an existing collection, either a list, a tuple,
 or a map. For lists this means prepending, for tuples appending,
 and for maps merging."
  (`[,coll . ,xs]
   `(cond ((is_list ,coll)
           (++ (lists:reverse (list ,@xs)) ,coll))
          ((is_tuple ,coll)
           (lists:foldl (lambda (x acc) (erlang:append_element acc x))
                        ,coll
                        (list ,@xs)))
          ((clj:map? ,coll) (lists:foldl (lambda (x acc) (maps:merge acc x))
                                          ,coll
                                          (list ,@xs))))))

;;; Function composition.

(defn comp
  "Function composition.
  If the second argument is a function, compose `f` and `g`.
  Otherwise, compose a list of functions `fs` and apply the result to `x`."
  ([f g] (when (function? g))
   (fn [x] (funcall f (funcall g x))))
  ([fs x]
   (funcall (comp fs) x)))

(defn comp [f g x]
  "Equivalent to `(funcall (comp f g) x)`."
  (funcall (comp f g) x))

(defn comp [fs]
  "Compose a list of functions right to left."
  (lists:foldr #'comp/2 #'identity/1 fs))

(defn comp []
  "Equivalent to `#'identity/1`."
  #'identity/1)


;;; Partial application.

(defn partial
  "Partial application.
  Given a function `f`, and an argument or list of arguments, return a function
  that applies `f` to the given argument(s) plus (an) additional argument(s)."
  ([f args-1] (when (is_list args-1))
   (match-lambda
     ([args-2] (when (is_list args-2))
      (apply f (++ args-1 args-2)))
     ([arg]
      (apply f (++ args-1 `(,arg))))))
  ([f arg-1]
   (match-lambda
     ([args] (when (is_list args))
      (apply f (cons arg-1 args)))
     ([arg-2]
      (funcall f arg-1 arg-2)))))


;;; Predicate functions.

(defn string? [data]
  "Return `'true` if `data` is a flat list of printable characters."
  (io_lib:printable_list data))

(defn unicode? [data]
  "Return `'true` if `data` is a flat list of printable Unicode characters."
  (io_lib:printable_unicode_list data))

(defn list? [data]
  "Return `'true` if `data` is a list and not a string."
  (andalso (is_list data) (not (string? data))))

(defn set? [data]
  "Return `'true` if `data` is appears to be a (possibly ordered) set."
  (orelse (sets:is_set data)
          (ordsets:is_set data)))

(defn dict?
  "Return `'true` if `data` is a dictionary."
  ([data] (when (=:= 'dict (element 1 data)))
   'true)
  ([_]
   'false))

(defn proplist?
  "Return `'true` if `lst` is a list where [[proplist-kv?/1]] returns `'true`
  for all elements in `lst`."
  ([lst] (when (is_list lst))
   (lists:all #'proplist-kv?/1 lst))
  ([_]
   'false))

(defn proplist-kv?
  "Return `'true` if a given term is a key/value tuple or an atom."
  ([`#(,key ,_)] (when (atom? key))      'true)
  ([bool-key]    (when (atom? bool-key)) 'true)
  ([_]                                   'false))

(defn queue? [x]
  "Return `'true` if `x` is a queue."
  (queue:is_queue x))

(defn empty? [x]
  "Return `'true` if `x` is the empty list, tuple, map, dictionary, queue, or
  general balanced tree."
  (orelse (=:= () x) (=:= #() x)
          (andalso (map? x) (=:= 0 (call 'maps 'size x)))
          (andalso (dict? x) (dict:is_empty x))
          (andalso (queue? x) (queue:is_empty x))
          (gb_sets:is_empty x)))

(defn every? [pred lst]
  "Return `'true` if `(pred x)` returns `'true` for every `x` in `lst`."
  (lists:all pred lst))

(defn all? [pred lst]
  "Return `'true` if `(pred x)` returns `'true` for every `x` in `lst`."
  (lists:all pred lst))

(defn any? [pred lst]
  "Return `'true` if `(pred x)` returns `'true` for any `x` in `lst`."
  (lists:any pred lst))

(defn not-any? [pred lst]
  "Return `'false` if `(pred x)` returns `'true` for any `x` in `lst`."
  (not (lists:any pred lst)))

(defn element?
  "Return `'true` if `elem` is an element of `data`, where `data` is a list,
  set or ordset."
  ([elem data] (when (is_list data))
   (lists:member elem data))
  ([elem data]
   (cond
    ((sets:is_set data)
     (sets:is_element elem data))
    ((ordsets:is_set data)
     (ordsets:is_element elem data))
    ('true 'false))))


;;; Sequence functions.

(defn seq [end]
  "Equivalent to `(seq 1 end)`."
  (seq 1 end))

(defn seq [start end]
  "Equivalent to `(seq start end 1)`."
  (seq start end 1))

(defn seq [start end step]
  "Return a sequence of integers, starting with `start`, containing the
  successive results of adding `step` to the previous element, until `end` has
  been reached or password. In the latter case, `end` is not an element of the
  sequence."
  (lists:seq start end step))

(defn next [func]
  "Equivalent to `(next func 1 1)`."
  (next func 1 1))

(defn next [func start]
  "Equivalent to `(next func start 1)`."
  (next func start 1))

;; TODO: Improve this docstring.
(defn next [func start step]
  "Return a nullary function that returns a cons cell with `start` as the head
  and a nullary function, `(next func (funcall func start step) step)` as the
  tail. The result can be treated as a (possibly infinite) lazy list, which
  only computes subseqeuent values as needed."
  (fn [] (cons start (next func (funcall func start step) step))))

(defn cycle
  "Return a lazy infinite sequence with all elements from a given list `lst`
  or another lazy sequence cycled.
  See [[next/3]] for details on the structure."
  ([()] ())
  ([lst] (fn [] (-cycle lst ()))))

(defn range []
  "Equivalent to `(range 1 1)`."
  (range 1 1))

(defn range [start]
  "Equivalent to `(range start 1)`."
  (range start 1))

(defn range [start step]
  "Return a lazy list of integers, starting with `start` and increasing by
  `step`. Equivalent to `(next #'+/2 start step)`. See also: [[next/3]]."
  (next #'+/2 start step))

(defn drop
  "Return a list of all but the first `n` elements in `lst`. If `n` is the atom
  `all`, return the empty list."
  ([_    ()]                             ())
  ([0    lst]                            lst)
  (['all lst]       (when (is_list lst)) ())
  ([n    f]         (when (function? f)) (fn [] (-drop n (funcall f))))
  ([n    `(,_ . ,t)]                     (drop (dec n) t)))

(defn take
  "Given a (possibly lazy) list `lst`, return a list of the first `n` elements
  of `lst`, or all elements if there are fewer than `n`. If `n` is the atom
  `all` and `lst` is a \"normal\" list, return `lst`."
  ([_ ()]                          ())
  ([0 _]                           ())
  (['all lst] (when (is_list lst)) lst)
  ([n lst] (when (is_list lst))
   (lists:sublist lst n))
  ([n func] (when (function? func 0) (integer? n) (pos? n))
   (-take n () (funcall func))))

(defn split-at [n lst]
  "Return a tuple of `` `#(,(take n lst) ,(drop n lst)) ``."
  (tuple (take n lst) (drop n lst)))

(defn partition [n lst]
  "Equivalent to `(partition n n lst)`."
  (partition n n lst))

(defn partition [n step lst]
  "Equivalent to `(partition n step () lst)`."
  (-partition n step () 'false () lst))

(defn partition [n step pad lst]
  "Return a list of lists of `n` items each, at offsets `step` apart. Use the
  elements of `pad` as necessary to complete the last partition up to `n`
  elements. In case there are not enough padding elements, return a parition
  with less than `n` items."
  (-partition n step pad 'true () lst))

(defn partition-all [n lst]
  "Equivalent to `(partition-all n n lst)`."
  (partition-all n n lst))

(defn partition-all [n step lst]
  "Return a list of lists like [[partition/3]], possibly including partitions
  with fewer than `n` elements at the end."
  (-partition n step () 'true () lst))

(defn interleave [list-1 list-2]
  "Return a list of the first element of each list, then the second, etc."
  (-interleave () list-1 list-2))

(defn get-in [data keys]
  "Equivalent to `(get-in data keys 'undefined)`."
  (-get-in data keys 'undefined))

(defn get-in [data keys not-found]
  "Return the value in a nested associative structure, where `keys` is a list of
  keys or list indices. Return the atom `not-found` if the key is not present or
  index is out of bounds, or the `not-found` value."
  (-get-in data keys not-found))

(defn reduce
  "Equivalent to `(reduce func head tail)`."
  ([func `(,head . ,tail)]
   (reduce func head tail)))

(defn reduce [func acc lst]
  "Equivalent to `(lists:foldl func acc lst)`."
  (lists:foldl func acc lst))

(defn repeat [x]
  "Return a lazy infinite sequence of `x`s.
  See [[next/3]] for details on the structure."
  (next (fn [y _] y) x x))

(defn repeat
  "Given a nullary function `f`, return a list of `n` applications of `f`.
  Given a term `x`, return a list of `n` copies of `x`."
  ([n f] (when (function? f) (integer? n) (>= n 0))
   (fletrec ((repeat-fun
              ((0 acc) acc)
              ((n acc) (repeat-fun (dec n) (cons (funcall f) acc)))))
     (repeat-fun n ())))
  ([n x]
   (lists:duplicate n x)))

;;; Other functions.

(defn identity [x]
  "Identity function."
  x)

(defn constantly [x]
  "Return a unary function that returns `x`.
  N.B. This is like Haskell's `const` rather than Clojure's `constantly`."
  (fn [_] x))

(defn inc [x]
  "Increment `x` by 1."
  (+ x 1))

(defn dec [x]
  "Decrement `x` by 1."
  (- x 1))

;;; Internal functions.

(defn- -drop
  ([_ ()] ())
  ([0 data] data)
  ([n `(,_ . ,tail)] (when (function? tail))
   (-drop (dec n) (funcall tail))))

(defn- -take
  ([_ acc ()] (lists:reverse acc))
  ([1 acc (cons item _func)] (lists:reverse (cons item acc)))
  ([n acc (cons item  func)] (-take (dec n) (cons item acc) (funcall func))))

(defn- -partition
  ([0  _step _pad _partial? _acc lst] lst) ; FIXME: Do we want this behaviour?
  ([_n _step _pad _partial?  acc ()]  (lists:reverse acc))
  ([n   step  pad  partial?  acc lst]
   (case (take n lst)
     (p (when (== n (length p)))
        (-partition n step pad partial? (cons p acc) (drop step lst)))
     (_ (when (== 'false partial?))
        (-partition n step pad partial? acc ()))
     (p (when (== () pad))
        (-partition n step pad partial? (cons (take n p) acc) ()))
     (p
      (let ((acc* (cons (take n (lists:append p pad)) acc)))
        (-partition n step pad partial? acc* ()))))))

(defn- -interleave
  ([acc `(,x . ,xs) `(,y . ,ys)]
   (-interleave (list* y x acc) xs ys))
  ([acc _ _] (lists:reverse acc)))

(defn- -get-in
  ([data () not-found] data)
  ([data keys not-found]
   (cond ((proplist? data) (-get-in-proplist data keys not-found))
         ((dict?     data) (-get-in-dict     data keys not-found))
         ((list?     data) (-get-in-list     data keys not-found))
         ((map?      data) (-get-in-map      data keys not-found))
         ('true            not-found))))

(defn- -get-in
  ([func data `(,key) not-found]
   (funcall func key data))
  ([func data `(,key . ,keys) not-found]
   (-get-in (funcall func key data) keys not-found)))

(defn- -get-in-list
  ([lst  () not-found] lst)
  ([lst `(,n . ,keys) not-found] (when (integer? n))
   (let ((data
          (try
            (lists:nth n lst)
            (catch
              (`#(error function_clause ,_)
               not-found)))))
     (-get-in data keys not-found))))

(defn- -get-in-proplist [proplist keys not-found]
  (flet ((get-value [k l] (proplists:get_value k l not-found)))
    (-get-in #'get-value/2 proplist keys not-found)))

(defn- -get-in-dict [dict keys not-found]
  (flet ((dict-find [k d]
                    (case (dict:fetch k d)
                      (`#(ok ,v) v)
                      ('error    not-found))))
    (-get-in #'dict-find/2 dict keys)))

(defn- -get-in-map
  ([xmap keys not-found] (when (map? xmap))
   (flet ((maps-get [k m] (call 'maps 'get k m not-found)))
     (-get-in #'maps-get/2 xmap keys not-found)))
  ([_xmap _keys not-found] not-found))

(defn- -cycle
  ([() lst] (-cycle (lists:reverse lst) ()))
  ([f lst] (when (function? f 0))
   (-cycle (funcall f) lst))
  ([`(,head . ,tail) lst]
   (cons head (fn [] (-cycle tail (cons head lst))))))

;;; Clojure-inspired if macro.

(defmacro if args
  "test then {{else}}
  If `test` evaluates to anything other than `'false` or `'undefined`,
  return `then`, otherwise `else`, if given, else `'undefined`."
  (flet ((exp-if (test then else)
           `(case ,test
              ('false     ,else)
              ('undefined ,else)
              (_          ,then))))
    (case args
      ((list test then)      (exp-if test then `'undefined))
      ((list test then else) (exp-if test then else)))))
