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

;; File    : cl.lfe
;; Author  : Robert Virding, Duncan McGreggor
;; Purpose : LFE Common Lisp interface library.

(defmodule cl
  "LFE Common Lisp interface library."
  (export
   ;; Boolean conversion functions.
   (make-lfe-bool 1) (make-cl-bool 1)
   ;; Control structure.
   (mapcar 2) (maplist 2) (mapc 2) (mapl 2)
   ;; Symbol functions.
   (symbol-plist 1) (symbol-name 1)
   (get 2) (get 3) (getl 2) (putprop 3) (remprop 2)
   ;; Property list functions.
   (getf 2) (getf 3) (putf 3) (remf 2) (get-properties 2)
   ;; Sequences.
   (elt 2) (length 1) (reverse 1) (some 2) (every 2) (notany 2) (notevery 2)
   (reduce 2) (reduce 4) (reduce 6)
   (remove 2) (remove-if 2) (remove-if-not 2) (remove-duplicates 1)
   (substitute 3) (substitute-if 3) (substitute-if-not 3)
   (find 2) (find-if 2) (find-if-not 2)
   (position 2) (position-if 2) (position-if-not 2)
   (count 2) (count-if 2) (count-if-not 2)
   ;; Lists.
   (car 1) (cdr 1) (first 1) (rest 1) (nth 2)
   (nthcdr 2) (last 1) (butlast 1)
   ;; Substitution of expressions.
   (subst 3) (subst-if 3) (subst-if-not 3) (sublis 2)
   ;; Lists as sets.
   (member 2) (member-if 2) (member-if-not 2) (adjoin 2) (union 2)
   (intersection 2) (set-difference 2) (set-exclusive-or 2) (subsetp 2)
   ;; Association list functions.
   (acons 3) (pairlis 2) (pairlis 3) (assoc 2) (assoc-if 2) (assoc-if-not 2)
   (rassoc 2) (rassoc-if 2) (rassoc-if-not 2)
   ;; Types.
   (type-of 1) (coerce 2))
  (export-macro
   ;; Export control structure macros.
   do
  ;; Export CL-style if and cond, which we don't use internally.
   if cond))

;;; Boolean conversion functions.

(defun make-lfe-bool                    ;Make an LFE bool from a CL value
  "cl-boolean
   Make an LFE bool from a CL value."
  ([()] 'false)
  ([_]  'true))                         ;Everything else is true

(defun make-cl-bool                     ;Make a CL bool from an LFE value
  "lfe-boolean
   Make a CL bool from an LFE value."
  (['false] ())
  (['true] 'true))

;; Control structure.

(defmacro do args
  "vars (end-test result) body"
  (let* ((`(,pars (,test ,ret) . ,body) args)
	 ((tuple vs is cs)
	  (lists:foldr (match-lambda
			 ([(list v i c) (tuple vs is cs)]
			  (tuple (cons v vs) (cons i is) (cons c cs))))
		       (tuple () () ()) pars)))
    `(letrec-function ((|\|-do-func-\||
			(lambda ,vs
			  (if ,test ,ret
			      (let ((do-state (progn . ,body)))
				(|\|-do-func-\|| . ,cs))))))
       (|\|-do-func-\|| . ,is))))

(defun mapcar (func list)
  "function list"
  (lists:map func list))

(defun maplist
  "function list"
  ([func (= (cons _ rest) list)]
   (cons (funcall func list) (maplist func rest)))
  ([func ()] ()))

(defun mapc (func list)
  "function list"
  (lists:foreach func list)
  list)

(defun mapl (func list)
  "function list"
  (fletrec ((mapl-loop
             ([(= (cons _ rest) list)]
              (funcall func list)
              (mapl-loop rest))
             ([()] ())))
    (mapl-loop list)
    list))

;; Symbol function functions.
;;  get, getl, putprop and remprop should really only work on a
;;  symbols plist not just a plist. This is coming. Hence including
;;  getf, putf and remf.

(defun ensure-plist-table ()
  (case (ets:info 'lfe-symbol-plist 'type)
    ('undefined
     (let ((init-pid (erlang:whereis 'init)))
       (ets:new 'lfe-symbol-plist
        (list 'set 'public 'named_table (tuple 'heir init-pid ())))))
    (_ 'ok)))

(defun symbol-plist (symbol)
  "symbol
   Get the property list for symbol."
  (ensure-plist-table)
  (case (ets:lookup 'lfe-symbol-plist symbol)
    (`(#(,_ ,plist)) plist)
    (() ())))

(defun symbol-name (symb)
  "symbol
   Get the name of symbol as a list."
  (atom_to_list symb))

(defun get (symbol pname)
  "symbol pname
   Get the property pname of symbol."
  (get symbol pname ()))

;;(defun get (plist pname def) (getf plist pname def))

(defun get (symbol pname def)
  "symbol pname default"
  (ensure-plist-table)
  (let ((plist (symbol-plist symbol)))
    (getf plist pname def)))

(defun getl (symbol pnames)
  "symbol pnames"
  (ensure-plist-table)
  (let ((plist (symbol-plist symbol)))
    (fletrec ((getl-loop
               ([(= (list* p v plist-rest) plist) pnames]
                (if (member p pnames)
                  plist
                  (getl-loop plist-rest pnames)))
               ([() pnames] ())))
      (getl-loop plist pnames))))

;; (defun putprop (plist val pname) (putf plist val pname))

(defun putprop (symbol val pname)
  "symbol value pname"
  (ensure-plist-table)
  (let* ((plist (symbol-plist symbol))
         (plist (putf plist val pname)))
    (ets:insert 'lfe-symbol-plist (tuple symbol plist))))

;; (defun getprop (plist pname) (remf plist pname))

(defun remprop (symbol pname)
  "symbol pname"
  (ensure-plist-table)
  (let* ((plist (symbol-plist symbol))
         (plist (remf plist pname)))
    ;; Delete element if plist empty
    (if (=:= plist ())
      (ets:delete 'lfe-symbol-plist symbol)
      (ets:insert 'lfe-symbol-plist (tuple symbol plist)))))

;; Property list functions.

(defun getf (plist pname)
  "plist pname"
  (getf plist pname ()))

(defun getf
  "plist pname default"
  ([(list* p v plist) p def]  v)
  ([(list* _ _ plist) pname def] (getf plist pname def))
  ([() _m def] def))

(defun putf                             ;This doesn't exist in CL
  "plist value pname"
  ([(list* p _ plist) val p]
   (list* p val plist))
  ([(list* p v plist) val pname]
   (list* p v (putf plist val pname)))
  ([() val pname] (list pname val)))

(defun remf
  "plist pname"
  ([(list* p _ plist) p] plist)
  ([(list* p v plist) pname]
   (list* p v (remf plist pname)))
  ([() pname] ()))

(defun get-properties
  "plist pnames"
  ([(= (list* p v plist-rest) plist) pnames]
   (if (member p pnames)
     (tuple p v plist)
     (get-properties plist-rest pnames)))
  ([() pnames] (tuple () () ())))

;; Arrays.

;; (defun aref (array i j)
;;   (elt j (elt i array)))

;; Sequences.
;; Simple sequence functions.

(defun elt
  ((n seq) (when (is_list seq))
   (nth n seq))
  ((n seq) (when (is_tuple seq))
   (tref seq (+ n 1))))

(defun length
  ([seq] (when (is_list seq))
   (length seq))
  ([seq] (when (is_tuple seq))
   (tuple_size seq)))

(defun reverse
  ([seq] (when (is_list seq))
   (lists:reverse seq))
  ([seq] (when (is_tuple seq))
   (list_to_tuple (lists:reverse (tuple_to_list seq)))))

;; Concatanation, mapping and reducing sequences.

(defun some
  "pred list
   Return true if pred is true for some element of list."
  ([pred seq] (when (is_list seq))
   (lists:any pred seq))
  ([pred seq] (when (is_tuple seq))
   (fletrec ((some-loop
              ([i n] (when (>= i n)) 'false)
              ([i n]
               (orelse (funcall pred (tref seq i))
                       (some-loop (+ i 1) n)))))
     (some-loop 1 (tuple_size seq)))))

(defun every
  "pred list
   Return true if pred is true for every element of list."
  ([pred seq] (when (is_list seq))
   (lists:all pred seq))
  ([pred seq] (when (is_tuple seq))
   (fletrec ((every-loop
              ([i n] (when (>= i n)) 'false)
              ([i n]
               (andalso (not (funcall pred (tref seq i)))
                        (every-loop (+ i 1) n)))))
     (every-loop 1 (tuple_size seq)))))

(defun notany (pred seq)
  "pred list
   Returns true if pred is false for every element of list."
  (every (lambda (x) (not (funcall pred x))) seq))

(defun notevery (pred seq)
  "pred list
   Returns true if pred is false for some element of list."
  (some (lambda (x) (not (funcall pred x))) seq))

(defun reduce (func seq)
  (lists:foldl func '() seq))

(defun reduce
  ((func seq 'initial-value x)
   (lists:foldl func x seq))
  ((func seq 'from-end 'true)
   (lists:foldr func '() seq)))

(defun reduce
  ((func seq 'from-end 'true 'initial-value x)
   (lists:foldr func x seq))
  ((func seq 'initial-value x 'from-end 'true)
   (lists:foldr func x seq)))

;; Modifying sequences.

(defun remove
  "item sequence
   Remove all elements from sequence which are equal to item."
  ([item seq] (when (is_list seq))
   (lc ((<- x seq) (=/= x item)) x))
  ([item seq] (when (is_tuple seq))
   (list_to_tuple (remove item (tuple_to_list seq)))))

(defun remove-if
  "pred sequence
   Remove all elements from sequence for which pred is true."
  ([pred seq] (when (is_list seq))
   (lc ((<- x seq) (not (funcall pred x))) x))
  ([pred seq] (when (is_tuple seq))
   (list_to_tuple (remove-if pred (tuple_to_list seq)))))

(defun remove-if-not
  "pred sequence
   Remove all elements from sequence for which pred is false."
  ([pred seq] (when (is_list seq))
   (lc ((<- x seq) (funcall pred x)) x))
  ([pred seq] (when (is_tuple seq))
   (list_to_tuple (remove-if-not pred (tuple_to_list seq)))))

(defun remove-duplicates
  "sequence
   Remove duplicates from sequence."
  ([seq] (when (is_list seq))
   (fletrec ((rm-loop
              ([(cons x rest)]
               (if (lists:member x rest)
                 (rm-loop rest)
                 (cons x (rm-loop rest))))
              ([()] ())))
     (rm-loop seq)))
  ([seq] (when (is_tuple seq))
   (list_to_tuple (remove-duplicates (tuple_to_list seq)))))

(defun substitute
  "new old sequence
   Replace all elements in sequence which are equal to old with new."
  ([new old seq] (when (is_list seq))
   (fletrec ((sub-loop
              ([n o (cons o xs)]
               (cons n (sub-loop n o xs)))
              ([n o (cons x xs)]
               (cons x (sub-loop n o xs)))
              ([_ _ ()] ())))
     (sub-loop new old seq)))
  ([new old seq] (when (is_tuple seq))
   (list_to_tuple (substitute new old (tuple_to_list seq)))))

(defun substitute-if
  "new pred sequence
   Replace all elements in sequence for which pred is true with new."
  ([new pred seq] (when (is_list seq))
   (fletrec ((sub-loop
              ([n p (cons x xs)]
               (cons (if (funcall p x) n x) (sub-loop n p xs)))
              ([_ _ ()] ())))
     (sub-loop new pred seq)))
  ([new pred seq] (when (is_tuple seq))
   (list_to_tuple (substitute-if new pred (tuple_to_list seq)))))

(defun substitute-if-not
  "new pred sequence
   Replace all elements in sequence for which pred is false with new."
  ([new pred seq] (when (is_list seq))
   (fletrec ((sub-loop
              ([n p (cons x xs)]
               (cons (if (funcall p x) x n) (sub-loop n p xs)))
              ([_ _ ()] ())))
     (sub-loop new pred seq)))
  ([new pred seq] (when (is_tuple seq))
   (list_to_tuple (substitute-if-not new pred (tuple_to_list seq)))))

;; Searching sequences.

(defun find (item seq)
  "item sequence
   If sequence contains item then it is returned else ()."
  (fletrec ((find-loop
             ([x (cons x xs)] x)
             ([x (cons _ xs)] (find-loop x xs))
             ([x ()] ())))
    (find-loop item seq)))

(defun find-if (pred seq)
  "pred sequence
   Return element in sequnce for which pred is true else ()."
  (fletrec ((find-if-loop
             ([pred (cons x xs)]
              (if (funcall pred x) x (find-if-loop pred xs)))
             ([pred ()] ())))
    (find-if-loop pred seq)))

(defun find-if-not (pred seq)
  "pred sequence
   Return element in sequnce for which pred is true else ()."
  (fletrec ((find-if-not-loop
             ([pred (cons x xs)]
              (if (funcall pred x) (find-if-not-loop pred xs) x))
             ([pred ()] ())))
    (find-if-not-loop pred seq)))

(defun position (item seq)
  "item sequence
   Return index of item in sequence else ()."
  (fletrec ((pos-loop
             ([x n (cons x xs)] n)
             ([x n (cons _ xs)] (pos-loop x (+ n 1) xs))
             ([x n ()] ())))
    (pos-loop item 0 seq)))

(defun position-if (pred seq)
  "item sequence
   Return index of item in sequence for which pred is true else ()."
  (fletrec ((pos-if-loop
             ([pred n (cons x xs)]
              (if (funcall pred x)
                n
                (pos-if-loop pred (+ n 1) xs)))
             ([pred n ()] ())))
    (pos-if-loop pred 0 seq)))

(defun position-if-not (pred xs)
  "item sequence
   Return index of item in sequence for which pred is false else ()."
  (fletrec ((pos-if-not-loop
             ([pred n (cons x xs)]
              (if (funcall pred x)
                (pos-if-not-loop pred (+ n 1) xs)
                n))
             ([pred n ()] ())))
    (pos-if-not-loop pred 0 xs)))

(defun count (item seq)
  "item sequence
   Return the number of elements in sequence equal to item."
  (fletrec ((count-loop
             ([x n (cons x1 xs)]
              (let ((n1 (if (=:= x x1) (+ n 1) n)))
                (count-loop x n1 xs)))
             ([x n ()] n)))
    (count-loop item 0 seq)))

(defun count-if (pred seq)
  "pred sequence
   Return the number of elements in sequence for which pred is true."
  (fletrec ((count-if-loop
             ([pred n (cons x xs)]
              (let ((n1 (if (funcall pred x) (+ n 1) n)))
                (count-if-loop pred n1 xs)))
             ([pred n ()] n)))
    (count-if-loop pred 0 seq)))

(defun count-if-not (pred seq)
  "pred sequence
   Return the number of elements in sequence for which pred is false."
  (fletrec ((count-if-not-loop
             ([pred n (cons x xs)]
              (let ((n1 (if (funcall pred x) n (+ n 1))))
                (count-if-not-loop pred n1 xs)))
             ([pred n ()] n)))
    (count-if-not-loop pred 0 seq)))

;;; Lists

(defun car
  ([()] ())
  ([xs] (car xs)))

(defun first (xs)
  (cl:car xs))

(defun cdr
  ([()] ())
  ([xs] (cdr xs)))

(defun rest (xs)
  (cl:cdr xs))

(defun nth
  ([n xs] (when (< n 0)) ())
  ([n xs]
   (fletrec ((nth-loop
              ([n ()] ())               ;End of the list
              ([0 xs] (car xs))         ;Found the one
              ([n xs] (nth-loop (- n 1) (cdr xs)))))
     (nth-loop n xs))))

(defun nthcdr (n xs)
  (lists:nthtail (+ n 1) xs))

(defun last (list)
  (lists:last list))

(defun butlast (list)
  (lists:droplast list))

;; Substitution of expressions

(defun subst
  "new old tree
   Substitute `new` for every subtree `old` in `tree`."
  ([new old old] new)
  ([new old (cons e rest)]
   (cons (subst new old e) (subst new old rest)))
  ([new old tree] tree))

(defun subst-if (new test tree)
  "new test tree
   Substitute `new` for every subtree which satisfies `test` in `tree`."
  (if (funcall test tree) new
      (case tree
        ((cons e rest)
         (cons (subst-if new test e) (subst-if new test rest)))
        (_ tree))))

(defun subst-if-not (new test tree)
  "new test tree
   Substitute `new` for every subtree which does not satisfy `test` in `tree`."
  (if (funcall test tree)
    (case tree
      ((cons e rest)
       (cons (subst-if-not new test e) (subst-if-not new test rest)))
      (_ tree))
    new))

(defun sublis (a-list tree)
  "a-list tree
   Subsitute the value of each key in `a-list` occurring in `tree`."
  (case (assoc tree a-list)
    ((cons _ new) new)                  ;Found it
    (()                                 ;Not there
     (case tree
       ((cons e rest)
        (cons (sublis a-list e) (sublis a-list rest)))
       (_ tree)))))

;; Lists as sets.

(defun member (item list)
  "item list
   Return true if `item` is a member of `list`."
  (lists:member item list))

(defun member-if
  "pred list
   Return true if `pred` is satisfied for a member of `list`."
  ([pred (cons e list)]
   (orelse (funcall pred e)
           (member-if pred list)))
  ([pred ()] 'false))

(defun member-if-not
  "pred list
   Return true if `pred` is not satisfied for a member of `list`."
  ([pred (cons e list)]
   (orelse (not (funcall pred e)) (member-if-not pred list)))
  ([pred ()] 'false))

(defun adjoin (item list)
  "item list
   Add `item` to `list` if it is not already a member."
  (if (member item list)
    list
    (cons item list)))

(defun union
  "list-1 list-2
   Returns the elements which are members of lists `list-1` or `list-2`."
  ([(cons e l1) l2]
   (if (member e l2)
     (union l1 l2)
     (cons e (union l1 l2))))
  ([() l2] l2))

(defun intersection (l1 l2)
  "list-1 list-2
   Returns the elements which are members of both lists `list-1` and `list-2`."
  (lc ((<- e l1) (member e l2)) e))

(defun set-difference (l1 l2)
  "list-1 list-2
   Returns the elements of `list-1` which are not elements in `list-2`."
  (lc ((<- e l1) (not (member e l2))) e))

(defun set-exclusive-or (l1 l2)
  "list-1 list-2
   Return the elements which are elements of one of `list-1` or `list-2`."
  (++ (set-difference l1 l2) (set-difference l2 l1)))

(defun subsetp
  "list-1 list-2
   Return true if every element in `list-1` is also in `list-2`."
  ([(cons e l1) l2] (andalso (member e l2) (subsetp l1 l2)))
  ([()          l2] 'true))

;; Association list functions.

(defun acons (k v a-list)
  "key value a-list
   Add `(key . value)` to the front of the `a-list`."
  (cons (cons k v) a-list))

(defun pairlis (ks vs)
  "keys values
   Make an alist from pairs of keys values."
  (pairlis ks vs ()))

(defun pairlis
  "keys values a-list
   Make an alist from pairs of keys values prepending them to a-list."
  ([(cons k ks) (cons v vs) a-list]
   (cons (cons k v) (pairlis ks vs a-list)))
  ([() () a-list] a-list))

(defun assoc
  "key a-list
   Searches a-list returning the first pair whose car is key."
  ([k (cons (= (cons k v) pair) _)] pair)
  ([k (cons _ a-list)] (assoc k a-list))
  ([k ()] ()))

(defun assoc-if
  "pred a-list
   Searches a-list returning the first pair for which pred is true."
  ([pred (cons (= (cons k _) pair) a-list)]
   (if (funcall pred k) pair
       (assoc-if pred a-list)))
  ([pred ()] ()))

(defun assoc-if-not
  "pred a-list
   Searches a-list returning the first pair for which pred is false."
  ([pred (cons (= (cons k _) pair) a-list)]
   (if (funcall pred k)
     (assoc-if-not pred a-list)
     pair))
  ([pred ()] ()))

(defun rassoc
  "value a-list
   Searches a-list returning the first pair whose cdr is value."
  ([v (cons (= (cons _ v) pair) _)] pair)
  ([v (cons _ a-list)] (rassoc v a-list))
  ([v ()] ()))

(defun rassoc-if
  "pred a-list
   Searches a-list returning the first pair for which pred is true."
  ([pred (cons (= (cons _ v) pair) a-list)]
   (if (funcall pred v)
     pair
     (rassoc-if pred a-list)))
  ([pred ()] ()))

(defun rassoc-if-not
  "pred a-list
   Searches a-list returning the first pair for which pred is false."
  ([pred (cons (= (cons _ v) pair) a-list)]
   (if (funcall pred v)
     (rassoc-if-not pred a-list)
     pair))
  ([pred ()] ()))

;;; Types

(defun type-of
  ((x) (when (is_boolean x))
   'boolean)
  ((x) (when (is_atom x))
   'atom)
  ((x) (when (is_tuple x))
   'tuple)
  ((x) (when (is_integer x))
   'integer)
  ((x) (when (is_float x))
   'float)
  ((x) (when (is_list x))
   (cond ((io_lib:printable_latin1_list x) 'string)
         ((io_lib:printable_unicode_list x) 'unicode)
         ((?= `(,a . ,b) (when (not (is_list b))) x) 'cons)
         ('true 'list)))
  ((x) (when (is_function x))
   'function)
  ((x) (when (is_binary x))
   'binary)
  ((x) (when (is_bitstring x))
   'bitstring)
  ((x) (when (is_pid x))
   'pid)
  ((x) (when (is_port x))
   'port)
  ((x) (when (is_reference x))
   'reference)
  ((x)
   (andalso (call 'erlang 'is_map x) 'map)))

(defun coerce
  ((x 'vector) (when (is_list x))
   (list_to_tuple x))
  ((x 'tuple) (when (is_list x))
   (list_to_tuple x))
  ((x 'atom) (when (is_list x))
   (list_to_atom x))
  ((x 'list) (when (is_atom x))
   (atom_to_list x))
  ((x 'list) (when (is_tuple x))
   (tuple_to_list x))
  ((x 'list) (when (is_binary x))
   (binary_to_list x))
  ((x 'list) (when (is_bitstring x))
   (bitstring_to_list x))
  ((x 'character) (when (is_atom x))
   (car (atom_to_list x)))
  ((x 'character) (when (is_list x))
   (car x))
  ((x 'integer) (when (is_float x))
   (trunc x))
  ((x 'float) (when (is_integer x))
   (list_to_float (integer_to_list x)))
  ((x 'float) (when (is_list x))
   (list_to_float x))
  ((x 'float) (when (is_atom x))
   (list_to_float (atom_to_list x)))
  ((x 't)
   x))

;;; System

(defun posix-argv ()
  (init:get_arguments))

;; Test defining CL if and cond. We need to put these last so they
;; won't be used inside this module, but of course the if can't.

(defmacro if args
  "test true-case false-case
   CL compatible if macro."
  (flet ((exp-if (test if-true if-false)
                 `(case ,test
                    (() ,if-false)
                    (_ ,if-true))))
    (case args
      ((list test if-true) (exp-if test if-true ()))
      ((list test if-true if-false)
       (exp-if test if-true if-false)))))

(defmacro cond args
  "args
   CL compatible cond macro."
  (fletrec ((exp-cond
             ([(cons (list test) cond)]
              `(case ,test
                 (() ,(exp-cond cond))
                 (|\|-cond-test-\|| |\|-cond-test-\||)))
             ([(cons (cons test body) cond)]
              `(case ,test
                 (() ,(exp-cond cond))
                 (_ (progn . ,body))))
             ([()] ())))
    (exp-cond args)))
