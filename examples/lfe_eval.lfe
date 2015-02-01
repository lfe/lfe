;; Copyright (c) 2008-2013 Robert Virding
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

;; File    : lfe-eval.lfe
;; Author  : Robert Virding
;; Purpose : Lisp Flavoured Erlang interpreter.

;;; Shouldn't use macros here as macros need the evaluator! But we do.

(defmodule lfe_eval
  (export (expr 1) (expr 2) (gexpr 1) (gexpr 2) (apply 2) (apply 3)
      (body 1) (body 2) (guard 1) (guard 2)
      (make_letrec_env 2) (add_expr_func 4) (match 3) (match_when 4))
  ;; Deprecated exports.
  (export (eval 1) (eval 2) (eval_list 2))
  (import (from lfe_env (new 0)
        (add_vbinding 3) (add_vbindings 2) (get_vbinding 2)
        (add_fbinding 4) (add_fbindings 2) (get_fbinding 3)
        (add_ibinding 5) (get_gbinding 3))
      (from lists (reverse 1) (foldl 3) (foldr 3))
      (from orddict (find 2) (store 3)))
  (deprecated #(eval 1) #(eval 2)))

(defun eval (e) (eval e (: lfe_env new)))

(defun eval (e env) (eval-expr e env))

(defun eval_list (es env) (eval-list es env))

;; expr(Sexpr) -> Value
;; expr(Sexpr, Env) -> Value
;; Evaluate the sexpr, first expanding all macros.

(defun expr (e) (expr e (: lfe_env new)))

(defun expr (e env)
  (let ((exp (: lfe_macro expand_expr_all e env)))
    (eval-expr exp env)))

;; gexpr(guardtest) -> Value
;; gexpr(guardtest, env) -> Value

(defun gexpr (gt) (gexpr gt (: lfe_env new)))

(defun gexpr (gt env) (eval-gexpr gt env))

;; (apply function args) -> expr.
;; (apply function args env) -> Expr.
;;  This is applying interpreted Erlang functions, for applying funs
;;  use normal apply. Name scoping stops us from using apply/s
;;  internally. Args should already be evaluated.

(defun apply (f args)
  (apply f args (: lfe_env new)))

(defun apply (f args env)
  (eval-apply-expr f args env))

;; (body body) -> value
;; (body body env) -> value
;; (guard guard) -> true | false
;; (guard guard env) -> true | false

(defun body (b) (body b (: lfe_env new)))

(defun body (b env) (eval-body b env))

(defun guard (g) (guard g (: lfe_env new)))

(defun guard (g env) (eval-guard g env))

(defun match_when (pat val body env)
  (match-when pat val body env))

;; (eval-expr Sexpr Environment) -> Value.
;;  Evaluate a sexpr in the current environment. Try to catch core
;;  forms by just name and check arguments arguments later. Otherwise
;;  users can redefine core forms with different number of arguments.

(defun eval-expr (e env)
  (case e
    ;; Handle the Core data special forms.
    ((list 'quote e) e)
    ((list 'cons x y)
     (cons (eval-expr x env) (eval-expr y env)))
    ((list 'car x)
     (car (eval-expr x env)))
    ((list 'cdr x)
     (cdr (eval-expr x env)))
    ((cons 'list xs) (eval-list xs env))
    ((cons 'tuple xs) (list_to_tuple (eval-list xs env)))
    ((cons 'binary bs) (eval-binary bs env))
    ;; Handle the Core closure special forms.
    ((cons 'lambda body)
     (eval-lambda body env))
    ((cons 'match-lambda cls)
     (eval-match-lambda cls env))
    ((cons 'let body)
     (eval-let body env))
    ((cons 'let-function body)
     (eval-let-function body env))
    ((cons 'letrec-function body)
     (eval-letrec-function body env))
    ;; Handle the Core control special forms.
    ((cons 'progn body) (eval-body body env))
    ((cons 'if body)
     (eval-if body env))
    ((cons 'case body)
     (eval-case body env))
    ((cons 'receive body)
     (eval-receive body env))
    ((cons 'catch body)
     (catch (eval-body body env)))
    ((cons 'try body)
     (eval-try body env))
    ((list* 'funcall f as)
     (: erlang apply (eval-expr f env) (eval-list as env)))
    ((cons 'call body)
     (eval-call body env))
    ;; General function calls.
    ((cons fun es) (when (is_atom fun))
     ;; Note that macros have already been expanded here.
     (let ((ar (length es)))        ;Arity
       (case (get_fbinding fun ar env)
     ((tuple 'yes m f) (: erlang apply m f (eval-list es env)))
     ((tuple 'yes f) (eval-apply f (eval-list es env) env))
     ('no (: erlang error (tuple 'unbound_func (tuple fun ar)))))))
    ((cons f es)
     (: erlang error (tuple 'bad_form 'application)))
    (e (if (is_atom e)
     (case (get_vbinding e env)
       ((tuple 'yes val) val)
       (no (: erlang error (tuple 'unbound_symb e))))
     e))))                ;Atoms evaluate to themselves

(defun eval-list (es env)
  (: lists map (lambda (e) (eval-expr e env)) es))

(defun eval-body (body env)
  (case body
    ((list e) (eval-expr e env))
    ((cons e es)
     (eval-expr e env) (eval-body es env))
    (() ())))

;; (eval-binary bitsegs env) -> binary.
;;   Construct a binary from bitsegs. This code is taken from eval_bits.erl.

(defun eval-binary (fs env)
  (let ((vsps (parse-bitsegs fs env)))
    (eval-bitsegs vsps env)))

(defrecord spec
  (type 'integer) (size 'default) (unit 'default)
  (sign 'default) (endian 'default))

(defun parse-bitsegs (fs env)
;;  (map (lambda (f) (parse-bitseg f env)) fs))
  (foldr (lambda (f vs) (parse-bitseg f vs env)) () fs))

(defun parse-bitseg (f vsps env)
  (fletrec ((is-integer-list
         ([(cons i is)] (when (is_integer i)) (is-integer-list is))
         ([()] 'true)
         ([_] 'false)))
    ;; Test what structure the bitseg has.
    (cond ((is-integer-list f)        ;A string
       (let ((sp (parse-bitspecs () (make-spec) env)))
         (foldr (lambda (v vs) (cons (tuple v sp) vs)) vsps f)))
      ((?= (cons val specs) f)        ;A value and spec
       (let ((sp (parse-bitspecs specs (make-spec) env)))
         (if (is-integer-list val)
           (foldr (lambda (v vs) (cons (tuple v sp) vs)) vsps val)
           (cons (tuple val sp) vsps))))
      (else                ;A simple value
       (cons (tuple f (parse-bitspecs () (make-spec) env)) vsps)))))

;; (defun parse-bitseg (f env)
;;   (case f
;;     ((cons pat specs) (tuple pat (parse-bitspecs specs (make-spec) env)))
;;     (pat (tuple pat (parse-bitspecs () (make-spec) env)))))

(defun eval-bitsegs (psps env)
  (foldl (lambda (psp acc)
        (let* (((tuple val spec) psp)
           (bin (eval-bitseg val spec env)))
          (binary (acc bitstring) (bin bitstring))))
     #b() psps))

(defun eval-bitseg (val spec env)
  (let ((v (eval-expr val env)))
    (eval-exp-bitseg v spec)))

;; (parse-bitspecs specs spec env) -> (tuple type size unit sign end).

(defun parse-bitspecs (ss spec0 env)
  (let* ((spec1 (foldl (lambda (s spec) (parse-bitspec s spec env)) spec0 ss))
     ((match-spec type ty size sz unit un sign si endian en) spec1))
    ;; Adjust values depending on type and given value.
    (flet ((val-or-def (v def) (if (=:= v 'default) def v)))
      (case ty
    ('integer
     (tuple 'integer
        (val-or-def sz 8) (val-or-def un 1)
        (val-or-def si 'unsigned) (val-or-def en 'big)))
    ;; Ignore unused fields in utf types!
    ('utf8 (tuple 'utf8 'undefined 'undefined 'undefined 'undefined))
    ('utf16
     (tuple 'utf16 'undefined 'undefined 'undefined (val-or-def en 'big)))
    ('utf32
     (tuple 'utf32 'undefined 'undefined 'undefined (val-or-def en 'big)))
    ('float
     (tuple 'float
        (val-or-def sz 64) (val-or-def un 1)
        (val-or-def si 'unsigned) (val-or-def en 'big)))
    ('binary
     (tuple 'binary
        (val-or-def sz 'all) (val-or-def un 8)
        (val-or-def si 'unsigned) (val-or-def en 'big)))
    ('bitstring
     (tuple 'binary
        (val-or-def sz 'all) (val-or-def un 1)
        (val-or-def si 'unsigned) (val-or-def en 'big)))))))

(defun parse-bitspec (spec sp env)
  (case spec
    ;; Types.
    ('integer (set-spec-type sp 'integer))
    ('float (set-spec-type sp 'float))
    ('binary (set-spec-type sp 'binary))
    ('bytes (set-spec-type sp 'binary))
    ('bitstring (set-spec-type sp 'bitstring))
    ('bits (set-spec-type sp 'bitstring))
    ;; Unicode types.
    ('utf-8 (set-spec-type sp 'utf8))
    ('utf-16 (set-spec-type sp 'utf16))
    ('utf-32 (set-spec-type sp 'utf32))
    ;; Endianess.
    ('big-endian (set-spec-endian sp 'big))
    ('big (set-spec-endian sp 'big))
    ('little-endian (set-spec-endian sp 'little))
    ('little (set-spec-endian sp 'little))
    ('native-endian (set-spec-endian sp 'native))
    ('native (set-spec-endian sp 'native))
    ;; Sign.
    ('signed (set-spec-sign sp 'signed))
    ('unsigned (set-spec-sign sp 'unsigned))
    ;; Size
    ((list 'size n)
     (let ((size (eval-expr n env)))
       (set-spec-size sp size)))
    ((list 'unit n) (when (is_integer n) (> n 0))
     (set-spec-unit sp n))
    ;; Illegal spec.
    (_ (: erlang error (tuple 'illegal_bitspec spec)))))

;; (eval-exp-bitseg value type size unit sign endian) -> binary().

(defun eval-exp-bitseg (val spec)
  (case spec
    ;; Integer types.
    ((tuple 'integer sz un si en) (eval-int-bitseg val (* sz un) si en))
    ;; Unicode types, ignore unused specs.
    ((tuple 'utf8 _ _ _ _) (binary (val utf-8)))
    ((tuple 'utf16 _ _ _ en) (eval-utf-16-bitseg val en))
    ((tuple 'utf32 _ _ _ en) (eval-utf-32-bitseg val en))
    ;; Float types.
    ((tuple 'float sz un _ en) (eval-float-bitseg val (* sz un) en))
    ;; Binary types.
    ((tuple 'binary 'all un _ _)
     (case (bit_size val)
       (size (when (=:= (rem size un) 0))
         (binary (val bitstring (size size))))
       (_ (: erlang error 'bad_arg))))
    ((tuple 'binary sz un _ _)
     (binary (val bitstring (size (* sz un)))))))

(defun eval-int-bitseg
  ([val sz 'signed 'big] (binary (val (size sz) signed big-endian)))
  ([val sz 'unsigned 'big] (binary (val (size sz) unsigned big-endian)))
  ([val sz 'signed 'little] (binary (val (size sz) signed little-endian)))
  ([val sz 'unsigned 'little] (binary (val (size sz) unsigned little-endian)))
  ([val sz 'signed 'native] (binary (val (size sz) signed native-endian)))
  ([val sz 'unsigned 'native] (binary (val (size sz) unsigned native-endian))))

(defun eval-utf-16-bitseg (val en)
  (case en
    ('big (binary (val utf-16 big-endian)))
    ('little (binary (val utf-16 little-endian)))
    ('native (binary (val utf-16 native-endian)))))

(defun eval-utf-32-bitseg (val en)
  (case en
    ('big (binary (val utf-32 big-endian)))
    ('little (binary (val utf-32 little-endian)))
    ('native (binary (val utf-32 native-endian)))))

(defun eval-float-bitseg (val sz en)
  (case en
    ('big (binary (val float (size sz) big-endian)))
    ('little (binary (val float (size sz) little-endian)))
    ('native (binary (val float (size sz) native-endian)))))

;; (eval-lambda (lambda-body env)) -> val

(defun eval-lambda
  ([(cons args body) env]
   ;; This is a really ugly hack!
   (case (length args)
     (0 (lambda () (apply-lambda () body () env)))
     (1 (lambda (a) (apply-lambda args body (list a) env)))
     (2 (lambda (a b) (apply-lambda args body (list a b) env)))
     (3 (lambda (a b c) (apply-lambda args body (list a b c) env)))
     (4 (lambda (a b c d) (apply-lambda args body (list a b c d) env)))
     (5 (lambda (a b c d e) (apply-lambda args body (list a b c d e) env)))
     (6 (lambda (a b c d e f)
      (apply-lambda args body (list a b c d e f) env)))
     (7 (lambda (a b c d e f g)
      (apply-lambda args body (list a b c d e f g) env)))
     (8 (lambda (a b c d e f g h)
      (apply-lambda args body (list a b c d e f g h) env)))
     (9 (lambda (a b c d e f g h i)
      (apply-lambda args body (list a b c d e f g h i) env)))
     (10 (lambda (a b c d e f g h i j)
       (apply-lambda args body (list a b c d e f g h i j) env)))
     (11 (lambda (a b c d e f g h i j k)
       (apply-lambda args body (list a b c d e f g h i j k) env)))
     (12 (lambda (a b c d e f g h i j k l)
       (apply-lambda args body (list a b c d e f g h i j k l) env)))
     (13 (lambda (a b c d e f g h i j k l m)
       (apply-lambda args body (list a b c d e f g h i j k l m) env)))
     (14 (lambda (a b c d e f g h i j k l m n)
       (apply-lambda args body (list a b c d e f g h i j k l m n) env)))
     (15 (lambda (a b c d e f g h i j k l m n o)
       (apply-lambda args body (list a b c d e f g h i j k l m n o) env)))
     )))

(defun apply-lambda (args body vals env)
  (fletrec ((bind-args
         ([(cons '_ as) (cons _ es) env]    ;Ignore don't care variables
          (bind-args as es env))
         ([(cons a as) (cons e es) env] (when (is_atom a))
          (bind-args as es (add_vbinding a e env)))
         ([() () env] env)))
    (eval-body body (bind-args args vals env))))

;; eval-match-lambda (MatchClauses Env) -> Value
;; Evaluate (match-lambda cls ...).

(defun eval-match-lambda (cls env)
  ;; This is a really ugly hack!
  (case (match-lambda-arity cls)
    (0 (lambda () (apply-match-clauses cls () env)))
    (1 (lambda (a) (apply-match-clauses cls (list a) env)))
    (2 (lambda (a b) (apply-match-clauses cls (list a b) env)))
    (3 (lambda (a b c) (apply-match-clauses cls (list a b c) env)))
    (4 (lambda (a b c d) (apply-match-clauses cls (list a b c d) env)))
    (5 (lambda (a b c d e) (apply-match-clauses cls (list a b c d e) env)))
    (6 (lambda (a b c d e f)
      (apply-match-clauses cls (list a b c d e f) env)))
    (7 (lambda (a b c d e f g)
      (apply-match-clauses cls (list a b c d e f g) env)))
    (8 (lambda (a b c d e f g h)
      (apply-match-clauses cls (list a b c d e f g h) env)))
    (9 (lambda (a b c d e f g h i)
      (apply-match-clauses cls (list a b c d e f g h i) env)))
    (10 (lambda (a b c d e f g h i j)
      (apply-match-clauses cls (list a b c d e f g h i j) env)))
    (11 (lambda (a b c d e f g h i j k)
      (apply-match-clauses cls (list a b c d e f g h i j k) env)))
    (12 (lambda (a b c d e f g h i j k l)
      (apply-match-clauses cls (list a b c d e f g h i j k l) env)))
    (13 (lambda (a b c d e f g h i j k l m)
      (apply-match-clauses cls (list a b c d e f g h i j k l m) env)))
    (14 (lambda (a b c d e f g h i j k l m n)
      (apply-match-clauses cls (list a b c d e f g h i j k l m n) env)))
    (15 (lambda (a b c d e f g h i j k l m n o)
      (apply-match-clauses cls (list a b c d e f g h i j k l m n o) env)))
    ))

(defun match-lambda-arity (cls) (length (caar cls)))

(defun apply-match-clauses (cls as env)
  (case cls
    ((cons (cons pats body) cls)
     (if (== (length as) (length pats))
       ;; Sneaky! m-l args a list of patterns so wrap with list
       ;; and pass in as one pattern. Have already checked a
       ;; proper list.
       (case (match-when (cons 'list pats) as body env)
     ((tuple 'yes body1 vbs) (eval-body body1 (add_vbindings vbs env)))
     ('no (apply-match-clauses cls as env)))
       (: erlang error 'badarity)))
    (_ (: erlang error 'function_clause))))

;; (eval-let (PatBindings . Body) Env) -> Value.

(defun eval-let (body env0)
  (let* (((cons vbs b) body)        ;Must match this
     ;; Make sure we use the right environment.
     (env (foldl (match-lambda
               ([(list pat e) env]
            (let ((val (eval-expr e env0)))
              (case (match pat val env0)
                ((tuple 'yes bs) (add_vbindings bs env))
                ('no (: erlang error (tuple 'badmatch val))))))
               ([(list pat (= (cons 'when _) g) e) env]
            (let ((val (eval-expr e env0)))
              (case (match-when pat val (list g) env0)
                ((tuple 'yes '() bs) (add_vbindings bs env))
                ('no (: erlang error (tuple 'badmatch val))))))
               ([_ _] (: erlang error (tuple 'bad_form 'let))))
             env0 vbs)))
    (eval-body b env)))

;; (eval-let-function (FuncBindings . Body) Env) -> Value.

(defun eval-let-function (form env0)
  (flet ((add (f ar def lenv e)
          (add_fbinding f ar (tuple 'lexical_expr def lenv) e)))
    (let* (((cons fbs body) form)
       (env (foldl (match-lambda
             ([(list v (= (list* 'lambda as _) f)) e]
              (when (is_atom v))
              (add v (length as) f env0 e))
             ([(list v (= (list* 'match-lambda (cons pats _) _) f)) e]
              (when (is_atom v))
              (add v (length pats) f env0 e))
             ([_ _] (: erlang error (tuple 'bad_form 'let-function))))
               env0 fbs)))
      (eval-body body env))))

;; (eval-letrec-function (FuncBindings . Body) Env) -> Value.
;;  This is a tricky one. But we dynamically update the environment
;;  each time we are called.

(defun eval-letrec-function (form env0)
  (let* (((cons fbs0 body) form)
         (map-fun (match-lambda
                    ([(list v (= (list* 'lambda args _) f))]
                     (when (is_atom v))
                     (tuple v (length args) f))
                    ([(list v (= (list* 'match-lambda (cons pats _) _) f))]
                     (when (is_atom v))
                     (tuple v (length pats) f))
                    ([_] (: erlang error (tuple 'bad_form 'letrec-function)))))
         (fbs1 (: lists map map-fun fbs0))
         (env1 (make_letrec_env fbs1 env0)))
    (eval-body body env1)))

;; (make_letrec_env fbs env) -> env.
;;  Create local function bindings for a set of mutally recursive
;;  functions, for example from a module or a letrec-function. This is
;;  very similar to "Metacircular Semantics for Common Lisp Special
;;  Forms" by Henry Baker, except he uses macros whereas we directly
;;  fiddle with the environment and he keeps functions in a vector
;;  where we just push them into the environment. His version compiles
;;  much better (which we don't need) but is basically the same
;;  interpreted.

(defun init_letrec_env (env) (tuple () env))

(defun make_letrec_env (fbs0 env)
  (let ((fbs (: lists map (lambda (fb)
                            (let (((tuple v ar body) fb))
                              (tuple v ar (tuple 'letrec body fbs0 env))))
                fbs0)))
    (add_fbindings fbs env)))

(defun extend_letrec_env (lete0 fbs0 env0)
  (tuple lete0 env0))

;; (add_expr_func name arity def env) -> env.
;;  Add a function definition in the correct format to the
;;  environment.

(defun add_expr_func (name ar def env)
  (add_fbinding name ar (tuple 'lexical_expr def env) env))

;; (eval-apply function args env) -> value
;;  This is used to evaluate interpreted functions. Macros are
;;  expanded completely in the function definition before it is
;;  applied.

(defun eval-apply (f es env0)
  (case f
    ((tuple 'dynamic_expr func)
     (eval-apply-expr func es env0))
    ((tuple 'dynamic_expr func env)
     (eval-apply-expr func es env))
    ((tuple 'letrec body fbs env)
     ;; A function created by/for letrec-function.
     (let ((newenv (foldl (match-lambda
                ([(tuple v ar lambda) e]
                 (add_fbinding v ar
                       (tuple 'letrec lambda fbs env) e)))
              env fbs)))
       (eval-apply-expr body es newenv)))))

;; (eval-apply-expr function args env) -> value
;;  Apply the function definition to the (evaluated) args in env.
;;  Macros are expanded first.

(defun eval-apply-expr (func es env)
  (case (: lfe_macro expand_expr_all func env)
    ((list* 'lambda args body) (apply-lambda args body es env))
    ((cons 'match-lambda cls) (apply-match-clauses cls es env))
    (fun (when (is_function fun)) (: erlang apply fun es))))

;; (eval-if body env) -> value

(defun eval-if (body env)
  (flet ((eval-if (test true false)
          ;; Use explicit case to catch errors.
          (case (eval-expr test env)
            ('true (eval-expr true env))
            ('false (eval-expr false env))
            (_ (: erlang error 'if_clause)))))
    (case body
      ((list test true) (eval-if test true 'false))
      ((list test true false) (eval-if test true false)))))

;; (eval-case (expr . cls) env) -> value

(defun eval-case (body env)
  (eval-case-clauses (eval-expr (car body) env)
             (cdr body) env))

(defun eval-case-clauses (v cls env)
  (case (match-clause v cls env)
    ((tuple 'yes b vbs)
     (eval-body b (add_vbindings vbs env)))
    ('no (: erlang error (tuple 'case_clause v)))))

(defun match-clause (v cls env)
  (case cls
    ((cons (cons pat body) cls)
     (case (match-when pat v body env)
       ((= (tuple 'yes body vbs) yes) yes)
       ('no (match-clause v cls env))))
    (() 'no)))

;; (eval-receive body env) -> value

(defun eval-receive (body env)
  (fletrec ((split-rec
         ([(list (list* 'after t b)) rcls]
          (tuple (reverse rcls) t b))
         ([(cons cl b) rcls]
          (split-rec b (cons cl rcls)))
         ([() rcls]            ;No timeout, return 'infinity
          (tuple (reverse rcls) 'infinity ()))))
    (let (((tuple cls te tb) (split-rec body [])))
      (case (eval-expr te env)
    ('infinity (receive-clauses cls env))
    (t (receive-clauses t tb cls env))))))

;; (receive-clauses Clauses Env) -> Value.
;;  Recurse down message queue. We are only called with timeout value
;;  of 'infinity'. Always pass over all messages in queue.

(defun receive-clauses (cls env)
  (fletrec ((rec-clauses
         (ms)
         (receive
          (msg (case (match-clause msg cls env)
             ((tuple 'yes b vbs)
              (merge-queue ms)
              (eval-body b (add_vbindings vbs env)))
             ('no (rec-clauses (cons msg ms))))))))
    (rec-clauses ())))

;; (receive-clauses Timeout TimeoutBody Clauses Env) -> Value.
;;  Recurse down message queue until timeout. We are never called with
;;  timeout value of 'infinity'. Always pass over all messages in
;;  queue.

(defun receive-clauses (t tb cls env)
  (fletrec ((rec-clauses
         (t ms)
         (receive
           (msg
        (case (match-clause msg cls env)
          ((tuple 'yes b vbs)
           (merge-queue ms)
           (eval-body b (add_vbindings vbs env)))
          ('no
           (let (((tuple _ t1) (statistics 'runtime)))
             (if (< t t1)
               (rec-clauses 0 (cons msg ms))
               (rec-clauses (- t t1) (cons msg ms)))))))
           (after t
         (merge-queue ms)
         (eval-body tb env)))))
    (statistics 'runtime)
    (rec-clauses t [])))

;; Merge the already received messages back into the process message
;; queue in the right order. Do this by first receiving the rest of
;; the messages in the queue then sending them all back to ourselves.

(defun merge-queue (ms)
  (fletrec ((recv-all (ms)        ;Receive all remaining messages
              (receive
            (msg (recv-all (cons msg ms)))
            (after 0
              (reverse ms))))
        (send-all (ms self)        ;Send them all back to ourselves
              (case ms
            ((cons m ms)
             (! self m)
             (send-all ms self))
            (() 'ok))))
    (send-all (recv-all ms) (self))))

;; (eval-try body env) -> value
;;  Complicated by checking legal combinations of options.

(defun eval-try (body env)
  (case body
    ((list* e (cons 'case cls) catch)
     (eval-try-catch catch e (list cls) env))
    ((cons e catch)
     (eval-try-catch catch e () env))))

(defun eval-try-catch (body e case env)
  (case body
    ((list (cons 'catch cls))
     (eval-try e case (list cls) () env))
    ((list (cons 'catch cls) (cons 'after b))
     (eval-try e case (list cls) (list b) env))
    ((list (cons 'after b))
     (eval-try e case () (list b) env))))

(defun eval-try (e case catch after env)
  (try
      (eval-expr e env)
    (case
    (r (case case
         ((list cls) (eval-case-clauses r cls env))
         (() r))))
    (catch
      ((tuple class error _)
       ;; Get stack trace explicitly.
       (let ((stk (: erlang get_stacktrace)))
     (case catch
       ((list cls) (eval-catch-clauses (tuple class error stk) cls env))
       (() (: erlang raise class error stk))))))
    (after
    (case after
      ((list b) (eval-body b env))
      (() ())))))

(defun eval-catch-clauses
  ([v (cons (cons pat b) cls) env]
   (case (match-when pat v b env)
     ((tuple 'yes b vbs) (eval-body b (add_vbindings vbs env)))
     ('no (eval-catch-clauses v cls env))))
  ([(tuple class val stk) () _]
   (: erlang raise class val stk)))

(defun eval-call (b env)
  (case b
    ((list* m f as)
     (let ((m (eval-expr m env))
       (f (eval-expr f env))
       (as (eval-list as env)))
       (: erlang apply m f as)))))

;; (match-when pattern value body env) -> #('yes restbody bindings) | 'no.
;;  Try to match pattern and evaluate guard.

(defun match-when (pat val b0 env)
  (case (match pat val env)
    ((tuple 'yes vbs)
     (case b0
       ((cons (cons 'when g) b1)
    (if (eval-guard g (add_vbindings vbs env))
      (tuple 'yes b1 vbs)
      'no))
       (b1 (tuple 'yes b1 vbs))))
    ('no 'no)))

;; (eval-guard guardtests env) -> true | false.
;; Guards are fault safe, catch all errors in guards here and fail guard.

(defun eval-guard (gts env)
  (try
      (eval-gbody gts env)
    (case ('true 'true)
      (_ 'false))            ;Fail guard
    (catch
      ((tuple t v i) 'false))))        ;Fail guard

;; (eval-gbody body env) -> true | false.
;; A body is a sequence of tests which must all succeed.

(defun eval-gbody (es env)
  (: lists all (lambda (e) (eval-gexpr e env)) es))

;; (eval-gexpr sexpr environment) -> value.
;;  Evaluate a guard sexpr in the current environment.

(defun eval-gexpr (e env)
  (case e
    ;; Handle the Core data special forms.
    ((list 'quote e) e)
    ((list 'cons x y)
     (cons (eval-gexpr x env) (eval-gexpr y env)))
    ((list 'car x)
     (car (eval-gexpr x env)))
    ((list 'cdr x)
     (cdr (eval-gexpr x env)))
    ((cons 'list xs) (eval-glist xs env))
    ((cons 'tuple xs) (list_to_tuple (eval-glist xs env)))
    ((cons 'binary bs) (eval-gbinary bs env))
    ;; Handle the Core closure special forms.
    ;; Handle the Core control special forms.
    ((cons 'progn b) (eval-gbody b env))
    ((cons 'if b) (eval-gif b env))
    ((list* 'call 'erlang f as)
     (let ((f (eval-gexpr f env))
       (ar (length as)))
       (case (get_gbinding f ar env)
     ((tuple 'yes m f) (: erlang apply m f (eval-glist as env)))
     (_ (: erlang error (tuple 'unbound_func (tuple f (length as))))))))
    ((cons f as) (when (is_atom f))
     (let ((ar (length as)))
       (case (get_gbinding f ar env)
     ((tuple 'yes m f) (: erlang apply m f (eval-glist as env)))
     ('no (: erlang error (tuple 'unbound_func (tuple f ar)))))))
    ((cons f es)            ;Everything else not allowed
     (: erlang error 'illegal_guard))
    (e (if (is_atom e)
     (case (get_vbinding e env)
       ((tuple 'yes val) val)
       (no (: erlang error (tuple 'unbound_symb e))))
     e))))                ;Atoms evaluate to themselves

(defun eval-glist (es env)
  (: lists map (lambda (e) (eval-gexpr e env)) es))

;; (eval-gbinary bitsegs env) -> binary.
;;   Construct a binary from bitsegs. This code is taken from eval_bits.erl.

(defun eval-gbinary (fs env)
  (let ((vsps (parse-bitsegs fs env)))
    (eval-gbitsegs vsps env)))

(defun eval-gbitsegs (psps env)
  (foldl (lambda (psp acc)
        (let* (((tuple val spec) psp)
           (bin (eval-gbitseg val spec env)))
          (binary (acc bitstring) (bin bitstring))))
     #b() psps))

(defun eval-gbitseg (val spec env)
  (let ((v (eval-gexpr val env)))
    (eval-exp-bitseg v spec)))

;; (eval-gif ifbody env) -> val

(defun eval-gif (body env)
  (flet ((eval-gif (test true false)
           (if (eval-gexpr test env)
             (eval-gexpr true env)
             (eval-gexpr false env))))
    (case body
      ((list test true) (eval-gif test true 'false))
      ((list test true false) (eval-gif test true false)))))

;; (match pattern value env) -> (tuple 'yes bs) | 'no
;;  Try to match Pattern against Value within the current environment
;;  returning bindings. Bindings is an orddict.

(defun match (pat val env) (match pat val env ()))

(defun match
  ([(list 'quote p) val env bs]
   (if (=:= p val) (tuple 'yes bs) 'no))
  ([(cons 'tuple ps) val env bs]
   (if (is_tuple val)
     (match-list ps (tuple_to_list val) env bs)
     'no))
  ([(cons 'binary fs) val env bs]
   (if (is_bitstring val)
     (match-binary fs val env bs)
     'no))
  ([(list '= p1 p2) val env bs]        ;Aliases
   (case (match p1 val env bs)
     ((tuple 'yes bs) (match p2 val env bs))
     ('no 'no)))
  ([(list 'cons p ps) (cons v vs) env bs] ;Explicit cons constructor
   (case (match p v env bs)
     ((tuple 'yes bs) (match ps vs env bs))
     ('no 'no)))
  ([(cons 'list ps) val env bs]        ;Explicit list constructor
   (match-list ps val env bs))
  ;; Use old no contructor list forms.
  ([(cons p ps) (cons v vs) env bs]
   (case (match p v env bs)
     ((tuple 'yes bs) (match ps vs env bs))
     ('no 'no)))
;;  ([(cons _ _) _ _ _] 'no)        ;No constructor

  ([() () env bs] (tuple 'yes bs))
  ([symb val env bs] (when (is_atom symb))
   (match-symb symb val env bs))
  ([pat val env bs]
   (if (=:= pat val) (tuple 'yes bs) 'no)))

(defun match-list
  ([(cons p ps) (cons v vs) env bs]
   (case (match p v env bs)
     ((tuple 'yes bs) (match-list ps vs env bs))
     ('no 'no)))
  ([() () _ bs] (tuple 'yes bs))
  ([_ _ _ _] 'no))

(defun match-symb (symb val env bs)
  (if (== symb '_) (tuple 'yes bs)    ;Don't care variable
      ;; Check if symbol already bound.
      (case (find symb bs)
    ((tuple 'ok _) 'no)        ;Already bound, multiple variable
    ('error (tuple 'yes (store symb val bs))))))

;; (match-binary bitsegs binary env bindings) -> (tuple 'yes bindings) | 'no.
;;  Match Bitsegs against Binary. This code is taken from
;;  eval_bits.erl. All bitspec errors and bad matches result in an
;;  error, we use catch to trap it.

(defun match-binary (fs bin env bs)
  (let ((psps (parse-bitsegs fs env)))
    (case (catch (match-bitsegs psps bin env bs))
      ((tuple 'yes bs) (tuple 'yes bs))    ;Matched whole binary
      ((tuple 'EXIT _) 'no))))        ;Error is no match

(defun match-bitsegs
  ([(cons (tuple pat specs) psps) bin0 env bs0]
   (let (((tuple 'yes bin1 bs1) (match-bitseg pat specs bin0 env bs0)))
     (match-bitsegs psps bin1 env bs1)))
  ([() #b() _ bs] (tuple 'yes bs)))    ;Reached the end of both

(defun match-bitseg (pat spec bin0 env bs0)
  (let* (((tuple val bin1) (get-pat-bitseg bin0 spec))
     ((tuple 'yes bs1) (match pat val env bs0)))
    (tuple 'yes bin1 bs1)))

;; (get-pat-bitseg binary #(type size unit sign endian)) -> #(value restbinary)

(defun get-pat-bitseg (bin spec)
  (case spec
    ;; Integer types.
    ((tuple 'integer sz un si en) (get-int-bitseg bin (* sz un) si en))
    ;; Unicode types, ignore unused specs.
    ((tuple 'utf8 _ _ _ _) (get-utf-8-bitseg bin))
    ((tuple 'utf16 _ _ _ en) (get-utf-16-bitseg bin en))
    ((tuple 'utf32 _ _ _ en) (get-utf-32-bitseg bin en))
    ;; Float types.
    ((tuple 'float sz un _ en) (get-float-bitseg bin (* sz un) en))
    ;; Binary types.
    ((tuple 'binary 'all un _ _)
     (let ((0 (rem (bit_size bin) un)))
       (tuple bin #b())))
    ((tuple 'binary sz un _ _)
     (let* ((tot-size (* sz un))
        ((binary (val bitstring (size tot-size)) (rest bitstring)) bin))
       (tuple val rest)))))

(defun get-int-bitseg
  ([bin sz 'signed 'big]
   (let (((binary (val signed big-endian (size sz))
          (rest bitstring)) bin))
     (tuple val rest)))
  ([bin sz 'unsigned 'big]
   (let (((binary (val unsigned big-endian (size sz))
          (rest bitstring)) bin))
     (tuple val rest)))
  ([bin sz 'signed 'little]
   (let (((binary (val signed little-endian (size sz))
          (rest bitstring)) bin))
     (tuple val rest)))
  ([bin sz 'unsigned 'little]
   (let (((binary (val unsigned little-endian (size sz))
          (rest bitstring)) bin))
     (tuple val rest)))
  ([bin sz 'signed 'native]
   (let (((binary (val signed native-endian (size sz))
          (rest bitstring)) bin))
     (tuple val rest)))
  ([bin sz 'unsigned 'native]
   (let (((binary (val unsigned native-endian (size sz))
          (rest bitstring)) bin))
     (tuple val rest))))

(defun get-utf-8-bitseg (bin)
  (let (((binary (val utf-8) (rest bitstring)) bin))
    (tuple val rest)))

(defun get-utf-16-bitseg (bin en)
  (case en
    ('big (let (((binary (val utf-16 big-endian) (rest bitstring)) bin))
        (tuple val rest)))
    ('little (let (((binary (val utf-16 little-endian) (rest bitstring)) bin))
           (tuple val rest)))
    ('native (let (((binary (val utf-16 native-endian) (rest bitstring)) bin))
           (tuple val rest)))))

(defun get-utf-32-bitseg (bin en)
  (case en
    ('big (let (((binary (val utf-32 big-endian) (rest bitstring)) bin))
        (tuple val rest)))
    ('little (let (((binary (val utf-32 little-endian) (rest bitstring)) bin))
           (tuple val rest)))
    ('native (let (((binary (val utf-32 native-endian) (rest bitstring)) bin))
           (tuple val rest)))))

(defun get-float-bitseg (bin sz en)
  (case en
    ('big
     (let (((binary (val float big-endian (size sz)) (rest bitstring)) bin))
       (tuple val rest)))
    ('little
     (let (((binary (val float little-endian (size sz)) (rest bitstring)) bin))
       (tuple val rest)))
    ('native
     (let (((binary (val float native-endian (size sz)) (rest bitstring)) bin))
       (tuple val rest)))))
