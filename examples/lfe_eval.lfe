;; Copyright (c) 2008 Robert Virding. All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;; File    : lfe-eval.lfe
;; Author  : Robert Virding
;; Purpose : Lisp Flavoured Erlang interpreter.

;;; We cannot use macros here as macros need the evaluator!

(defmodule lfe_eval
  (export (expr 1) (expr 2) (gexpr 1) (gexpr 2) (apply 2) (apply 3)
	  (make_letrec_env 2) (add_expr_func 4) (match 3)
	  (eval 1) (eval 2) (eval_list 2))
  (import (from lfe_lib (new_env 0)
		(add_vbinding 3) (add_vbindings 2) (get_vbinding 2)
		(add_fbinding 4) (add_fbindings 2) (get_fbinding 3)
		(add_ibinding 5) (get_gbinding 3))
	  (from lists (reverse 1) (map 2) (foldl 3))
	  (from orddict (find 2) (store 3)))
  (deprecated #(eval 1) #(eval 2)))

(defun eval (e) (eval e (new_env)))

(defun eval (e env) (eval-expr e env))

(defun eval_list (es env) (eval-list es env))

(defun expr (e) (expr e (new_env)))

(defun expr (e env) (eval-expr e env))

(defun gexpr (e) (gexpr e (new_env)))

(defun gexpr (e env) (eval-gexpr e env))

(defun apply (f args)
  (let ((env (new_env)))
    (lfe-apply (tuple 'expr f env) args env)))

(defun apply (f args env)
  (lfe-apply (tuple 'expr f env) args env))

;; (eval-expr Sexpr Environment) -> Value.
;;  Evaluate a sexpr in the current environment. Try to catch core
;;  forms by just name and check arguments arguments later. Otherwise
;;  users can redefine core forms with different number of arguments.

(defun eval-expr (e env)
  (case e
    ;; Handle the Core data special forms.
    (('quote e) e)
    (('cons x y)
     (cons (eval-expr x env) (eval-expr y env)))
    (('car x)
     (car (eval-expr x env)))
    (('cdr x)
     (cdr (eval-expr x env)))
    (('list . xs) (eval-list xs env))
    (('tuple . xs) (list_to_tuple (eval-list xs env)))
    (('binary . bs) (eval-binary bs env))
    ;; Handle the closure special forms.
    (('lambda . body)
     (eval-lambda body env))
    (('match-lambda . cls)
     (eval-match-lambda cls env))
    (('let . body)
     (eval-let body env))
    (('let-function . body)
     (eval-let-function body env))
    (('letrec-function . body)
     (eval-letrec-function body env))
    ;; Handle the control special forms.
    (('progn . body) (eval-body body env))
    (('if . body)
     (eval-if body env))
    (('case . body)
     (eval-case body env))
    (('receive . body)
     (eval-receive body env))
    (('catch . body)
     (catch (eval-body body env)))
    (('try . body)
     (eval-try body env))
    (('funcall f . as)
     (: erlang apply (eval-expr f env) (eval-list as env)))
    (('call . body)
     (eval-call body env))
    ((f . es) (when (is_atom f))
     ;; If macro then expand and try again, else try to find function.
     ;; We only expand the top level here.
     (case (: lfe_macro expand_macro e env)
       ((tuple 'yes exp)
	(eval-expr exp env)) ;This was macro, try again
       ('no
	(let ((ar (length es)))		;Arity
	  (case (get_fbinding f ar env)
	    ((tuple 'yes m f) (: erlang apply m f (eval-list es env)))
	    ((tuple 'yes f) (lfe-apply f (eval-list es env) env))
	    ('no (: erlang error (tuple 'unbound_func (tuple f ar)))))))))
    ((f . es)
     (: erlang error (tuple 'bad_form 'application)))
    (e (if (is_atom e)
	 (case (get_vbinding e env)
	   ((tuple 'yes val) val)
	   (no (: erlang error (tuple 'unbound_symb e))))
	 e))))				;Atoms evaluate to themselves

(defun eval-list (es env)
  (map (lambda (e) (eval-expr e env)) es))

(defun eval-body (body env)
  (case body
    ((e) (eval-expr e env))
    ((e . es)
     (eval-expr e env) (eval-body es env))
    (() ())))

;; (eval-binary fields env) -> binary.
;;   Construct a binary from fields. This code is taken from eval_bits.erl.

(defun eval-binary (fs env)
  (let ((psps (map (lambda (f) (parse-field f env)) fs)))
    (eval-fields psps env)))

(defrecord spec
  (type 'integer) (size 'default) (unit 'default)
  (sign 'default) (endian 'default))

(defun parse-field (f env)
  (case f
    ((pat . specs) (tuple pat (parse-bitspecs specs (make-spec) env)))
    (pat (tuple pat (parse-bitspecs () (make-spec) env)))))

(defun eval-fields (psps env)
  (foldl (lambda (psp acc)
	    (let* (((tuple val spec) psp)
		   (bin (eval-field val spec env)))
	      (binary (acc binary (unit 1)) (bin binary (unit 1)))))
	 #b() psps))

(defun eval-field (val spec env)
  (let ((v (eval-expr val env)))
    (eval-exp-field v spec)))

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
	 (tuple 'integer
		(val-or-def sz 'all) (val-or-def un 8)
		(val-or-def si 'unsigned) (val-or-def en 'big)))
	('bitstring
	 (tuple 'binary
		(val-or-def sz 'all) (val-or-def un 1)
		(val-or-def si 'unsigned) (val-or-def en 'big)))))))

(defun parse-bitspec (sp spec env)
  (case sp
    ;; Types.
    ('integer (set-spec-type spec 'integer))
    ('float (set-spec-type spec 'float))
    ('binary (set-spec-type spec 'binary))
    ('bytes (set-spec-type spec 'binary))
    ('bitstring (set-spec-type spec 'bitstring))
    ('bits (set-spec-type spec 'bitstring))
    ;; Unicode types.
    ('utf-8 (set-spec-type spec 'utf8))
    ('utf-16 (set-spec-type spec 'utf16))
    ('utf-32 (set-spec-type spec 'utf32))
    ;; Endianess.
    ('big-endian (set-spec-type spec 'big))
    ('little-endian (set-spec-type spec 'little))
    ('big-native (set-spec-type spec 'native))
    ;; Sign.
    ('signed (set-spec-sign spec 'signed))
    ('unsigned (set-spec-sign spec 'unsigned))
    ;; Size
    (('size n)
     (let ((size (eval-expr n env)))
       (set-spec-size spec size)))
    (('unit n) (when (and (is_integer n) (> n 0)))
     (set-spec-unit spec n))
    ;; Illegal spec.
    (sp (: erlang error (tuple 'illegal_bitspec sp)))))

;; (eval-exp-field value type size unit sign endian) -> binary().

(defun eval-exp-field (val spec)
  (case spec
    ;; Integer types.
    ((tuple 'integer sz un si en) (eval-int-field val (* sz un) si en))
    ;; Unicode types, ignore unused fields.
    ((tuple 'utf8 _ _ _ _) (binary (val utf-8)))
    ((tuple 'utf16 _ _ _ en) (eval-utf-16-field val en))
    ((tuple 'utf32 _ _ _ en) (eval-utf-32-field val en))
    ;; Float types.
    ((tuple 'float sz un _ en) (eval-float-field val (* sz un) en))
    ;; Binary types.
    ((tuple 'binary 'all un _ _)
     (case (: erlang bit_size val)
       (size (when (=:= (rem size un) 0))
	     (binary (val binary (size size) (unit 1))))
       (_ (: erlang error 'bad_arg))))
    ((tuple 'binary sz un _ _)
     (binary (val binary (size (* sz un)) (unit 1))))))

(defun eval-int-field
  ([val sz 'signed 'little] (binary (val (size sz) signed little-endian)))
  ([val sz 'unsigned 'little] (binary (val (size sz) unsigned little-endian)))
  ([val sz 'signed 'native] (binary (val (size sz) signed native-endian)))
  ([val sz 'unsigned 'native] (binary (val (size sz) unsigned native-endian)))
  ([val sz 'signed 'big] (binary (val (size sz) signed big-endian)))
  ([val sz 'unsigned 'big] (binary (val (size sz) unsigned big-endian))))

(defun eval-utf-16-field (val en)
  (case en
    ('little (binary (val utf-16 little-endian)))
    ('native (binary (val utf-16 native-endian)))
    ('big (binary (val utf-16 big-endian)))))

(defun eval-utf-32-field (val en)
  (case en
    ('little (binary (val utf-32 little-endian)))
    ('native (binary (val utf-32 native-endian)))
    ('big (binary (val utf-32 big-endian)))))

(defun eval-float-field (val sz en)
  (case en
    ('little (binary (val float (size sz) little-endian)))
    ('native (binary (val float (size sz) native-endian)))
    ('big (binary (val float (size sz) big-endian)))))

;; (eval-lambda (lambda-body env)) -> val

(defun eval-lambda
  (((args . body) env)
   ;; This is a really ugly hack!
   (case (length args)
     (0 (lambda () (eval-lambda () () body env)))
     (1 (lambda (a) (eval-lambda (list a) args body env)))
     (2 (lambda (a b) (eval-lambda (list a b) args body env)))
     (3 (lambda (a b c) (eval-lambda (list a b c) args body env)))
     (4 (lambda (a b c d) (eval-lambda (list a b c d) args body env)))
     (5 (lambda (a b c d e) (eval-lambda (list a b c d e) args body env)))
     (6 (lambda (a b c d e f)
	  (eval-lambda (list a b c d e f) args body env)))
     (7 (lambda (a b c d e f g)
	  (eval-lambda (list a b c d e f g) args body env)))
     (8 (lambda (a b c d e f g h)
	  (eval-lambda (list a b c d e f g h) args body env)))
     (9 (lambda (a b c d e f g h i)
	  (eval-lambda (list a b c d e f g h i) args body env)))
     (10 (lambda (a b c d e f g h i j)
	   (eval-lambda (list a b c d e f g h i j) args body env)))
     (11 (lambda (a b c d e f g h i j k)
	   (eval-lambda (list a b c d e f g h i j k) args body env)))
     (12 (lambda (a b c d e f g h i j k l)
	   (eval-lambda (list a b c d e f g h i j k l) args body env)))
     (13 (lambda (a b c d e f g h i j k l m)
	   (eval-lambda (list a b c d e f g h i j k l m) args body env)))
     (14 (lambda (a b c d e f g h i j k l m n)
	   (eval-lambda (list a b c d e f g h i j k l m n) args body env)))
     (15 (lambda (a b c d e f g h i j k l m n o)
	   (eval-lambda (list a b c d e f g h i j k l m n o) args body env)))
     )))

(defun eval-lambda (vals args body env)
  (fletrec ((bind-args
	     ([('_ . as) (_ . es) env]	;Ignore don't care variables
	      (bind-args as es env))
	     ([(a . as) (e . es) env] (when (is_atom a))
	      (bind-args as es (add_vbinding a e env)))
	     ([() () env] env)))
    (eval-body body (bind-args args vals env))))

(defun eval-match-lambda (cls env)
  ;; This is a really ugly hack!
  (case (match-lambda-arity cls)
    (0 (lambda () (eval-match-clauses () cls env)))
    (1 (lambda (a) (eval-match-clauses (list a) cls env)))
    (2 (lambda (a b) (eval-match-clauses (list a b) cls env)))
    (3 (lambda (a b c) (eval-match-clauses (list a b c) cls env)))
    (4 (lambda (a b c d) (eval-match-clauses (list a b c d) cls env)))
    (5 (lambda (a b c d e) (eval-match-clauses (list a b c d e) cls env)))
    (6 (lambda (a b c d e f)
	  (eval-match-clauses (list a b c d e f) cls env)))
    (7 (lambda (a b c d e f g)
	  (eval-match-clauses (list a b c d e f g) cls env)))
    (8 (lambda (a b c d e f g h)
	  (eval-match-clauses (list a b c d e f g h) cls env)))
    (9 (lambda (a b c d e f g h i)
	  (eval-match-clauses (list a b c d e f g h i) cls env)))
    (10 (lambda (a b c d e f g h i j)
	  (eval-match-clauses (list a b c d e f g h i j) cls env)))
    (11 (lambda (a b c d e f g h i j k)
	  (eval-match-clauses (list a b c d e f g h i j k) cls env)))
    (12 (lambda (a b c d e f g h i j k l)
	  (eval-match-clauses (list a b c d e f g h i j k l) cls env)))
    (13 (lambda (a b c d e f g h i j k l m)
	  (eval-match-clauses (list a b c d e f g h i j k l m) cls env)))
    (14 (lambda (a b c d e f g h i j k l m n)
	  (eval-match-clauses (list a b c d e f g h i j k l m n) cls env)))
    (15 (lambda (a b c d e f g h i j k l m n o)
	  (eval-match-clauses (list a b c d e f g h i j k l m n o) cls env)))
    ))

(defun match-lambda-arity (cls)
  (length (car (car cls))))

(defun eval-match-clauses (as cls env)
  (case cls
    ([(pats . body) . cls]
     (if (== (length as) (length pats))
       (case (match-when pats as body env)
	 ((tuple 'yes body1 vbs) (eval-body body1 (add_vbindings vbs env)))
	 ('no (eval-match-clauses as cls env)))
       (: erlang error 'badarity)))
    ([_ _] (: erlang error 'function_clause))))

;; (eval-let (PatBindings . Body) Env) -> Value.

(defun eval-let (body env0)
  (let* (((vbs . b) body)		;Must match this
	 (env (foldl (match-lambda
		       (((pat e) env)
			(let* ((val (eval-expr e env0))
			       ((tuple 'yes bs) (match pat val env0)))
			  (add_vbindings bs env)))
		       (((pat g e) env)
			(let* ((val (eval-expr e env0))
			       ((tuple 'yes '() bs)
				(match-when pat val (list g) env0)))
			  (add_vbindings bs env)))
		       ((_ _) (: erlang error (tuple 'bad_form 'let))))
		     env0 vbs)))
    (eval-body b env)))

;; (eval-let-function (FuncBindings . Body) Env) -> Value.

(defun eval-let-function (form env0)
  (let* (((fbs . body) form)
	 (env (foldl (match-lambda
		       ([(v (= ('lambda as . _) f)) e]
			(when (is_atom v))
			(add_fbinding v (length as) (tuple 'expr f env0) e))
		       ([(v (= ('match-lambda (pats . _) . _) f)) e]
			(when (is_atom v))
			(add_fbinding v (length pats) (tuple 'expr f env0) e))
		       ((_ _) (: erlang error (tuple 'bad_form 'let-function))))
		     env0 fbs)))
    (eval-body body env)))

;; (eval-letrec-function (FuncBindings . Body) Env) -> Value.
;;  This is a tricky one. But we dynamically update the environment
;;  each time we are called.

(defun eval-letrec-function (form env0)
  (let* (((fbs0 . body) form)
	 (fbs1 (map (match-lambda
		      ([(v (= ('lambda args . body) f))] (when (is_atom v))
		       (tuple v (length args) f))
		      ([(v (= ('match-lambda (pats . _) . _) f))]
		       (when (is_atom v))
		       (tuple v (length pats) f))
		      ((_) (: erlang error (tuple 'bad_form 'letrec-function))))
		    fbs0))
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

(defun make_letrec_env (fbs0 env)
  (let ((fbs (map (lambda (fb)
		    (let (((tuple v ar body) fb))
		      (tuple v ar (tuple 'letrec body fbs0 env))))
		  fbs0)))
    (add_fbindings fbs env)))

;; (add_expr_func name arity def env) -> env.
;;  Add a function definition in the correct format to the
;;  environment.

(defun add_expr_func (name ar def env)
  (add_fbinding name ar (tuple 'expr def env) env))

;; (lfe-apply function args env) -> value
;;  This is used to evaluate interpreted functions.

(defun lfe-apply (f es env0)
  (case f
    ((tuple 'expr ('lambda args . body) env)
     (eval-lambda es args body env))
    ((tuple 'expr ('match-lambda . cls) env)
     (eval-match-clauses es cls env))
    ((tuple 'letrec body fbs env)
     (let ((newenv (foldl (match-lambda
			    (((tuple v ar lambda) e)
			     (add_fbinding v ar
					   (tuple 'letrec lambda fbs env) e)))
			  env fbs)))
       (lfe-apply (tuple 'expr body newenv) es env0)))))

;; (eval-if body env) -> value

(defun eval-if (body env)
  (flet ((eval-if (test true false)
		  ;; Use explicit case to catch errors.
		  (case (eval-expr test env)
		    ('true (eval-expr true env))
		    ('false (eval-expr false env))
		    (_ (: erlang error 'if_clause)))))
    (case body
      ((test true) (eval-if test true 'false))
      ((test true false) (eval-if test true false)))))

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
    (((pat . body) . cls)
     (case (match-when pat v body env)
       ((= (tuple 'yes body vbs) yes) yes)
       ('no (match-clause v cls env))))
    (() 'no)))

;; (eval-receive body env) -> value

(defun eval-receive (body env)
  (fletrec ((split_rec
	     ([(('after t . b)) rcls]
	      (tuple (reverse rcls) t b))
	     ([(cl . b) rcls]
	      (split_rec b (cons cl rcls)))
	     ([() rcls]
	      (tuple (reverse rcls) 'infinity ()))))
    (let (((tuple cls te tb) (split_rec body [])))
      (case (eval-expr te env)
	('infinity (receive-clauses cls env ()))
	(t (receive-clauses t tb cls env))))))

(defun receive-clauses (cls env ms)
  (receive
    (msg (case (match-clause msg cls env)
	   ((tuple 'yes b vbs)
	    (merge-queue ms)
	    (eval-body b (add_vbindings vbs env)))
	   ('no (receive-clauses cls env (cons msg ms)))))))

;; (receive-clauses Timeout TimeoutBody Clauses) -> Value.
;;  Recurse down message queue until timeout. We are never called with
;;  timeout value of 'infinity'. Always pass over all messages in
;;  queue.

(defun receive-clauses (t tb cls env)
  (fletrec ((rec_clauses
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
		       (rec_clauses 0 (cons msg ms))
		       (rec_clauses (- t t1) (cons msg ms)))))))
	       (after t
		      (merge-queue ms)
		      (eval-body tb env)))))
    (statistics 'runtime)
    (rec_clauses t [])))

;; Merge the already received messages back into the process message
;; queue in the right order. Do this by first receiving the rest of
;; the messages in the queue then sending them all bakc to ourselves.

(defun merge-queue (ms)
  (fletrec ((recv-all (ms)		;Receive all remaining messages
		      (receive
			(msg (recv-all (cons msg ms)))
			(after 0
			       (reverse ms))))
	    (send-all (ms self)		;Send them all back to ourselves
		      (case ms
			((m . ms)
			 (! self m)
			 (send-all ms self))
			(() 'ok))))
    (send-all (recv-all ms) (self))))

;; (eval-try body env) -> value
;;  Complicated by checking legal combinations of options.

(defun eval-try (body env)
  (case body
    ((e ('case . cls) . catch)
     (eval-try-catch catch e (list cls) env))
    ((e . catch)
     (eval-try-catch catch e () env))))

(defun eval-try-catch (body e case env)
  (case body
    ((('catch . cls))
     (eval-try e case (list cls) () env))
    ((('catch . cls) ('after . b))
     (eval-try e case (list cls) (list b) env))
    ((('after . b))
     (eval-try e case () (list b) env))))

(defun eval-try (e case catch after env)
  (try
    (eval-expr e env)
    (case
	(r (case case
	     ((cls) (eval-case-clauses r cls env))
	     (() r))))
    (catch
      ((tuple class val _)
       ;; Get stack trace explicitly.
       (let ((stk (: erlang get_stacktrace)))
	 (case catch
	   ((cls) (eval-catch-clauses (tuple class val stk) cls env))
	   (() (: erlang raise class val stk))))))
    (after
      (case after
	((b) (eval-body b env))
	(() ())))))

(defun eval-catch-clauses
  ([v ((pat . b) . cls) env]
   (case (match-when pat v b env)
     ((tuple 'yes b vbs) (eval-body b (add_vbindings vbs env)))
     ('no (eval-catch-clauses v cls env))))
  ([(tuple class val stk) () _]
   (: erlang raise class val stk)))

(defun eval-call (b env)
  (case b
    ((m f . as)
     (let ((m (eval-expr m env))
	   (f (eval-expr f env))
	   (as (eval-list as env)))
       (: erlang apply m f as)))))

;; (match-when pattern value body env) -> #('yes restbody bindings) | 'no.
;;  Try to match pattern and evaluate guard.

(defun match-when (pat val body env)
  (case (match pat val env)
    ((tuple 'yes vbs)
     (case body
       ((('when g) . body)
	;; Guards are fault safe.
	(try
	  (eval-gexpr g (add_vbindings vbs env))
	  (case ('true (tuple 'yes body vbs))
	    (_ 'no))
	  (catch
	    ((tuple t v i) 'no))))
       (_ (tuple 'yes body vbs))))
    ('no 'no)))

;; (eval-gexpr sexpr environment) -> value.
;;  Evaluate a guard sexpr in the current environment.

(defun eval-gexpr (e env)
  (case e
    ;; Handle the Core data special forms.
    (('quote e) e)
    (('cons x y)
     (cons (eval-gexpr x env) (eval-gexpr y env)))
    (('car x)
     (car (eval-gexpr x env)))
    (('cdr x)
     (cdr (eval-gexpr x env)))
    (('list . xs) (eval-glist xs env))
    (('tuple . xs) (list_to_tuple (eval-glist xs env)))
    ;; Handle the Core closure special forms.
    (('let . body)
     (eval-let body env))
    ;; Handle the Core control special forms.
    (('progn . b) (eval-gbody b env))
    (('if test t f)
     (eval-gif test t f env))
    (('if test t)
     (eval-gif test t 'false env))
    (('case e . cls)
     (eval-gcase e cls env))
    (('call 'erlang f . as)
     (let ((f (eval-gexpr f env))
	   (ar (length as)))
       (case (get_gbinding f ar env)
	 ((tuple 'yes m f) (: erlang apply m f (eval-glist as env)))
	 (_ (: erlang error (tuple 'unbound_func (tuple f (length as))))))))
    ((f . as) (when (is_atom f))
     (let ((ar (length as)))
       (case (get_gbinding f ar env)
	 ((tuple 'yes m f) (: erlang apply m f (eval-glist as env)))
	 ('no (: erlang error (tuple 'unbound_func (tuple f ar)))))))
    ((f . es)				;Everything else not allowed
     (: erlang error 'illegal_guard))
    (e (if (is_atom e)
	 (case (get_vbinding e env)
	   ((tuple 'yes val) val)
	   (no (: erlang error (tuple 'unbound_symb e))))
	 e))))				;Atoms evaluate to themselves

(defun eval-glist (es env)
  (map (lambda (e) (eval-gexpr e env)) es))

(defun eval-gbody
  (((e) env) (eval-gexpr e env))
  (((e . es) env)
   (eval-gexpr e env) (eval-gbody es env))
  ((() env) ()))

(defun eval-glet (vbs body env0)
  (let ((env (foldl (match-lambda
		      (((v e) env) (when (is_atom v))
		       (add_vbinding v (eval-gexpr e env0) env)))
		    env0 vbs)))
    (eval-gbody body env)))

(defun eval-gif (test t f env)
  (if (eval-gexpr test env)
    (eval-gexpr t env)
    (eval-gexpr f env)))

(defun eval-gcase (e cls env)
  (eval-gcase-clauses (eval-gexpr e env) cls env))

(defun eval-gcase-clauses (v cls env)
  (case cls
    (((pat . body) . cls)
     (case (match-when pat v body env)
       ((tuple 'yes body vbs) (eval-gbody body (add_vbindings vbs env)))
       ('no (eval-gcase-clauses v cls env))))))

;; (match pattern value env) -> (tuple 'yes bs) | 'no
;;  Try to match Pattern against Value within the current environment
;;  returning bindings. Bindings is an orddict.

(defun match (pat val env) (match pat val env ()))

(defun match
  ((('quote p) val env bs)
   (if (=:= p val) (tuple 'yes bs) 'no))
  ((('tuple . ps) val env bs)
   (if (is_tuple val)
     (match ps (tuple_to_list val) env bs)
     'no))
  ((('binary . fs) val env bs)
   (if (is_binary val)
     (match-binary fs val env bs)
     'no))
  ((('= p1 p2) val env bs)		;Aliases
   (case (match p1 val env bs)
     ((tuple 'yes bs) (match p2 val env bs))
     ('no 'no)))
  (((p . ps) (v . vs) env bs)
   (case (match p v env bs)
     ((tuple 'yes bs) (match ps vs env bs))
     ('no 'no)))
  ((() () env bs) (tuple 'yes bs))
  ((symb val env bs) (when (is_atom symb))
   (match-symb symb val env bs))
  ((pat val env bs)
   (if (=:= pat val)
     (tuple 'yes bs)
     'no)))

(defun match-symb (symb val env bs)
  (if (== symb '_) (tuple 'yes bs)	;Don't care variable
      ;; Check if symbol already bound.
      (case (find symb bs)
	((tuple 'ok _) 'no)		;Already bound, multiple variable
	('error (tuple 'yes (store symb val bs))))))

;; (match-binary fields binary env bindings) -> (tuple 'yes bindings) | 'no.
;;  Match Fields against Binary. This code is taken from
;;  eval_bits.erl.  Use catch to trap bad matches when getting value,
;;  errors become no match. Split into two passes so as to throw error
;;  on bitspec error and return no on all no matches.

(defun match-binary (fs bin env bs)
  (let ((psps (map (lambda (f) (parse-field f env)) fs)))
    (case (catch (match-fields psps bin env bs))
      ((tuple 'yes #b() bs) (tuple 'yes bs))
      ((tuple 'yes _ _) 'no)
      ('no 'no))))

(defun match-fields (psps bin env bs)
  (case psps
    (((tuple pat specs) . psps)
     (case (match-field pat specs bin env bs)
       ((tuple 'yes bin bs) (match-fields psps bin env bs))
       ('no 'no)))
    (() (tuple 'yes bin bs))))

(defun match-field (pat spec bin0 env bs0)
  (let (((tuple val bin1) (get-pat-field bin0 spec)))
    (case (match pat val env bs0)
      ((tuple 'yes bs1) (tuple 'yes bin1 bs1))
      ('no 'no))))

(defun get-pat-field (bin spec)
  (case spec
    ;; Integer types.
    ((tuple 'integer sz un si en) (get-int-field bin (* sz un) si en))
    ;; Unicode types, ignore unused fields.
    ((tuple 'utf8 _ _ _ _) (get-utf-8-field bin))
    ((tuple 'utf16 _ _ _ en) (get-utf-16-field bin en))
    ((tuple 'utf32 _ _ _ en) (get-utf-32-field bin en))
    ;; Float types.
    ((tuple 'float sz un _ en) (get-float-field bin (* sz un) en))
    ;; Binary types.
    ((tuple 'binary 'all un _ _)
     (let ((0 (rem (: erlang bit_size bin) un)))
       (tuple bin #b())))
    ((tuple 'binary sz un _ _)
     (let* ((tot-size (* sz un))
	    ((binary (val bitstring (size tot-size)) (rest bitstring)) bin))
       (tuple val rest)))))

(defun get-int-field
  ([bin sz 'signed 'little]
   (let (((binary (val signed little-endian (size sz))
		  (rest binary (unit 1))) bin))
     (tuple val rest)))
  ([bin sz 'unsigned 'little]
   (let (((binary (val unsigned little-endian (size sz))
		  (rest binary (unit 1))) bin))
     (tuple val rest)))
  ([bin sz 'signed 'native]
   (let (((binary (val signed native-endian (size sz))
		  (rest binary (unit 1))) bin))
     (tuple val rest)))
  ([bin sz 'unsigned 'native]
   (let (((binary (val unsigned native-endian (size sz))
		  (rest binary (unit 1))) bin))
     (tuple val rest)))
  ([bin sz 'signed 'big]
   (let (((binary (val signed big-endian (size sz))
		  (rest binary (unit 1))) bin))
     (tuple val rest)))
  ([bin sz 'unsigned 'big]
   (let (((binary (val unsigned big-endian (size sz))
		  (rest binary (unit 1))) bin))
     (tuple val rest))))

(defun get-utf-8-field (bin)
  (let (((binary (val utf-8) (rest bitstring)) bin))
    (tuple val rest)))

(defun get-utf-16-field (bin en)
  (case en
    ('little (let (((binary (val utf-16 little-endian) (rest bitstring)) bin))
	       (tuple val rest)))
    ('native (let (((binary (val utf-16 native-endian) (rest bitstring)) bin))
	       (tuple val rest)))
    ('big (let (((binary (val utf-16 big-endian) (rest bitstring)) bin))
	    (tuple val rest)))))

(defun get-utf-32-field (bin en)
  (case en
    ('little (let (((binary (val utf-32 little-endian) (rest bitstring)) bin))
	       (tuple val rest)))
    ('native (let (((binary (val utf-32 native-endian) (rest bitstring)) bin))
	       (tuple val rest)))
    ('big (let (((binary (val utf-32 big-endian) (rest bitstring)) bin))
	    (tuple val rest)))))

(defun get-float-field (bin sz en)
  (case en
    ('little
     (let (((binary (val float little-endian (size sz)) (rest binary (unit 1)))
	    bin))
       (tuple val rest)))
    ('native
     (let (((binary (val float native-endian (size sz)) (rest binary (unit 1)))
	    bin))
       (tuple val rest)))
    ('big
     (let (((binary (val float big-endian (size sz)) (rest binary (unit 1)))
	    bin))
       (tuple val rest)))))
