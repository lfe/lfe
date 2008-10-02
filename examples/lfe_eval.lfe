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
  (export (eval 1) (eval 2) (apply 2) (apply 3) (make_letrec_env 2) (match 3))
  (import (from lfe_lib (new_env 0) (add_vbinding 3) (add_vbindings 2)
		(vbinding 2) (add_fbinding 4) (add_fbindings 2) (fbinding 3)
		(add_ibinding 5) (gbinding 3))
	  (from lists (reverse 1) (map 2) (foldl 3))
	  (from orddict (find 2) (store 3))))

(defun apply (f args)
  (let ((env (new_env)))
    (lfe-apply (tuple 'expr f env) args env)))

(defun apply (f args env)
  (lfe-apply (tuple 'expr f env) args env))

(defun eval (e) (eval e (new_env)))

(defun eval (e env) (eval-expr e env))

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
    (('begin . body) (eval-body body env))
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
     (let ((ar (length es)))		;Arity
       (case (fbinding f ar env)
	 ((tuple 'yes m f) (: erlang apply m f (eval-list es env)))
	 ((tuple 'yes f) (lfe-apply f (eval-list es env) env))
	 ('no (: erlang error (tuple 'unbound_func (tuple f ar)))))))
    ((f . es)
     (: erlang error #(bad_form application)))
    (e (if (is_atom e)
	 (case (vbinding e env)
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

(defun eval-binary (fs env) (eval-binary fs env #b()))

(defun eval-binary (fs env acc)
  (case fs
    ((f . fs)
     (let ((bin (eval-field f env)))
       (eval-binary fs env
		    (binary (acc binary (unit 1)) (bin binary (unit 1))))))
    (() acc)))

(defrecord spec type size unit sign endian)

(defun def-spec ()			;Make a default spec
  (make-spec 'integer 'default 'default 'default 'default))

(defun eval-field (field env)
  (case field
    ((val . specs)
     (let* ((v (eval-expr val env))
	    ((tuple ty sz un si en) (eval-bitspecs specs (def-spec) env)))
       (eval-exp-field v ty sz un si en)))
    (val
     (let* ((v (eval-expr val env))
	    ((tuple ty sz un si en) (eval-bitspecs () (def-spec) env)))
       (eval-exp-field v ty sz un si en)))))
    
;; (eval-bitspecs specs spec env) -> (tuple type size unit sign end).

(defun eval-bitspecs (specs spec env)
  (case specs
    ((('size n) . ss)
     (let ((size (eval-expr n env)))
       (eval-bitspecs ss (set-spec-size spec size) env)))
    ((('unit n) . ss) (when (is_integer n))
     (eval-bitspecs ss (set-spec-unit spec n) env))
    (('integer . ss)
     (eval-bitspecs ss (set-spec-type spec 'integer) env))
    (('float . ss)
     (eval-bitspecs ss (set-spec-type spec 'float) env))
    (('binary . ss)
     (eval-bitspecs ss (set-spec-type spec 'binary) env))
    (('bitstring . ss)
     (eval-bitspecs ss (set-spec-type spec 'bitstring) env))
    (('signed . ss)
     (eval-bitspecs ss (set-spec-sign spec 'signed) env))
    (('unsigned . ss)
     (eval-bitspecs ss (set-spec-sign spec 'unsigned) env))
    (('big-endian . ss)
     (eval-bitspecs ss (set-spec-endian spec 'big) env))
    (('little-endian . ss)
     (eval-bitspecs ss (set-spec-endian spec 'little) env))
    (('native-endian . ss)
     (eval-bitspecs ss (set-spec-endian spec 'native) env))
    (()
     (let (((match-spec type size unit sign end) spec))
       ;; Adjust values depending on type and given value.
       (flet ((val-or-def (v def) (if (=:= v 'default) def v)))
	 (case type
	   ('integer
	    (tuple 'integer
		   (val-or-def size 8) (val-or-def unit 1)
		   (val-or-def sign 'unsigned) (val-or-def end 'big)))
	   ('float
	    (tuple 'float
		   (val-or-def size 64) (val-or-def unit 1)
		   (val-or-def sign 'unsigned) (val-or-def end 'big)))
	   ('binary
	    (tuple 'integer
		   (val-or-def size 'all) (val-or-def unit 8)
		   (val-or-def sign 'unsigned) (val-or-def end 'big)))
	   ('bitstring
	    (tuple 'binary
		   (val-or-def size 'all) (val-or-def unit 1)
		   (val-or-def sign 'unsigned) (val-or-def end 'big)))))))))

;; (eval-exp-field value type size unit sign endian) -> binary().

(defun eval-exp-field
  ;; Integer types.
  ([val 'integer sz un 'signed 'little]
   (binary (val (size (* sz un)) signed little-endian)))
  ([val 'integer sz un 'unsigned 'little]
   (binary (val (size (* sz un)) unsigned little-endian)))
  ([val 'integer sz un 'signed 'native]
   (binary (val (size (* sz un)) signed native-endian)))
  ([val 'integer sz un 'unsigned 'native]
   (binary (val (size (* sz un)) unsigned native-endian)))
  ([val 'integer sz un 'signed 'big]
   (binary (val (size (* sz un)) signed big-endian)))
  ([val 'integer sz un 'unsigned 'big]
   (binary (val (size (* sz un)) unsigned big-endian)))
  ;; Float types.
  ([val 'float sz un _ 'little]
   (binary (val float (size (* sz un)) little-endian)))
  ([val 'float sz un _ 'native]
   (binary (val float (size (* sz un)) native-endian)))
  ([val 'float sz un _ 'big]
   (binary (val float (size (* sz un)) big-endian)))
  ;; Binary types.
  ([val 'binary 'all un _ _]
   (case (: erlang bit_size val)
     (size (when (=:= (rem size un) 0))
	   (binary (val binary (size size) (unit 1))))
     (_ (: erlang error 'bad_arg))))
  ([val 'binary sz un _ _]
   (binary (val binary (size (* sz un)) (unit 1)))))

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
  (let ((env (bind-args args vals env)))
    (eval-body body env)))

(defun bind-args
  (((a . as) (e . es) env) (when (is_atom a))
   (bind-args as es (add_vbinding a e env)))
  ((() () env) env))

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
    (((pats . body) . cls)
     (if (== (length as) (length pats))
       (case (match-when pats as body env)
	 ((tuple 'yes body1 vbs) (eval-body body1 (add_vbindings vbs env)))
	 ('no (eval-match-clauses as cls env)))
       (eval-match-clauses as cls env)))))

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
			  (add_vbindings bs env))))
		     env0 vbs)))
    (eval-body b env)))

;; (eval-let-function (FuncBindings . Body) Env) -> Value.

(defun eval-let-function (body env0)
  (let* (((fbs . b) body)
	 (env (foldl (match-lambda
		       ([(v (= ('lambda as . b) f)) e]
			(when (is_atom v))
			(add_fbinding v (length as) (tuple 'expr f env0) e))
		       ([(v (= ('lambda as . b) f)) e]
			(when (is_atom v))
			(add_fbinding v (length as) (tuple 'expr f env0) e)))
		     env0 fbs)))
    (eval-body b env)))
			
;; (eval-letrec-function (FuncBindings . Body) Env) -> Value.
;;  This is a tricky one. But we dynamically update the environment
;;  each time we are called.

(defun eval-letrec-function (body env0)
  (let* (((fbs . b) body)
	 (fbs1 (map (match-lambda
		      ([(v (= ('lambda args . body) f))] (when (is_atom v))
			(tuple v (length args) f))
		      ([(v (= ('match-lambda (pats . _) . _) f))]
		       (when (is_atom v))
		       (tuple v (length pats) f)))
		    fbs))
	 (env (make_letrec_env fbs1 env0)))
    (eval-body body env)))

(defun make_letrec_env (fbs0 env)
  (let ((fbs (map (lambda (fb)
		    (let (((tuple v ar body) fb))
		      (tuple v ar (tuple 'letrec body fbs0 env))))
		  fbs0)))
    (add_fbindings fbs env)))

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
    (('begin . b) (eval-gbody b env))
    (('if test t f)
     (eval-gif test t f env))
    (('if test t)
     (eval-gif test t 'false env))
    (('case e . cls)
     (eval-gcase e cls env))
    (('call 'erlang f . as)
     (let ((f (eval-gexpr f env))
	   (ar (length as)))
       (case (gbinding f ar env)
	 ((tuple 'yes m f) (: erlang apply m f (eval-glist as env)))
	 (_ (: erlang error (tuple 'unbound_func (tuple f (length as))))))))
    ((f . as) (when (is_atom f))
     (let ((ar (length as)))
       (case (gbinding f ar env)
	 ((tuple 'yes m f) (: erlang apply m f (eval-glist as env)))
	 ('no (: erlang error (tuple 'unbound_func (tuple f ar)))))))
    ((f . es)				;Everything else not allowed
     (: erlang error 'illegal_guard))
    (e (if (is_atom e)
	 (case (vbinding e env)
	   ((tuple 'yes val) val)
	   (no (: erlang error (tuple 'unbound_symb e))))
	 e))))				;Atoms evaluate to themselves

(defun eval-glist (es env)
  (map (lambda (e) (eval-gexpr e env)) es))

(defun eval-gbody
  (((e) env) (eval-gexpr e env))
  (((e . es) env)
   (begin (eval-gexpr e env) (eval-gbody es env)))
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
;;  Match Fields against Binary. This code is taken from eval_bits.erl.
;;  Use catch to trap bad matches when getting value, errors become
;;  no match.

(defun match-binary (fs bin env bs)
  (case fs
    ((f . fs)
     (case (catch (match-field f bin env bs))
       ((tuple 'yes bs bin) (match-binary fs bin env bs))
       ('no 'no)
       (_ 'no)))			;Catch errors
    (()
     (if (=:= bin #b())
       (tuple 'yes bs)
       'no))))

(defun match-field (f bin env bs)
  (case f
    ((pat . specs)
     (let ((spec-t (eval-bitspecs specs (def-spec) env)))
       (match-field pat spec-t bin env bs)))
    (pat
     (let ((spec-t (eval-bitspecs '() (def-spec) env)))
       (match-field pat spec-t bin env bs)))))

(defun match-field (pat spec bin env bs)
  (let* (((match-spec ty sz un si en) spec) ;Pull spec apart
	 ((tuple val bin) (get-value bin ty sz un si en)))
    (case (match pat val env bs)
      ((tuple 'yes bs1) (tuple 'yes bs bin))
      ('no 'no))))

(defun get-value (bin ty sz un si en)
  (case ty
    ('integer (get-integer bin (* sz un) si en))
    ('float (get-float bin (* sz un) en))
    ('binary
     (if (=:= sz 'all)
       (let ((0 (rem (: erlang bit_size bin) un)))
	 (tuple bin #b()))
       (let* ((tot-size (* sz un))
	      ((binary (val bitstring (size tot-size)) (rest bitstring)) bin))
	 (tuple val rest))))))

(defun get-integer (bin sz si en)
  (case (tuple si en)
    ((tuple 'signed 'little-endian)
     (let (((binary (val signed little-endian (size sz))
		    (rest binary (unit 1))) bin))
       (tuple val rest)))
    ((tuple 'unsigned 'little-endian)
     (let (((binary (val unsigned little-endian (size sz))
		    (rest binary (unit 1))) bin))
       (tuple val rest)))
    ((tuple 'signed 'native-endian)
     (let (((binary (val signed native-endian (size sz))
		    (rest binary (unit 1))) bin))
       (tuple val rest)))
    ((tuple 'unsigned 'native-endian)
     (let (((binary (val unsigned native-endian (size sz))
		    (rest binary (unit 1))) bin))
       (tuple val rest)))
    ((tuple 'signed 'bin-endian)
     (let (((binary (val signed big-endian (size sz))
		    (rest binary (unit 1))) bin))
       (tuple val rest)))
    ((tuple 'unsigned 'big-endian)
     (let (((binary (val unsigned big-endian (size sz))
		    (rest binary (unit 1))) bin))
       (tuple val rest)))))

(defun get-float (bin sz en)
  (case en
    ('little-endian
     (let (((binary (val float little-endian (size sz)) (rest binary (unit 1)))
	    bin))
       (tuple val rest)))
    ('native-endian
     (let (((binary (val float native-endian (size sz)) (rest binary (unit 1)))
	    bin))
       (tuple val rest)))
    ('big-endian
     (let (((binary (val float big-endian (size sz)) (rest binary (unit 1)))
	    bin))
       (tuple val rest)))))
