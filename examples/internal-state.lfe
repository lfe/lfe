;; Copyright (c) 2013, 2015 Duncan McGreggor <oubiwann@gmail.com>
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

;; File    : internal-state.lfe
;; Author  : Duncan McGreggor
;; Purpose : Demonstrating keeping state with closures

;; This file contains a conversion of some of the Common Lisp code from the
;; beginning of Chapter 13 in Peter Norvig's "Paradigms of Artificial
;; Intelligence Programming: Case Studies in Common Lisp".
;;
;; To use the code below in LFE, do the following:
;;
;;  $ ./bin/lfe -pa ./ebin
;;
;; > (slurp "examples/internal-state.lfe")
;; #(ok internal-state)
;; > (set acct (new-account "Alice" 100.00 0.06))
;; #Fun<lfe_eval.10.53503600>
;; > (send acct 'name)
;; "Alice"
;; > (send acct 'balance)
;; 100.0
;; > (set acct (send acct 'apply-interest))
;; #Fun<lfe_eval.10.53503600>
;; > (send acct 'balance)
;; 106.0
;; > (set acct (send acct 'withdraw 54.90))
;; #Fun<lfe_eval.10.53503600>
;; > (set acct (send acct 'withdraw 54.90))
;; exception error: insufficient-funds
;;
;; > (send acct 'balance)
;; 51.1
;; > (set acct (send acct 'deposit 1000))
;; #Fun<lfe_eval.10.53503600>
;; > (set acct (send acct 'withdraw 54.90))
;; #Fun<lfe_eval.10.53503600>
;; > (set acct (send acct 'withdraw 54.90))
;; #Fun<lfe_eval.10.53503600>
;; > (set acct (send acct 'withdraw 54.90))
;; #Fun<lfe_eval.10.53503600>
;; > (send acct 'balance)
;; 886.4
;; > (set acct (send acct 'apply-interest))
;; #Fun<lfe_eval.10.53503600>
;; > (send acct 'balance)
;; 939.584

(defmodule internal-state
 (export all))

(defun new-account (name balance interest-rate)
  (lambda (message)
    (case message
      ('withdraw (lambda (amt)
                    (if (=< amt balance)
                        (new-account name (- balance amt) interest-rate)
                        (error 'insufficient-funds))))
      ('deposit (lambda (amt) (new-account name (+ balance amt) interest-rate)))
      ('balance (lambda () balance))
      ('name (lambda () name))
      ('apply-interest (lambda ()
                    (new-account
                      name
                      (+ balance (* balance interest-rate))
                      interest-rate))))))

(defun send (object method-name)
  "This is a generic function, used to call into the given object (class
  instance)."
  (funcall (funcall object method-name)))

(defun send (object method-name arg)
  "This is a generic function, used to call into the given object (class
  instance)."
  (funcall (funcall object method-name) arg))

;; It is also possible to create functionally equivalent code using LFE
;; processes. The code below would then be used in the following manner:
;;
;; > (set acct (init-account "Alice" 1000 0.1))
;; <0.37.0>
;; > (snd acct 'name)
;; "Alice"
;; > (snd acct 'balance)
;; 1000
;; > (snd acct 'apply-interest)
;; 1.1e3
;; > (snd acct 'deposit 1000)
;; 2.1e3
;; > (snd acct 'balance)
;; 2.1e3
;; > (snd acct 'withdraw 2000)
;; 100.0
;; > (snd acct 'withdraw 101)
;; #(error insufficient-funds)

(defun account-class (name balance interest-rate)
  (receive
    (`#(,method name ())
     (! method `#(ok ,name))
     (account-class name balance interest-rate))
    (`#(,method balance ())
     (! method `#(ok ,balance))
     (account-class name balance interest-rate))
    (`#(,method deposit (,amt))
     (let ((new-balance (+ balance amt)))
       (! method `#(ok ,new-balance))
       (account-class name new-balance interest-rate)))
    (`#(,method apply-interest ())
     (let ((new-balance (+ balance (* balance interest-rate))))
       (! method `#(ok ,new-balance))
       (account-class name new-balance interest-rate)))
    (`#(,method withdraw (,amt))
     (let ((new-balance (- balance amt)))
       (cond ((< new-balance 0)
              (! method #(error insufficient-funds))
              (account-class name balance interest-rate))
             ('true
              (! method `#(ok ,new-balance))
              (account-class name new-balance interest-rate)))))))

(defun init-account (name balance interest-rate)
  (spawn (lambda ()
           (account-class name balance interest-rate))))

(defun snd (object method-name)
  (snd object method-name '()))

(defun snd
  ((object method-name arg) (when (not (is_list arg)))
   (snd object method-name `(,arg)))
  ((object method-name args)
   (! object `#(,(self) ,method-name ,args))
   (receive
     (`#(ok ,result)
      result)
     (error
      error))))
