;; Copyright (c) 2013 Duncan McGreggor <oubiwann@gmail.com>
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