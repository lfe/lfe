;; Copyright (c) 2013 Duncan McGreggor <oubiwann@cogitat.io>
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
;;  $ cd examples
;;  $ ../bin/lfe -pa ../ebin
;;
;; > (slurp '"internal-state.lfe")
;; #(ok internal-state)
;; > (set account (new-account '"Alice" 100.00 0.06))
;; #Fun<lfe_eval.10.53503600>
;; > (name account)
;; "Alice"
;; > (balance account)
;; 100.0
;; > (set account (interest account))
;; #Fun<lfe_eval.10.53503600>
;; > (balance account)
;; 106.0
;; > (set account (withdraw account 54.90))
;; #Fun<lfe_eval.10.53503600>
;; > (set account (withdraw account 54.90))
;; exception error: insufficient-funds
;;
;; > (balance account)
;; 51.1
;; > (set account (deposit account 1000))
;; #Fun<lfe_eval.10.53503600>
;; > (set account (withdraw account 54.90))
;; #Fun<lfe_eval.10.53503600>
;; > (set account (withdraw account 54.90))
;; #Fun<lfe_eval.10.53503600>
;; > (set account (withdraw account 54.90))
;; #Fun<lfe_eval.10.53503600>
;; > (balance account)
;; 886.4
;; > (set account (interest account))
;; #Fun<lfe_eval.10.53503600>
;; > (balance account)
;; 939.584

(defmodule internal-state
 (export all))

(defun new-account (name balance interest-rate)
  (lambda (message)
    (case message
      ('withdraw (lambda (amt)
                    (if (=< amt balance)
                        (new-account name (- balance amt) interest-rate)
                        (: erlang error 'insufficient-funds))))
      ('deposit (lambda (amt) (new-account name (+ balance amt) interest-rate)))
      ('balance (lambda () balance))
      ('name (lambda () name))
      ('interest (lambda ()
                    (new-account
                      name
                      (+ balance (* balance interest-rate))
                      interest-rate))))))

(defun get-method (object command)
  (funcall object command))

(defun send (object command arg)
  (funcall (get-method object command) arg))

; returns an updated account object
(defun withdraw (object amt)
  (funcall (get-method object 'withdraw) amt))

; returns an updated account object
(defun deposit (object amt)
  (funcall (get-method object 'deposit) amt))

; returns a float representing the balance
(defun balance (object)
  (funcall (get-method object 'balance)))

; returns a string represnting the account holder
(defun name (object)
  (funcall (get-method object 'name)))

; returns an updated account object
(defun interest (object)
  (funcall (get-method object 'interest)))