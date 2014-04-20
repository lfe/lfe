;; Copyright (c) 2013-2014 Duncan McGreggor
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

;; File    : lfunit.lfe
;; Author  : Duncan McGreggor
;; Purpose : An EUnit wrapper for Lisp Flavoured Erlang.

;; This macro file contains the meat of the code for lfunit.
;;
;; Note that this code was originally copied by the author from an
;; independent open source project, lfeunit.

(include-lib "eunit/include/eunit.hrl")

(defmacro deftest arg
  "This macro is for defining standard EUnit tests."
  (let ((name (car arg))
        (body (cdr arg)))
    `(defun ,(list_to_atom (++ (atom_to_list name) '"_test")) ()
       ,@body)))

(defmacro deftestgen arg
  "This macro is for defining EUnit tests that use test generators."
  (let ((name (re:replace (atom_to_list (car arg))
                          '"-"
                          '"_"
                          '(#(return list) global)))
        (body (cdr arg)))
    `(defun ,(list_to_atom (++ name '"_test_")) ()
       ,@body)))

(defmacro deftestskip arg
  "This macro is for defining standard EUnit test that will be skipped (not
  run)."
  (let ((name (car arg))
        (body (cdr arg)))
    `(defun ,(list_to_atom (++ (atom_to_list name) '"_skip")) ()
       ,@body)))

(defmacro is (bool-expression)
  "This macro takes an expression that returns a boolean value. If the
  expression does not evaluate as a truth value, an error is returned."
  `(assert ,bool-expression))

(defmacro is-not (bool-expression)
  "This macro takes an expression that returns a boolean value. If the
  expression does not evaluate as false value, an error is returned."
  `(assertNot ,bool-expression))

(defmacro is-equal (value expression)
  "This macro checks the equality between a passed value and a quoated
  expression."
  `(assertEqual ,value ,expression))

(defmacro is-not-equal (value expression)
  "This macro checks the inequality between an expected value and a passed
  expression."
  `(assertNotEqual ,value ,expression))

(defmacro is-exception (expected-class expected-term expression)
  "This macro check that the passeed expression raises the expected
  exception class (e.g., 'error, 'throw, etc.) and term (e.g., 'undef,
  'badarith, etc.)."
  `(assertException ,expected-class ,expected-term ,expression))

(defmacro is-not-exception (expected-class expected-term expression)
  "This macro check that the passeed expression does not raise the expected
  exception class (e.g., 'error, 'throw, etc.) and term (e.g., 'undef,
  'badarith, etc.)."
  `(not (is-exception ,expected-class ,expected-term ,expression)))

(defmacro is-error (expected-term expression)
  "This macro is a convenience macro for is-exception with an
  exception class of 'error."
  `(assertError ,expected-term ,expression))

(defmacro is-not-error (expected-term expression)
  "This macro is a convenience macro for is-not-exception with an
  exception class of 'error."
  `(is-not-exception 'error ,expected-term ,expression))

(defmacro is-exit (expected-term expression)
  "This macro is a convenience macro for is-exception with an
  exception class of 'exit."
  `(assertExit ,expected-term ,expression))

(defmacro is-not-exit (expected-term expression)
  "This macro is a convenience macro for is-not-exception with an
  exception class of 'exit."
  `(is-not-exception 'exit ,expected-term ,expression))

(defmacro is-throw (expected-term expression)
  "This macro is a convenience macro for is-exception with an
  exception class of 'throw."
  `(assertThrow ,expected-term ,expression))

(defmacro is-not-throw (expected-term expression)
  "This macro is a convenience macro for is-not-exception with an
  exception class of 'throw."
  `(is-not-exception 'throw ,expected-term ,expression))

(defmacro is-match (guard expression)
  "This macro checks an expression against a guard."
  `(assertMatch ,guard ,expression))

(defmacro DEFAULT-DATA ()
  "This macro returns the boilerplate needed for every assertion's failure
  cases."
  `(list
     (tuple 'module (MODULE))
     (tuple 'line (LINE))))
