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

;; This module contains utility functions for lfunit. Most of the functionality
;; of lfunit is actually in the included macro file.
;;
;; Note that this code was originally copied by the author from an
;; independent open source project, lfeunit.

(defmodule lfunit
  (export
    (check-failed-assert 2)
    (check-wrong-assert-exception 2)))

(include-lib "include/lfunit.lfe")

(defun check-failed-assert (data expected)
  "This function
    1) unwraps the data held in the error result returned by a failed
       assertion, and
    2) checks the buried failure type against an expected value, asserting
       that they are the same."
  (let (((tuple failure-type _) data))
    (is-equal failure-type expected)))

(defun check-wrong-assert-exception (data expected)
  "This function
    1) unwraps the data held in the error result returned by
       assert-exception when an unexpected error occurs, and
    2) checks the buried failure type against an expected value, asserting
       that they are the same."
  (let (((tuple 'assertException_failed
    (list _ _ _ _ (tuple fail-type _))) data))
    (is-equal fail-type expected)))

(defun get-failure-data (expected expression)
  "Building upon the boilerplate data returned from DEFAULT-DATA, this
  function gets the rest of the data needed when returning results for a
  failed assertion."
  (let* ((value (eval expression))
         (expr-data (add-data 'expression expression (DEFAULT-DATA)))
         (expt-data (add-data 'expected expected expr-data)))
    (add-data 'value value expt-data)))

(defun get-exception-data (expected-class expected-term expression)
  "Building upon the boilerplate data returned from DEFAULT-DATA, this
  function gets the rest of the data needed when returning results for a
  failed exception assertion."
  (let ((pattern
          (++
            '"{ " (atom_to_list expected-class)
            '" , " (atom_to_list expected-term)
            '" , [...] }"))
       (expr-data (add-data 'expression expression (DEFAULT-DATA))))
    (add-data 'pattern pattern expr-data)))

(defun add-data (key value data)
  "A utility function for appending to assert* result data."
  (++ data (list (tuple key value))))
