;; -*- mode: LFE; indent-tabs-mode: nil -*-
;; Copyright (c) 2024 Robert Virding
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

;; File    : maybe_SUITE.lfe
;; Author  : Robert Virding
;; Purpose : Maybe test suite.

;; By having the test functions in a separate test module
;; 'maybe-tests.lfe' then it is easier to detect problems when
;; compiling them.

(include-file "test_server.lfe")

(defmodule maybe_SUITE
  (export (all 0) (suite 0) (groups 0) (init_per_suite 1) (end_per_suite 1)
          (init_per_group 2) (end_per_group 2)
          (init_per_testcase 2) (end_per_testcase 2))
  (export (test-1 1) (test-2 1) (test-3 1) (test-4 1)
          (testl-1 1) (testl-2 1) (testl-3 1) (testl-4 1)
          (eval-1 1) (evall-1 1))
  )

(defun all () '(test-1 test-2 test-3 test-4
                testl-1 testl-2 testl-3 testl-4
                eval-1 evall-1))

(defun suite () ())

(defun groups () ())

(defun init_per_suite (config)
  (let* ((dpath (config 'data_dir config))
         (lfile (filename:join dpath "maybe-tests.lfe")))
    ;; (case (lfe_comp:file lfile `(return #(outdir ,dpath)))
    (case (lfe_comp:file lfile `(return))
      (`#(ok ,_ ,_) config)
      (`#(error ,_ ,_ ,_) #(skip "Error in maybe_tests.lfe\n"))
      )
    ))

(defun end_per_suite (config) 'ok)

(defun init_per_group (name config) config)

(defun end_per_group (name config) config)

(defun init_per_testcase (func config)
  config)

(defun end_per_testcase (func config)
  config)

(defun test-1
  (['suite] ())
  (['doc] "Testing t1")
  ([config] (when (is_list config))
   (test-pat 141 (maybe-tests:t1 'good))
   (test-pat error (maybe-tests:t1 'bada))
   (test-pat wrong (maybe-tests:t1 'badb))
   'ok))

(defun test-2
  (['suite] ())
  (['doc] "Testing t2")
  ([config] (when (is_list config))
   (test-pat 141 (maybe-tests:t2 'good))
   (test-pat #(got error) (maybe-tests:t2 'bada))
   (test-pat #(got wrong) (maybe-tests:t2 'badb))
   'ok))

(defun test-3
  (['suite] ())
  (['doc] "Testing t2")
  ([config] (when (is_list config))
   (test-pat #(ok 99) (maybe-tests:t3 'good))
   (test-pat error (maybe-tests:t3 'bada))
   (test-pat wrong (maybe-tests:t3 'badb))
   'ok))

(defun test-4
  (['suite] ())
  (['doc] "Testing t2")
  ([config] (when (is_list config))
   (test-pat #(ok 99) (maybe-tests:t4 'good))
   (test-pat #(got error) (maybe-tests:t4 'bada))
   (test-pat #(got wrong) (maybe-tests:t4 'badb))
   'ok))

(defun testl-1
  (['suite] ())
  (['doc] "Testing t1")
  ([config] (when (is_list config))
   (test-pat 141 (maybe-tests:tl1a 'good))
   (test-pat error (maybe-tests:tl1a 'bada))
   (test-pat wrong (maybe-tests:tl1a 'badb))
   (test-pat 141 (maybe-tests:tl1b 'good))
   (test-pat error (maybe-tests:tl1b 'bada))
   (test-pat wrong (maybe-tests:tl1b 'badb))
   'ok))

(defun testl-2
  (['suite] ())
  (['doc] "Testing t1")
  ([config] (when (is_list config))
   (test-pat 141 (maybe-tests:tl2a 'good))
   (test-pat #(got error) (maybe-tests:tl2a 'bada))
   (test-pat #(got wrong) (maybe-tests:tl2a 'badb))
   (test-pat 141 (maybe-tests:tl2b 'good))
   (test-pat #(got error) (maybe-tests:tl2b 'bada))
   (test-pat #(got wrong) (maybe-tests:tl2b 'badb))
   'ok))

(defun testl-3
  (['suite] ())
  (['doc] "Testing t2")
  ([config] (when (is_list config))
   (test-pat #(ok 99) (maybe-tests:tl3a 'good))
   (test-pat error (maybe-tests:tl3a 'bada))
   (test-pat wrong (maybe-tests:tl3a 'badb))
   (test-pat #(ok 99) (maybe-tests:tl3b 'good))
   (test-pat error (maybe-tests:tl3b 'bada))
   (test-pat wrong (maybe-tests:tl3b 'badb))
   'ok))

(defun testl-4
  (['suite] ())
  (['doc] "Testing t2")
  ([config] (when (is_list config))
   (test-pat #(ok 99) (maybe-tests:tl4a 'good))
   (test-pat #(got error) (maybe-tests:tl4a 'bada))
   (test-pat #(got wrong) (maybe-tests:tl4a 'badb))
   (test-pat #(ok 99) (maybe-tests:tl4b 'good))
   (test-pat #(got error) (maybe-tests:tl4b 'bada))
   (test-pat #(got wrong) (maybe-tests:tl4b 'badb))
   'ok))

(defun eval-1
  (['suite] ())
  (['doc] "Testing t1")
  ([config] (when (is_list config))
   (test-pat #(ok 141) (eval-expr "(maybe (?= a 42) (?= b 99) (+ a b))"))
   (test-pat #(ok error)
             (eval-expr "(maybe (?= `#(ok ,a) 'error) (?= b 99) (+ a b))"))
   (test-pat #(ok wrong)
             (eval-expr "(maybe (?= a 42) (?= `#(ok ,b) 'wrong) (+ a b))"))
   (test-pat #(ok #(b good))
             (eval-expr "(maybe (?= a 42) (?= b #(b good)))"))
   ;; Catch throws and errors.
   (test-pat `#(error ,_)
             (eval-expr "(maybe (?= a 42) (?= b (error #(b error))))"))
   (test-pat `#(throw ,_)
             (eval-expr "(maybe (?= a 42) (?= b (throw #(b throw))))"))
   'ok))

(defun evall-1
  (['suite] ())
  (['doc] "Testing t1")
  ([config] (when (is_list config))
   (test-pat #(ok 141)
             (eval-expr "(maybe (?= a 42)
                                (let ((x (time))) (?= b 99) (+ a b)))"))
   (test-pat #(ok error)
             (eval-expr "(maybe (?= `#(ok ,a) 'error)
                                (let ((x (time))) (?= b 99) (+ a b)))"))
   (test-pat #(ok wrong)
             (eval-expr "(maybe (?= a 42)
                                (let ((x (time)))
                                  (?= `#(ok ,b) 'wrong) (+ a b)))"))
   (test-pat #(ok #(b good))
             (eval-expr "(maybe (?= a 42)
                                (let ((x (time))) (?= b #(b good))))"))
   ;; Catch throws and errors.
   (test-pat `#(error ,_)
             (eval-expr "(maybe (?= a 42)
                                (let ((x (time))) (?= b (error #(b error)))))"))
   (test-pat `#(throw ,_)
             (eval-expr "(maybe (?= a 42)
                                (let ((x (time))) (?= b (throw #(b throw)))))"))
   'ok))

(defun eval-expr (string)
  (let* ((`#(ok ,ts ,_) (lfe_scan:string string))
         (`#(ok ,_ ,expr ,_) (lfe_parse:sexpr ts))
         (expr (case (lfe_macro:expand_expr expr (lfe_env:new))
                 (`#(yes ,exp) exp)
                 ('no expr))))
    ;; Evaluate expression in a try so we can handle both good and errors.
    (try
        `#(ok ,(lfe_eval:expr expr))
      (catch
        (`#(,class ,error ,_) `#(,class ,error)))
      )))
