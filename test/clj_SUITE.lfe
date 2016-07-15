;; Copyright (c) 2016 Eric Bailey
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

;; File    : clj_SUITE.lfe
;; Author  : Eric Bailey
;; Purpose : Test clj exports.

(include-file "test_server.lfe")

(defmodule clj_SUITE
  "Test clj exports."
  (export (all 0) (identity 1) (compose 1) (partial 1)))

(defmacro is (expr)
  `(let (('true ,expr))
     ,expr))

(defun all () '(identity compose))

(defun identity (_config)
  (lc ((<- x '(atom
               0 42
               0.0 3.14
               #\c
               "" "abc"
               #"" #"abc"
               () (1 2)
               #() #(1 2)
               #m()
               #m(a 1 b 2))))
    (line (is (=:= (clj:identity x) x))))
  (line (test-pat 3 (clj:identity (+ 1 2))))
  (line (test-pat 'true (clj:identity (> 5 0)))))

(defun compose (_config)
  (flet ((c0 (x) (funcall (clj:compose) x)))
    (lc ((<- x '(atom
                 0 42
                 0.0 3.14
                 #\c
                 "" "abc"
                 #"" #"abc"
                 () (1 2)
                 #() #(1 2)
                 #m()
                 #m(a 1 b 2))))
      (line (is (=:= (clj:identity x) (c0 x)))))
    (line (is (=:= (clj:identity (+ 1 2 3)) (c0 6))))
    (line (is (=:= (clj:identity (quote foo)) (c0 'foo))))))

(defun partial (_config)
  (flet (;; (p0 (x) (funcall (clj:partial inc) x))
         (p1 (x) (funcall (clj:partial #'+/2 20) x))
         ;; (p2 (x) (funcall (clj:partial conj #(1 2)) x))
         )
    ;; (line (is (=:= 41 (p0 40))))
    (line (is (=:= 40 (p1 20))))
    ;; (line (is (=:= #(1 2 3) (p2 3))))
    ))
