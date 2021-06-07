;; Copyright (c) 2021 Duncan McGreggor
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

;; File    : maps-tests.lfe
;; Author  : Duncan McGreggor
;; Purpose : Tests map functions support across multiple versions of Erlang.

(defmodule maps-tests
  "Test various map functions.")

(include-file "ltest-macros.lfe")

(defun test-data ()
  '#m(a 1
      b 2))

(deftest mref
  (is-equal 1 (mref (test-data) 'a)))

(deftest map-get
  (is-equal 2 (map-get (test-data) 'b)))
