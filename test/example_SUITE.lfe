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

;; File    : example_SUITE.lfe
;; Author  : Eric Bailey
;; Purpose : Ensure dev/example.lfe compiles correctly.

(include-file "test_server.lfe")

(defmodule example_SUITE
  "Ensure dev/example.lfe compiles correctly."
  (export (all 0) (compile 1)))

(defun all () '(compile))

(defun compile (config)
  (let* ((dpath (config 'data_dir config))
         (efile (filename:join dpath "example.lfe")))
    (line (test-pat #(ok (#(ok example ()) #(ok another-example ())) ())
                    (lfe_comp:file efile '(return))))))
