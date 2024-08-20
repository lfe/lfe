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

;; File    : tqstring_SUITE.lfe
;; Author  : Eric Bailey
;; Purpose : Test triple-quote strings parse correctly.

(include-file "test_server.lfe")

(defmodule tqstring_SUITE
  "Ensure the tripple quote strings parse correctly."
  (export (all 0) (parse-strings 1)))

(defun all () '(parse-strings))

(defun parse-strings (config)
  (let* ((dpath (config 'data_dir config))
	 (sfile (filename:join dpath "sstring-file"))
	 (tqfile (filename:join dpath "tqstring-file")))
    (let ((`#(ok ,sstrings) (lfe_io:read_file sfile))
	  (`#(ok ,tqstrings) (lfe_io:read_file tqfile)))
      (compare-strings sstrings tqstrings))))

(defun compare-strings
  ([(cons s1 s1s) (cons s2 s2s)]
   (if (=:= s1 s2)
     (compare-strings s1s s2s)
     (erlang:error 'not_equal)))
  ([() ()] 'ok))
