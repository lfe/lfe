;; Copyright (c) 2016 Duncan McGreggor <oubiwann@cogitat.io>
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

;; File    : version.lfe
;; Author  : Duncan McGreggor
;; Purpose : Demonstrating module call and macro/alias call to version function

;; Here is some example usage:
;;
;; lfe> (slurp "example/version.lfe")
;; #(ok version)
;;
;; Not that after the first call to `version` or `lfe_system_info:version`,
;; the results are memoized and thus subsequent calls are much faster.
;;
;; lfe> (get-version-1)
;; ...
;; lfe> (get-version-2)
;; ...
;; lfe> (get-lfe-version-1)
;; ...
;; lfe> (get-lfe-version-2)
;; ...

(defmodule version
  (export all))

(defun get-version-1 ()
  (lfe_system_info:version))

(defun get-version-2 ()
  (version))

(defun get-lfe-version-1 ()
  (lfe_system_info:version 'lfe))

(defun get-lfe-version-2 ()
  (version 'lfe))

