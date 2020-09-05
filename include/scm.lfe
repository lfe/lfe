;; Copyright (c) 2020 Robert Virding
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

;; File    : scm.erl
;; Author  : Robert Virding
;; Purpose : Lisp Flavoured Erlang scheme include macros

(defmacro begin args `(scm:begin ,@args))
(defmacro define args `(scm:define ,@args))
(defmacro define-syntax args `(scm:define-syntax ,@args))
(defmacro let-syntax args `(scm:let-syntax ,@args))
(defmacro defsyntax args `(scm:defsyntax ,@args))
