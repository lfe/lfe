;; Copyright (c) 2013 Joe Armstrong <joearms@gmail.com>, Original Erlang version
;; Copyright (c) 2013-2020 Duncan McGreggor <oubiwann@gmail.com>, LFE version
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

;; File    : joesfav.lfe
;; Author  : Joe Armstrong, Duncan McGreggor
;; Purpose : A conversion of Joe Armstrong's favorite Erlang program

;; The original Erlang code was originally published here, on Joe's blog:
;;    http://joearms.github.io/2013/11/21/My-favorite-erlang-program.html
;;
;; Here is some example usage:
;;
;; $ ./bin/lfe
;;
;; lfe> (slurp "examples/joes-fav.lfe")
;; #(ok joes-fav)
;;
;; Quick sanity check:
;;
;; lfe> (factorial 10)
;; 3628800
;; lfe> (factorial 20)
;; 2432902008176640000
;; lfe> (factorial 50)
;; 30414093201713378043612608166064768844377641568960512000000000000
;;
;; Now, for the real thing:
;;
;; lfe> (run-it)
;; 30414093201713378043612608166064768844377641568960512000000000000
;;
(defmodule joes-fav
  (export all))

(defun universal-server ()
  (receive
    ((tuple 'become server-function)
     (funcall server-function))))

(defun factorial
  ((0) 1)
  ((number) (* number (factorial (- number 1)))))

(defun factorial-server ()
  (receive
    ((tuple sender number)
     (! sender (factorial number))
     (factorial-server))))

(defun run-it ()
  (let ((pid (spawn #'universal-server/0)))
    (! pid (tuple 'become #'factorial-server/0))
    (! pid (tuple (self) 50)))
  (receive
    (data data)))
