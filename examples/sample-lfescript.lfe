#! /usr/bin/env lfescript
;; -*- mode: lfe -*-
;;! -smp enable -sname factorial -mnesia debug verbose
;;
;; Copyright (c) 2013 Duncan McGreggor <oubiwann@cogitat.io>
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

;; File    : sample-lfescript.lfe
;; Author  : Robert Virding and Duncan McGreggor
;; Purpose : Demonstrating usage of lfescript

;; To use this script, do the following:
;;  1) make sure that you are in your lfe working directory
;;  2) $ export PATH=$PATH:./bin
;;  3) $ export LFESCRIPT_EMULATOR="erl -pa ./ebin"
;;  4) $ chmod 755 examples/sample-lfescript.lfe
;;
;; At this point, you can call this script directly:
;;
;;  $ examples/sample-lfescript.lfe
;;  usage: examples/sample-lfescript.lfe <integer>
;;
;; As you can see, calling the script like that displays the usage statement.
;; Now call the script as intended, like the usage says:
;;
;;  $ examples/sample-lfescript.lfe 12
;;  factorial 12 = 479001600
;;
;; For more information, see ./doc/lfescript.txt.
(defun main
  ([(list string)]
   (try
       (let* ((n (list_to_integer string))
              (f (fac n)))
         (: lfe_io format '"factorial ~w = ~w\n" (list n f)))
     (catch
       ((tuple _ _ _) (usage)))))
  ([_] (usage)))

(defun fac
  ([0] 1)
  ([n] (* n (fac (- n 1)))))

(defun usage ()
  (let ((script-name (: escript script_name)))
    (: lfe_io format '"usage: ~s <integer>\n" (list script-name))))
