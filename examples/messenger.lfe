;; Copyright (c) 2013-2020 Duncan McGreggor <oubiwann@cogitat.io>
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

;; File    : messenger.lfe
;; Author  : Duncan McGreggor
;; Purpose : Demonstrating message passing to spawned processes

;; This file contains a simple demo for passing messages to an Erlang process
;; in LFE. To use, do the following:
;;
;; $ ./bin/lfe
;;
;; lfe> (c "examples/messenger")
;; #(module messenger)
;; lfe> (set pid (spawn 'messenger 'print-result ()))
;; <0.34.0>
;; lfe> (! pid "Zaphod was here.")
;; "Zaphod was here."
;; Received message: 'Zaphod was here.'
;; lfe> (! pid "Ford is missing.")
;; "Ford is missing."
;; Received message: 'Ford is missing.'
;; lfe> (! pid "Arthur is pining for Trillian.")
;; "Arthur is pining for Trillian."
;; Received message: 'Arthur is pining for Trillian.'

(defmodule messenger
  (export 
    (print-result 0)))

(defun print-result ()
  (receive
    (msg
      (io:format "Received message: '~s'~n" (list msg))
      (print-result))))
