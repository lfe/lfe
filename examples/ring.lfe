;; Copyright (c) 2014 Duncan McGreggor <oubiwann@cogitat.io>
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

;; File    : ring.lfe
;; Author  : Duncan McGreggor
;; Purpose : Demonstrating the classic ring benchmark

;; The code below was translated from the Erlang ring benchmark written by
;; Jiri Isa and optimized by Shun Shino:
;;  http://benchmarksgame.alioth.debian.org/u32/program.php?test=threadring&lang=hipe&id=1
;;
;; The LFE version split the logic of the above-mentioned Erlang code into more
;; functions for increased clarity. Note that the additional calls do make this
;; version a fraction less efficient.
;;
;; After passing the desired parameters to lfe/erl, the main function takes
;; just two parameters:
;;  1) the number of processes to create, and
;;  2) the number of trips around the ring to make.
;;
;; To use the code below in LFE, do the following:
;;
;;  $ make compile
;;  $ cd examples
;;  $ ../bin/lfe -pa ../ebin -smp disable -noshell -run ring main 503 50000000
;;
;; This should give the following output:
;;
;;  Result: 292
;;
(defmodule ring
  (export
    (main 1)
    (roundtrip 2)))

(defun main (args)
  (apply
    #'start-ring/2
    (lists:map #'list_to_integer/1 args)))

(defun start-ring (process-count traversal-count)
  (let ((batch (make-processes process-count traversal-count)))
    (! batch traversal-count)
    (roundtrip 1 batch)))

(defun make-processes (process-count traversal-count)
  (lists:foldl
    #'make-process/2
    (self)
    (lists:seq process-count 2 -1)))

(defun make-process (id pid)
  (spawn 'ring 'roundtrip (list id pid)))

(defun roundtrip (id pid)
  (receive
    (1
      (io:fwrite '"Result: ~b~n" (list id))
      (erlang:halt))
    (data
      (! pid (- data 1))
      (roundtrip id pid))))
