;; Copyright (c) 2020 Duncan McGreggor <oubiwann@cogitat.io>
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

;; File    : guessing-game2.lfe
;; Author  : Duncan McGreggor
;; Purpose : Demonstrating a random-number guessing game (classic BASIC-era
;;           demo) using a simple client and server, message-passing, and
;;           pattern-matching

;; This file contains a simple demo for guessing a random number, chosen by
;; "the computer". Outwardly, this example behaves exactly like the first
;; guessing game example. Internally, however, this one differs significantly:
;;
;; 1. It uses a record to track game state
;; 2. It has abstrated a client behaviour and a server behaviour
;; 3. It uses message-passing between these two
;; 4. It uses pattern matching of records in the function heads as well as the
;;    receives to perform flow control (instead of the 'cond' and `if` forms)
;;
;; To use, do the following:
;;
;; $ ./bin/lfe
;;
;; > (slurp "examples/guessing-game2.lfe")
;; #(ok guessing-game)
;; lfe> (main)
;; Guess the number I have chosen, between 1 and 10.
;; Guess number: 1
;; Your guess is too low.
;; Guess number: 10
;; Your guess is too high.
;; Guess number: 5
;; Your guess is too low.
;; Guess number: 7
;; Your guess is too high.
;; Guess number: 6
;; Well-guessed!!
;; game-over
(module guessing-game2)

(export ((main 0)))

(record state
        (server
         client
         answer
         guess
         status))

(function guess-server
  (match-lambda
    ([(record state answer a)]
     (receive
       ((= (record state client p guess g) game) (when (== g a))
        (! p (record-update game state status 'game-over)))
       ((= (record state client p guess g) game) (when (> g a))
        (! p (record-update game state status 'too-high))
        (guess-server game))
       ((= (record state client p guess g) game) (when (< g a))
        (! p (record-update game state status 'too-low))
        (guess-server game))))))

(function guess-client
  (match-lambda
    ([(record state status 'game-over)]
     (io:format "Well-guessed!!~n")
     'game-over)
    ([(= (record state status 'started) game)]
     (io:format "Guess the number I have chosen, between 1 and 10.~n")
     (guess-client (record-update game state status 'running)))
    ([(= (record state status 'too-high) game)]
     (io:format "Your guess is too high.~n")
     (guess-client (record-update game state status 'running)))
    ([(= (record state status 'too-low) game)]
     (io:format "Your guess is too low.~n")
     (guess-client (record-update game state status 'running)))
    ([(= (record state server p) game)]
     (let ((`#(ok (,g)) (io:fread "Guess number: " "~d")))
       (! p (record-update game state client (self) guess g))
       (receive
         (game (guess-client game)))))))

(function main
  (lambda ()
    (let* ((a (rand:uniform 10))
           (s (record state answer a
                      guess 'undefined
                      status 'started))
           (p (spawn (lambda () (guess-server s)))))
      (guess-client (record-update s state server p)))))
