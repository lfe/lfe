;; Copyright (c) 2013-2020 Duncan McGreggor <oubiwann@gmail.com>
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

;; File    : guessing-game.lfe
;; Author  : Duncan McGreggor
;; Purpose : Demonstrating a random-number guessing game (classic BASIC-era
;;           demo)

;; This file contains a simple demo for guessing a random number, chosen by
;; "the computer". To use, do the following:
;;
;; $ ./bin/lfe
;;
;; > (slurp "examples/guessing-game.lfe")
;; #(ok guessing-game)
;; > (main)
;; Guess the number I have chosen, between 1 and 10.
;; Guess number: 10
;; Your guess is too high.
;; Guess number: 1
;; Your guess is too low.
;; Guess number: 5
;; Your guess is too low.
;; Guess number: 7
;; Well-guessed!!
;; ok
;; >

(defmodule guessing-game
  (export
    (main 0)))

(defun get-player-guess ()
  (let (((tuple 'ok (list guessed)) (io:fread "Guess number: " "~d")))
    guessed))

(defun check-guess (answer guessed)
    (cond
      ((== answer guessed)
        (io:format "Well-guessed!!~n")
        'game-over)
      ((/= answer guessed)
        (if (> answer guessed) (io:format "Your guess is too low.~n"))
        (if (< answer guessed) (io:format "Your guess is too high.~n"))
        (check-guess answer (get-player-guess)))))

(defun main ()
  (io:format "Guess the number I have chosen, between 1 and 10.~n")
  (check-guess
    (random:uniform 10)
    (get-player-guess)))
