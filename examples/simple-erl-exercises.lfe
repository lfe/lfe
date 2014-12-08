;; Copyright (c) 2008-2013 Sean Chalmers
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

;; File    : simple-erl-exercises.lfe
;; Author  : Sean Chalmers
;; Purpose : LFE Implementation of Erlang introductory exercises.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [Erlang Exercises] (http://www.erlang.org/course/exercises.html)      ;;
;;                                                                       ;;
;; Completed using Lisp Flavoured Erlang by Sean Chalmers, November 2013 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are the exercise solutions that are contained here:             ;;
;;                                                                       ;;
;; - Entering a program                                                  ;;
;; - Simple sequential programs                                          ;;
;; - Simple recursive programs                                           ;;
;; - Interaction between processes, Concurrency                          ;;
;;                                                                       ;;
;; I make no assumptions about these being the best way to solve any     ;;
;; of these problems and I encourange feedback and alternate solutions.  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmodule exercises
  (export (convert 1)
          (perimeter 1)
          (min 1)
          (max 1)
          (min_max 1)
          (min_max2 1)
          (start_pong 1)
          (start_ring 2)
          (start_star 2)))

;; SIMPLE SEQUENTIAL EXERCISES

;; Temperature Conversion Exercise
(defun f2c (c)
  (+ 32 (/ (* c 9) 5)))

(defun c2f (f)
  (/ (* 5 (- f 32)) 9))

(defun convert
  ([(tuple 'c temp)] (tuple 'f (f2c temp)))
  ([(tuple 'f temp)] (tuple 'c (c2f temp))))
;; End of Temperature Conversion Exercise

;; Perimeter calculation by tuple input exercise
(defun perimeter
  ;; Square
  ([(tuple 'square side)] (when (is_number side))
   (tuple 'square (* 4 side)))
  ;; Circle
  ([(tuple 'circle radius)]  (when (is_number radius))
   (tuple 'circle (: math pow (* (: math pi) radius) 2)))
  ;; Triangle
  ([(tuple 'triangle a b c)] (when (is_number a)
                                   (is_number b)
                                   (is_number c))
   (tuple 'triangle (+ a b c))))

;; end of perimeter exercise

;; SIMPLE RECURSIVE EXERCISES

;; I know lists:max/1 and lists:min/1 exist, that isn't the point.
(defun min ([(cons x xs)]
   (: lists foldl (fun erlang min 2) x xs)))

(defun max ([(cons x xs)]
   (: lists foldl (fun erlang max 2) x xs)))

(defun min_max (col)
  ;; Flavourless min_max/1 implementation.
  (tuple (min col) (max col)))

;; Alternative min_max without the little helpers
(defun min_max2 (col)
  (let-function [(gief (match-lambda ([f (cons x xs)] (: lists foldl f x xs))))]
    ;; Create a tuple using our temp function above.
    (tuple (gief (fun erlang min 2) col)
           (gief (fun erlang max 2) col))))

(defun nom-date
  ;; Extract the final digits from the year
  ([(list _ _ a r)] (list a r))
  ;; These two handle if our month or day value is a single digit.
  ([(cons a ())] (list #\0 a))
  ;; Or if it contains two digits
  ([(list a b)] (list a b)))

(defun swedish_date ()
  (: lists foldl ;; I heart fold
    (lambda (x acc)
      (++ acc (nom-date (integer_to_list x))))
    () ;; This is our accumulator
    (tuple_to_list (: erlang date))))

(defun create-pids-one-arg (fn arg col)
  ;; I ended up using this pattern a few times in the next couple of
  ;; exercises so I pulled it out into it's own function.
  (: lists map (lambda (_) (spawn (MODULE) fn (list arg))) col))

(defun pong (n)
  ;; This is the pong receiver.
  (receive
    ;; We've reached the maximum number of passes, so pass it on
    ;; and drop out gracefully.
    ((tuple _ from count) (when (=:= count n))
     (! from (tuple 'ping (self) 10))
     'ok)
    ;; We've received a message, bump the counter and send it back.
    ((tuple 'ping from count)
     (: io format '"caught ball~n" (list))
     (! from (tuple 'ping (self) (+ count 1)))
     ;; Make sure we're still here to get the next message.
     (pong n))))

(defun start_pong (n)
  ;; Creates our players.
  (let [((list a b) (create-pids-one-arg 'pong n '(1 2)))]
    ;; Starts the game and exits because the initiator is not part of play.
    (! a (tuple 'ping b 1))))

(defun start_ring (n-rings n-msgs)
  ;; Create the desired number of "servers" in the ring.
  (let [((cons x xs)
          (create-pids-one-arg 'ring n-msgs (: lists seq 1 n-rings)))]
    ;; Get it rolling.
    (! x (tuple 'pass (++ xs (list x)) 0))))

(defun ring (n-msgs)
  ;; We need a tiny helper function to just ease the rebuild of the ring list.
  ;; I'm sure there is a more efficient/idiomatic method of doing this...
  (let-function [(ring-col (lambda (x xs) (++ xs (list x))))]
    (receive
      ;; We've reached the maxiumum number of messages
      ((tuple 'pass (cons x xs) msg) (when (== msg n-msgs))
       ;; State our intentions.
       (: io format '"Shutting Down.~n" '())
       ;; Ensure we trigger the shut down of all remaining rings.
       (! x (tuple 'pass (ring-col x xs) msg)))
      ;; We've received a message, pass it on to the next ring.
      ((tuple 'pass (cons x xs) msg)
       (: io format '"Recieved Message~n" '())
       (! x (tuple 'pass (ring-col x xs) (+ msg 1)))
       ;; Make sure we're still around to receive the next one.
       (ring n-msgs)))))


(defun contact_stars
  ;; This function sends a message to each of the individual stars in turn
  ;; until the list is exhausted.
  ([()] 'done) ;; No more stars, we're done here.
  ([(cons x xs)]
   (: io format '"Sent message to star~n" '())
   (! x (tuple 'msg (self)))
   (receive ;; Wait until the star replies before moving on to the next one.
     ;; Recur into the rest of the list.
     ('ok (contact_stars xs)))))

(defun start_star ;; Start our star communication process
  ((n-stars n-msgs) (when (is_number n-stars) (is_number n-msgs))
   ;; Use the function from earlier to create our list of pids
   (let* [(stars (create-pids-one-arg 'star n-stars (: lists seq 1 n-stars)))
          ;; For every message, trigger a sequence of communication with every
          ;; star. This is inside the let* so I can deliberately discard the
          ;; value and not be yelled at by the compiler.
          (_ (lc ((<- _ (: lists seq 1 n-msgs))) (contact_stars stars)))]
     ;; Ensure all the star processes are killed off.
     (lc ((<- star stars)) (! star 'die)))))

(defun star (x) ;; Our star receiver function
  (receive
    ((tuple 'msg from) ;; Received a message from the core.
     (: io format '"Received msg from center~n" '())
     (! from 'ok)
     (star x))
    ('die 'ok))) ;; Received instruction to die from the core.
