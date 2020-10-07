;; Copyright (c) 2008-2020 Sean Chalmers
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

;; File    : fizzbuzz.lfe
;; Author  : Sean Chalmers
;; Purpose : A simple FizzBuzz solution using LFE.

;; This is a very simplistic (albiet not terribly extensible) solution
;; to the FizzBuzz problem of counting through a sequence of numbers
;; and replacing certain numbers with words when specific conditions are met.
;;
;; There are no doubt more interesting or extensible ways of solving FizzBuzz,
;; however I felt this was a good example of using both Pattern Matching and
;; Higher Order Functions (lists:map/2) in a very simplistic way.

;; Here is some example usage:
;; 
;; $ ./bin/lfe
;;
;; lfe> (c "examples/fizzbuzz.lfe")
;; (#(module fizzbuzz))
;; lfe> (fizzbuzz:buzz 1) 
;; (1)
;; lfe> (fizzbuzz:buzz 2) 
;; (1 2)
;; lfe> (fizzbuzz:buzz 5)
;; (1 2 "Buzz" 4 "Fizz")
;; lfe> (fizzbuzz:buzz 10)
;; (1 2 "Buzz" 4 "Fizz" "Buzz" 7 8 "Buzz" "Fizz")

(defmodule fizzbuzz
  (export 
    (buzz 1)
    (buzz1 1)
    (buzz2 1)
    (buzz3 1)))

(defun get-fizz (n)
  ;; Request a FizzBuzz result for a given number.
  (fizz n (rem n 5) (rem n 3)))

(defun fizz
  ;; Do the grunt work of the FizzBuzz solution
  ;; using pattern matching to simply return the
  ;; result that we want instead of getting tied
  ;; in knots with conditionals.
  ([_ 0 0] '"FizzBuzz")
  ([_ 0 _] '"Fizz")
  ([_ _ 0] '"Buzz")
  ([n _ _] n))

(defun buzz (n)
  ;; This is the basic version, takes an argument
  ;; and attempts to create result list of results.
  (lists:map
    ;; Wrap our call to 'get-fizz in a lambda
    (lambda (x) (get-fizz x))
    ;; Create a list of numbers from one to n
    (lists:seq 1 n)))

(defun buzz1
  ;; This version utilises pattern matching and guard to
  ;; protect our implementation from trying to work with
  ;; unwanted arguments.
  ;; Only run the FizzBuzz solution when we have a positive
  ;; whole number that is greater than zero
  ([n] (when (and (erlang:is_integer n)
                  (> n 0)))
   ;; woo!
   (buzz n))
  ([_]
   ;; Otherwise return a nice error message. There are many different
   ;; ways you can return an error in erlang and it depends entirely on
   ;; what you're trying to achieve. Depending on your requirements,
   ;; sending back an 'error atom is sufficient.
   ;;
   ;; You can also provide an error tuple. With the 'error atom and a reason.
   ;;
   ;; -> (tuple 'error '"Buzz/1 only accepts whole numbers > 0")
   ;;
   ;; Or if necessary you can trigger an Erlang error:
   ;;
   ;; -> (: erlang error (tuple 'error '"Buzz1/1 only accepts whole numbers > 0"))
   ;;
   ;; In this example it is enough we return the error atom.
   'error))

(defun buzz2
  ;; A concise recursive implementation without a guard
  ;; the guard or the use of the accumulator.
  ([(cons x xs)]
   ;; When using the 'cons', the list _must_ be the second argument.
   ;; You cannot append to the end of a list.
   (cons (get-fizz x) (buzz3 xs)))
  ;; Pattern match to an empty list as our termination condition.
  ([()] ()))

(defun buzz3 (col)
  ;; A tail-recursive implementation of FizzBuzz using a common pattern within
  ;; Erlang. Supplying an accmulator to keep our recursion in a constant space.
  ;; The problem with this however is that the list we create using this method
  ;; will be reversed, as Erlang only allows to append to the head of a list.
  (tail-buzz [] col))

(defun tail-buzz
  ;; This is our base case, we've exhausted the list that was provided as input
  ;; and we have our completed list of FizzBuzz results. Because we've been
  ;; building our list by prepending our new result at each step our list in
  ;; reverse order and must be reversed. Unless you want a list in reverse
  ;; order in which case you can happily skip it. :)
  ([acc []]
   ;; Use the built in lists:reverse/1 function as it is is a much loved
   ;; component of Erlang and due to Erlang favouring this form of tail recursive
   ;; functions it has been highly optimised for the task. There is a great
   ;; explanation and discussion of this topic in Learn You Some Erlang. I
   ;; recommend you check it out.
   (: lists reverse acc))
  ;; Similar to our other recursive implementation we split the list we receive
  ;; into a Head component (x) and a Tail component (xs). In Erlang speak [H|T].
  ;; Note the lack of square parenthesis, as we have two function arguments we
  ;; need to wrap them both in a list and simply a call to 'cons' is enough to
  ;; type check that we have received a list as our second argument.
  ([acc (cons x xs)]
   ;; Place the result of our FizzBuzz at the start of the list and dive into
   ;; the breach once more!
   (tail-buzz (cons (get-fizz x) acc) xs)))
