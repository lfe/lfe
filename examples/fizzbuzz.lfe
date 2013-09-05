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
(defmodule fizzbuzz
  (export (buzz 1)
          (buzz1 1)
          (buzz2 1)))

(defun get-fizz (n)
  ;; Request a FizzBuzz result for a given number.
  (fizz n (rem n 5) (rem n 3)))

(defun fizz
  ;; Do the grunt work of the FizzBuzz solution
  ;; using pattern matching to simply return the
  ;; result that we want instead of getting tied
  ;; in knots with conditionals.
  ((_ 0 0) '"FizzBuzz")
  ((_ 0 _) '"Fizz")
  ((_ _ 0) '"Buzz")
  ((n _ _) n))

(defun buzz (n)
  ;; This is the basic version, takes an argument
  ;; and attempts to create result list of results.
  (: lists map
    ;; Wrap our call to 'get-fizz in a lambda
    (lambda (x) (get-fizz x))
    ;; Create a list of numbers from one to n
    (: lists seq 1 n)))

(defun buzz1
  ;; This version utilises pattern matching and guard to
  ;; protect our implementation from trying to work with
  ;; unwanted arguments.
  ;; Only run the FizzBuzz solution when we have a positive
  ;; whole number that is greater than zero
  ((n) (when (and (: erlang is_integer n)
                  (> n 0)))
   ;; woo!
   (buzz n))
  ((_)
   ;; Otherwise return a nice error message.
   ;; (: erlang error (tuple 'error '"Buzz1/1 only accepts whole numbers > 0"))
   (tuple 'error '"Buzz/1 only accepts whole numbers > 0")))

(defun start-buzz2
  ;; Completion pattern, we've emptied the provided list
  ((acc [])
   acc)
  ;; Worker pattern, we have results to work through!
  ((acc (cons x xs))
   ;; Very curious results in the repl from this particular one :\
   (start-buzz2 (cons acc (get-fizz x)) xs)))

(defun buzz2 ((col) (when (: erlang is_list col))
  ;;An alternative implementation using destructuring and
  ;;recursion to operate on a given list instead of assuming
  ;;its structure.
  (start-buzz2 [] col)))