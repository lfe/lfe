;; -*- Mode: LFE; -*-
;; Code from Paradigms of Artificial Intelligence Programming
;; Copyright (c) 1991 Peter Norvig, Common Lisp version
;; Copyright (c) 2008-2020 Robert Virding

;; File    : gps1.lisp
;; Author  : Peter Norvig, Robert Virding
;; Purpose : Demonstrate the General Problem Solver from PAIP in LFE.

;; This files was converted from the PAIP Common Lisp book source cdoe to LFE
;; by Robert Virding. This example is the first (naive) GPS implementation
;; given in the book.
;;
;; Here is some example usage for a successful run:
;;
;; $ ./bin/lfe
;; 
;; lfe> (slurp "examples/gps1.lfe")
;; #(ok gps1)
;; lfe> (gps '(son-at-home car-needs-battery have-money have-phone-book)
;;        '(son-at-school)
;;        (school-ops))
;; executing 'look-up-number'
;; executing 'telephone-shop'
;; executing 'tell-shop-problem'
;; executing 'give-shop-money'
;; executing 'shop-installs-battery'
;; executing 'drive-son-to-school'
;; solved
;; lfe>
;;
;; Here is an unsuccessful run:
;;
;; lfe> (gps '(son-at-home car-needs-battery have-money have-phone-book)
;;        '(son-at-school have-money)
;;        (school-ops))
;; executing 'look-up-number'
;; executing 'telephone-shop'
;; executing 'tell-shop-problem'
;; executing 'give-shop-money'
;; executing 'shop-installs-battery'
;; executing 'drive-son-to-school'
;; false
;; lfe>
;;
;; And a trivial run (for Saturdays!):
;;
;; lfe> (gps '(son-at-home) '(son-at-home) (school-ops))
;; solved
;;
(include-lib "lfe/include/scm.lfe")

;; Define macros for global variable access. This is a hack and very naughty!
(defsyntax defvar
  ([name val] (let ((v val)) (put 'name v) v)))

(defsyntax setvar
  ([name val] (let ((v val)) (put 'name v) v)))

(defsyntax getvar
  ([name] (get 'name)))

;; Module definition.

(module gps1)

(export ((gps 2) (gps 3) (school-ops 0)))

(import lists ((member 2) (all 2) (any 2)))

;; Rename lists functions to be more CL like.
(rename lists (((all 2) every) ((any 2) some) ((filter 2) find-all)))

;; An operation.
(record op (action preconds add-list del-list))

;; General Problem Solver: achieve all goals using *ops*.
(function gps
  (lambda (state goals ops)
    ;; Set global variables
    (defvar *state* state)    ;The current state: a list of conditions.
    (defvar *ops* ops)        ;A list of available operators.
    (if (every (fun achieve 1) goals) 'solved)))

(function gps
  (lambda (state goals)
    ;; Set global variables, but use existing *ops*
    (defvar *state* state)    ;The current state: a list of conditions.
    (if (every (fun achieve 1) goals) 'solved)))

;; A goal is achieved if it already holds or if there is an
;; appropriate op for it that is applicable."
(function achieve
  (lambda (goal)
    (orelse (member goal (getvar *state*))
            (some (fun apply-op 1)
                  (find-all (lambda (op) (appropriate-p goal op))
                            (getvar *ops*))))))

;; An op is appropriate to a goal if it is in its add list.
(function appropriate-p
  (lambda (goal op)
    (member goal (record-field op op add-list))))

;; Print a message and update *state* if op is applicable.
(function apply-op
  (lambda (op)
    (if (every (fun achieve 1) (record-field op op preconds))
      (progn
        (io:fwrite "executing ~p\n" (list (record-field op op action)))
        (setvar *state* (set-difference (getvar *state*)
                                        (record-field op op del-list)))
        (setvar *state* (union (getvar *state*) (record-field op op add-list)))
        'true))))

;; Define the set functions to work on list, a listsets module really.
(function set-difference
  (match-lambda
    ([(cons e es) s2]
     (if (member e s2)
       (set-difference es s2)
       (cons e (set-difference es s2))))
    ([() s2] ())))

(function union
  (match-lambda
    ([(cons e es) s2]
     (if (member e s2)
       (union es s2)
       (cons e (union es s2))))
    ([() s2] ())))

;; Define a list of operations to use with GPS.
(function school-ops
  (lambda ()
    (list
     (record op action 'drive-son-to-school
             preconds '(son-at-home car-works)
             add-list '(son-at-school)
             del-list '(son-at-home))
     (record op action 'shop-installs-battery
             preconds '(car-needs-battery shop-knows-problem shop-has-money)
             add-list '(car-works)
             del-list ())
     (record op action 'tell-shop-problem
             preconds '(in-communication-with-shop)
             add-list '(shop-knows-problem)
             del-list ())
     (record op action 'telephone-shop
             preconds '(know-phone-number)
             add-list '(in-communication-with-shop)
             del-list ())
     (record op action 'look-up-number
             preconds '(have-phone-book)
             add-list '(know-phone-number)
             del-list ())
     (record op action 'give-shop-money
             preconds '(have-money)
             add-list '(shop-has-money)
             del-list '(have-money)))))
