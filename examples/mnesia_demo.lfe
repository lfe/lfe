;; Copyright (c) 2011 Robert Virding. All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
;; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.

;; File    : mnesia_demo.lfe
;; Author  : Robert Virding
;; Purpose : A simple Mnesia demo file for LFE.

;; This file contains a simple demo of using LFE to access Mnesia
;; tables.  It shows how to use the emp-XXXX macro (ETS match pattern)
;; together with mnesia:match_object, match specifications with
;; mnesia:select and Query List Comprehensions.

(defmodule mnesia_demo
  (export (new 0) (by_place 1) (by_place_ms 1) (by_place_qlc 1)))

(defrecord person name place job)

(defun new ()
  ;; Start mnesia and create a table, we will get an in memory only schema.
  (: mnesia start)
  (: mnesia create_table 'person '(#(attributes (name place job))))
  ;; Initialise the table.
  (let ((people '(
		  ;; First some people in London.
		  #(fred london waiter)
		  #(bert london waiter)
		  #(john london painter)
		  #(paul london driver)
		  ;; Now some in Paris.
		  #(jean paris waiter)
		  #(gerard paris driver)
		  #(claude paris painter)
		  #(yves paris waiter)
		  ;; And some in Rome.
		  #(roberto rome waiter)
		  #(guiseppe rome driver)
		  #(paulo rome painter)
		  ;; And some in Berlin.
		  #(fritz berlin painter)
		  #(kurt berlin driver)
		  #(hans berlin waiter)
		  #(franz berlin waiter)
		  )))
    (: lists foreach (match-lambda
		       ([(tuple n p j)]
			(: mnesia transaction
			  (lambda ()
			    (let ((new (make-person name n place p job j)))
			      (: mnesia write new))))))
       people)))

;; Match records by place using match_object and the emp-XXXX macro.
(defun by_place (place)
  (: mnesia transaction
    (lambda () (: mnesia match_object (emp-person place place)))))

;; Use match specifications to match records
(defun by_place_ms (place)
  (let ((f (lambda () (: mnesia select 'person
			 (match-spec ([(match-person name n place p job j)]
				      (when (=:= p place))
				      (tuple n j)))))))
    (: mnesia transaction f)))

;; Use Query List Comprehensions to match records
(defun by_place_qlc (place)
  (let ((f (lambda ()
	     (let ((q (qlc (lc ((<- person (: mnesia table 'person))
				(=:= (person-place person) place))
			     person))))
	       (: qlc e q)))))
    (: mnesia transaction f)))

;; Ignore this
;; (qlc ((<- A (call 'qlc 'q (tuple 'qlc_lc (match-lambda (() (tuple 'simple_v1 'X (match-lambda (() (cons 1 (cons 2 (cons 3 ()))))) 42))) 'undefined)))) A)
