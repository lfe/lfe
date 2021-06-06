;; Copyright (c) 2008-2020 Robert Virding
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

;; File    : ets_demo.lfe
;; Author  : Robert Virding
;; Purpose : A simple ETS demo file for LFE.

;; This file contains a simple demo of using LFE to access ETS tables.
;; It shows how to use the emp-XXXX macro (ETS match pattern) together
;; with ets:match/match_object and match specifications with
;; ets:select.

;; Here is some example usage:
;; 
;; $ ./bin/lfe
;;
;; lfe> (c "examples/ets_demo.lfe")
;; (#(module ets_demo))
;; lfe> (set db (ets_demo:new))
;; #Ref<0.2772763705.1333133315.72774>
;; lfe> (ets_demo:by_place db 'london)
;; #(((paul driver) (fred waiter) (john painter) (bert waiter))
;;   (#(person paul london driver)
;;    #(person fred london waiter)
;;    #(person john london painter)
;;    #(person bert london waiter)))

(defmodule ets_demo
  (export 
    (new 0) 
    (by_place 2) 
    (by_place_ms 2) 
    (not_painter 2)))

;; Define a simple person record to work on.
(defrecord person name place job)

;; Create an initialse the ets table.
(defun new ()
  (let ((db (ets:new 'ets_demo '(#(keypos 2) set))))
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
            #(franz berlin waiter))))
      (lists:foreach 
        (match-lambda
          ([(tuple n p j)]
           (ets:insert db (make-person name n place p job j))))
     people))
    db))                ;; Return the table

;; Match records by place using match, match_object and the emp-XXXX macro.
(defun by_place (db place)
  (let ((s1 (ets:match db (emp-person name '$1 place place job '$2)))
        (s2 (ets:match_object db (emp-person place place))))
    (tuple s1 s2)))

;; Use match specifications to match records
(defun by_place_ms (db place)
  (ets:select db (match-spec ([(match-person name n place p job j)]
                              (when (=:= place p))
                              (list 'p n j)))))

(defun not_painter (db place)
  (ets:select db (match-spec ([(match-person name n place p job j)]
                              (when (=:= place p) (=/= j 'painter))
                              (list 'p n j)))))
