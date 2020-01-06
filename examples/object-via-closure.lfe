;; Copyright (c) 2013 Duncan McGreggor <oubiwann@gmail.com>
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

;; File    : object-via-closure.lfe
;; Author  : Duncan McGreggor
;; Purpose : Demonstrating simple OOP with closures

;; The simple object system demonstrated below shows how to do the following:
;;  * create objects
;;  * call methods on those objects
;;  * have methods which can call other methods
;;  * update the state of an instance variable
;;
;; Note, however, that his example does not demonstrate inheritance.
;;
;; To use the code below in LFE, do the following:
;;
;;  $ .bin/lfe -pa .ebin
;;
;; Load the file and create a fish-class instance:
;;
;; > (slurp "examples/object-via-closure.lfe")
;; #(ok object-via-closure)
;; > (set mommy-fish (fish-class "Carp"))
;; #Fun<lfe_eval.10.91765564>
;;
;; Execute some of the basic methods:
;;
;; > (send mommy-fish 'species)
;; "Carp"
;; > (send mommy-fish 'move 17)
;; The Carp swam 17 feet!
;; ok
;; > (send mommy-fish 'id)
;; "47eebe91a648f042fc3fb278df663de5"
;;
;; Now let's look at "modifying" state data (e.g., children counts):
;;
;; > (send mommy-fish 'children)
;; ()
;; > (send mommy-fish 'children-count)
;; 0
;; > (set `(,mommy-fish ,baby-fish-1) (send mommy-fish 'reproduce))
;; (#Fun<lfe_eval.10.91765564> #Fun<lfe_eval.10.91765564>)
;; > (send mommy-fish 'id)
;; "47eebe91a648f042fc3fb278df663de5"
;; > (send baby-fish-1 'id)
;; "fdcf35983bb496650e558a82e34c9935"
;; > (send mommy-fish 'children-count)
;; 1
;; > (set `(,mommy-fish ,baby-fish-2) (send mommy-fish 'reproduce))
;; (#Fun<lfe_eval.10.91765564> #Fun<lfe_eval.10.91765564>)
;; > (send baby-fish-2 'id)
;; "3e64e5c20fb742dd88dac1032749c2fd"
;; > (send mommy-fish 'children-count)
;; 2
;; > (send mommy-fish 'info)
;; id: "47eebe91a648f042fc3fb278df663de5"
;; species: "Carp"
;; children: ["fdcf35983bb496650e558a82e34c9935",
;;            "3e64e5c20fb742dd88dac1032749c2fd"]
;; ok

(defmodule object-via-closure
 (export all))

(defun fish-class (species)
  "This is the constructor that will be used most often, only requiring that
  one pass a 'species' string.

  When the children are not defined, simply use an empty list."
  (fish-class species ()))

(defun fish-class (species children)
  "This contructor is mostly useful as a way of abstracting out the id
  generation from the larger constructor. Nothing else uses fish-class/2
  besides fish-class/1, so it's not strictly necessary.

  When the id isn't known, generate one."
  (let* (((binary (id (size 128))) (crypto:rand_bytes 16))
         (formatted-id (car
                         (io_lib:format "~32.16.0b" (list id)))))
    (fish-class species children formatted-id)))

(defun fish-class (species children id)
  "This is the constructor used internally, once the children and fish id are
  known."
  (let ((move-verb "swam"))
    (lambda (method-name)
      (case method-name
        ('id
          (lambda (self) id))
        ('species
          (lambda (self) species))
        ('children
          (lambda (self) children))
        ('info
          (lambda (self)
            (io:format "id: ~p~nspecies: ~p~nchildren: ~p~n"
                       `(,(send self 'id)
                         ,(send self 'species)
                         ,(send self 'children)))))
        ('move
          (lambda (self distance)
            (io:format "The ~s ~s ~p feet!~n"
                       `(,species ,move-verb ,distance))))
        ('reproduce
          (lambda (self)
            (let* ((child (fish-class species))
                   (child-id (send child 'id))
                   (children-ids (lists:append children `(,child-id)))
                   (parent-id (send self 'id))
                   (parent (fish-class species children-ids parent-id)))
              `(,parent ,child))))
        ('children-count
          (lambda (self)
            (length children)))))))

(defun send (object method-name)
  "This is a generic function, used to call into the given object (class
  instance)."
  (funcall (funcall object method-name) object))

(defun send (object method-name arg)
  "This is a generic function, used to call into the given object (class
  instance)."
  (funcall (funcall object method-name) object arg))