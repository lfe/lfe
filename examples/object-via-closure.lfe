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
;; $ ./bin/lfe
;;
;; Load the file and create a fish-class instance:
;;
;; lfe> (slurp "examples/object-via-closure.lfe")
;; #(ok object-via-closure)
;; lfe> (set mommy-fish (fish-class "Carp"))
;; #Fun<lfe_eval.10.91765564>
;;
;; Execute some of the basic methods:
;;
;; lfe> (send mommy-fish 'species)
;; "Carp"
;; lfe> (send mommy-fish 'move 17)
;; The Carp swam 17 feet!
;; ok
;; lfe> (send mommy-fish 'id)
;; "c0ec94b9de24657c51ba180768542b27"
;;
;; Now let's look at "modifying" state data (e.g., children counts):
;;
;; lfe> (send mommy-fish 'children)
;; ()
;; lfe> (send mommy-fish 'children-count)
;; 0
;; lfe> (set `(,mommy-fish ,baby-fish-1) (send mommy-fish 'reproduce))
;; (#Fun<lfe_eval.10.91765564> #Fun<lfe_eval.10.91765564>)
;; lfe> (send mommy-fish 'id)
;; "c0ec94b9de24657c51ba180768542b27"
;; lfe> (send baby-fish-1 'id)
;; "5f31a47f000b5d173faa2793ea2ec876"
;; lfe> (send mommy-fish 'children-count)
;; 1
;; lfe> (set `(,mommy-fish ,baby-fish-2) (send mommy-fish 'reproduce))
;; (#Fun<lfe_eval.10.91765564> #Fun<lfe_eval.10.91765564>)
;; lfe> (send baby-fish-2 'id)
;; "2f40b14a4394f3b7a57d4e9048bbb19e"
;; lfe> (send mommy-fish 'children-count)
;; 2
;; lfe> (send mommy-fish 'info)
;; (#(id "c0ec94b9de24657c51ba180768542b27")
;;  #(species "Carp")
;;  #(children
;;    ("5f31a47f000b5d173faa2793ea2ec876" "2f40b14a4394f3b7a57d4e9048bbb19e")))
;; ok

(defmodule object-via-closure
  (export all))

(defun fish-class (species)
  "This is the constructor that will be used most often, only requiring that
  one pass a 'species' string.

  When the children are not defined, simply use an empty list."
  (fish-class species '()))

(defun fish-class (species children)
  "This contructor is mostly useful as a way of abstracting out the id
  generation from the larger constructor. Nothing else uses fish-class/2
  besides fish-class/1, so it's not strictly necessary.

  When the id isn't known, generate one."
  (fish-class species children (gen-id)))

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
            `(#(id ,(send self 'id))
              #(species ,(send self 'species))
              #(children ,(send self 'children)))))
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

(defun gen-id ()
  (let (((binary (id (size 128))) (crypto:strong_rand_bytes 16)))
    (io_lib:format "~32.16.0b" (list id))))

