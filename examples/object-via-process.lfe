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

;; File    : object-via-process.lfe
;; Author  : Duncan McGreggor
;; Purpose : Demonstrating simple OOP with lightweight Erlang processes

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
;;  $ ./bin/lfe -pa ./ebin
;;
;; Load the file and create a fish-class instance:
;;
;; > (slurp "examples/object-via-process.lfe")
;; #(ok object-via-process)
;; > (set mommy-fish (init-fish "Carp"))
;; <0.33.0>
;;
;; Execute some of the basic methods:
;;
;; > (send mommy-fish 'species)
;; "Carp"
;; > (send mommy-fish 'move 17)
;; "The Carp swam 17 feet!"
;; > (send mommy-fish 'id)
;; "47eebe91a648f042fc3fb278df663de5"
;;
;; Now let's look at modifying state data (e.g., children counts):
;;
;; > (send mommy-fish 'children)
;; ()
;; > (send mommy-fish 'children-count)
;; 0
;; > (set baby-fish-1 (send mommy-fish 'reproduce))
;; <0.34.0>
;; > (send baby-fish-1 'id)
;; "fdcf35983bb496650e558a82e34c9935"
;; > (send mommy-fish 'children-count)
;; 1
;; > (set baby-fish-2 (send mommy-fish 'reproduce))
;; <0.35.0>
;; > (send baby-fish-2 'id)
;; "3e64e5c20fb742dd88dac1032749c2fd"
;; > (send mommy-fish 'children-count)
;; 2
;; > (send mommy-fish 'info)
;; (#(id "f05064ffcf92d7b3e72968fd481abbd0")
;;  #(species "Carp")
;;  #(children
;;    ("d53a426c732c938f996a1c2520bb621f" "15fede691ab3f96e9e3df248d37b7b55")))

(defmodule object-via-process
 (export all))

(defun init-fish (species)
  "This is the constructor that will be used most often, only requiring that
  one pass a 'species' string.

  When the children are not defined, simply use an empty list."
  (init-fish species ()))

(defun init-fish (species children)
  "This constructor is useful for two reasons:
    1) as a way of abstracting out the id generation from the
       larger constructor, and
    2) spawning the 'object loop' code (fish-class/3)."
  (let* (((binary (id (size 128))) (crypto:rand_bytes 16))
         (formatted-id (car
                         (io_lib:format "~32.16.0b" `(,id)))))
    (spawn (lambda ()
             (fish-class species children formatted-id)))))

(defun fish-class (species children id)
  "This function is intended to be spawned as a separate process which is
  used to track the state of a fish. In particular, fish-class/2 spawns
  this function (which acts as a loop, pattern matching for messages)."
  (let ((move-verb "swam"))
    (receive
      (`#(,caller move ,distance)
       (! caller (lists:flatten
                  (io_lib:format "The ~s ~s ~p feet!"
                                 `(,species ,move-verb ,distance))))
        (fish-class species children id))
      (`#(,caller species ())
        (! caller species)
        (fish-class species children id))
      (`#(,caller children ())
        (! caller children)
        (fish-class species children id))
      (`#(,caller children-count ())
        (! caller (length children))
        (fish-class species children id))
      (`#(,caller id ())
        (! caller (lists:flatten id))
        (fish-class species children id))
      (`#(,caller info ())
       (! caller `(#(id ,id)
                   #(species ,species)
                   #(children ,children)))
        (fish-class species children id))
      (`#(,caller reproduce ())
        (let* ((child (init-fish species))
               (child-id (send child 'id))
               (children-ids (lists:append children `(,child-id))))
          (! caller child)
          (fish-class species children-ids id))))))

(defun send (object method-name)
  "This is a generic function, used to call into the given object (class
  instance)."
  (send object method-name '()))

(defun send (object method-name arg)
  "This is a generic function, used to call into the given object (class
  instance)."
  (! object `#(,(self) ,method-name ,arg))
  (receive
    (data data)))
