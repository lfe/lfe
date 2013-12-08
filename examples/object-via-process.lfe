;; Copyright (c) 2013 Duncan McGreggor <oubiwann@cogitat.io>
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
;;  $ cd examples
;;  $ ../bin/lfe -pa ../ebin
;;
;; Load the file and create a fish-class instance:
;;
;; > (slurp '"object-via-process.lfe")
;; #(ok object-via-process)
;; > (set mommy-fish (fish-class '"Carp"))
;; <0.33.0>
;;
;; Execute some of the basic methods:
;;
;; > (get-species mommy-fish)
;; "Carp"
;; > (move mommy-fish 17)
;; The Carp swam 17 feet!
;; ok
;; > (get-id mommy-fish)
;; "47eebe91a648f042fc3fb278df663de5"
;;
;; Now let's look at modifying state data (e.g., children counts):
;;
;; > (get-children mommy-fish)
;; ()
;; > (get-children-count mommy-fish)
;; 0
;; > (set baby-fish-1 (reproduce mommy-fish))
;; <0.34.0>
;; > (get-id mommy-fish)
;; "47eebe91a648f042fc3fb278df663de5"
;; > (get-id baby-fish-1)
;; "fdcf35983bb496650e558a82e34c9935"
;; > (get-children-count mommy-fish)
;; 1
;; > (set baby-fish-2 (reproduce mommy-fish))
;; <0.35.0>
;; > (get-id mommy-fish)
;; "47eebe91a648f042fc3fb278df663de5"
;; > (get-id baby-fish-2)
;; "3e64e5c20fb742dd88dac1032749c2fd"
;; > (get-children-count mommy-fish)
;; 2
;; > (get-info mommy-fish)
;; id: 47eebe91a648f042fc3fb278df663de5
;; species: Carp
;; children: ["fdcf35983bb496650e558a82e34c9935",
;;            "3e64e5c20fb742dd88dac1032749c2fd"]
;; ok

(defmodule object-via-process
 (export all))

(defun fish-class (species)
  "This is the constructor that will be used most often, only requiring that
  one pass a 'species' string.

  When the children are not defined, simply use an empty list."
  (fish-class species ()))

(defun fish-class (species children)
  "This constructor is useful for two reasons:
    1) as a way of abstracting out the id generation from the
       larger constructor, and
    2) spawning the 'object loop' code (fish-class/3)."
  (let* (((binary (id (size 128))) (: crypto rand_bytes 16))
         (formatted-id (car
                         (: io_lib format
                           '"~32.16.0b" (list id)))))
    (spawn 'object-via-process
           'fish-class
           (list species children formatted-id))))

(defun fish-class (species children id)
  "This function is intended to be spawned as a separate process which is
  used to track the state of a fish. In particular, fish-class/2 spawns
  this function (which acts as a loop, pattern matching for messages)."
  (let ((move-verb '"swam"))
    (receive
      ((tuple caller 'move distance)
        (! caller (list species move-verb distance))
        (fish-class species children id))
      ((tuple caller 'species)
        (! caller species)
        (fish-class species children id))
      ((tuple caller 'children)
        (! caller children)
        (fish-class species children id))
      ((tuple caller 'children-count)
        (! caller (length children))
        (fish-class species children id))
      ((tuple caller 'id)
        (! caller id)
        (fish-class species children id))
      ((tuple caller 'info)
        (! caller (list id species children))
        (fish-class species children id))
      ((tuple caller 'reproduce)
        (let* ((child (fish-class species))
               (child-id (get-id child))
               (children-ids (: lists append
                               (list children (list child-id)))))
        (! caller child)
        (fish-class species children-ids id))))))

(defun call-method (object method-name)
  "This is a generic function, used to call into the given object (class
  instance)."
  (! object (tuple (self) method-name))
  (receive
    (data data)))

(defun call-method (object method-name arg)
  "Same as above, but with an additional argument."
  (! object (tuple (self) method-name arg))
  (receive
    (data data)))

(defun get-id (object)
  "Define object methods."
  (call-method object 'id))

(defun get-species (object)
  (call-method object 'species))

(defun get-info (object)
  (let ((data (call-method object 'info)))
    (: io format '"id: ~s~nspecies: ~s~nchildren: ~p~n" data)))

(defun move (object distance)
  (let ((data (call-method object 'move distance)))
    (: io format '"The ~s ~s ~p feet!~n" data)))

(defun reproduce (object)
  (call-method object 'reproduce))

(defun get-children (object)
  (call-method object 'children))

(defun get-children-count (object)
  (call-method object 'children-count))
