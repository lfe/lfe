;; Copyright (c) 2013-2020 Duncan McGreggor <oubiwann@cogitat.io>
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;    http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; File    : messenger-back.lfe
;; Author  : Duncan McGreggor
;; Purpose : Demonstrating bidirectional message passing between the shell and
;;       a spawned process.

;; This file contains a simple demo for passing messages to an Erlang process
;; in LFE and the sending a message back to the calling process. To use, do the
;; following:
;;
;; $ ./bin/lfe
;;
;; lfe> (c "examples/messenger-back")
;; #(module messenger-back)
;; lfe> (messenger-back:send-message (self) "Well, I was able to extend the original entry a bit, yes.")
;; #(<0.25.0> "Well, I was able to extend the original entry a bit, yes.")
;; Received message: 'Well, I was able to extend the original entry a bit, yes.'
;; lfe> Sending message to process <0.25.0> ...
;; (messenger-back:send-message (self) "And what does it say now?")
;; #(<0.25.0> "And what does it say now?")
;; Received message: 'And what does it say now?'
;; lfe> Sending message to process <0.25.0> ...
;; (messenger-back:send-message (self) "Mostly harmless.")
;; #(<0.25.0> "Mostly harmless.")
;; Received message: 'Mostly harmless.'
;; lfe> Sending message to process <0.25.0> ...

;; With the messages sent and then resent back to the process whose ID was
;; presented, we can check to see that the calling process received the
;; information, as planned:
;;
;; lfe> (c:flush)
;; Shell got {"Well, I was able to extend the original entry a bit, yes."}
;; Shell got {"And what does it say now?"}
;; Shell got {"Mostly harmless."}
;; lfe>
;;
(defmodule messenger-back
  (export 
    (print-result 0) 
    (send-message 2)))

(defun print-result ()
  (receive
    ((tuple pid msg)
      (io:format "Received message: '~s'~n" (list msg))
      (io:format "Sending message to process ~p ...~n" (list pid))
      (! pid (tuple msg))
      (print-result))))

(defun send-message (calling-pid msg)
  (let ((spawned-pid (spawn 'messenger-back 'print-result ())))
    (! spawned-pid (tuple calling-pid msg))))
