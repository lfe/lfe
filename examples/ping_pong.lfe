;; Copyright (c) 2009-2020 Tim Dysinger tim <[<-on->]> dysinger.net

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;; To use the code below in LFE, do the following:
;;
;;  $ ./bin/lfe
;;
;; Compile this example and run it:
;;
;; lfe> (c "examples/ping_pong.lfe")
;; (#(module ping_pong))
;; lfe> (ping_pong:start_link)
;; #(ok #Pid<0.196.0>)
;; lfe> (ping_pong:ping)
;; #(pong 1)
;; lfe> (ping_pong:stop)
;; ok

(defmodule ping_pong
  (export 
    (start_link 0)
    (start 0)
    (stop 0)
    (ping 0))
  (export 
    (init 1) 
    (handle_call 3) 
    (handle_cast 2)
    (handle_info 2) 
    (terminate 2) 
    (code_change 3))
  (behaviour gen_server))        ; Just indicates intent

;; Management API

(defun start_link ()
  (gen_server:start_link
    #(local ping_pong) 'ping_pong '() '()))

(defun start ()
  (gen_server:start
    #(local ping_pong) 'ping_pong '() '()))

(defun stop ()
  (gen_server:stop 'ping_pong))

;; Client API

(defun ping ()
  (gen_server:call 'ping_pong 'ping))

;; Gen_server callbacks

(defrecord state 
  (pings 0))

(defun init (args)
  `#(ok ,(make-state pings 0)))

(defun handle_call (req from state)
  (let* ((new-count (+ (state-pings state) 1))
         (new-state (set-state-pings state new-count)))
    `#(reply #(pong ,new-count) ,new-state)))

(defun handle_cast (msg state)
  `#(noreply ,state))

(defun handle_info (info state)
  `#(noreply ,state))

(defun terminate (reason state)
  'ok)

(defun code_change (old-vers state extra)
  `#(ok ,state))
