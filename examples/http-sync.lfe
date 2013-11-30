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

;; File    : http-sync.lfe
;; Author  : Duncan McGreggor
;; Purpose : Demonstrate the use of the synchronous Erlang HTTP client in LFE

;; Here is some example usage from a Bash command line:
;;
;; $ time erl -pa ./ebin -s lfe_comp file examples/http-sync.lfe \
;;    -s http-sync get-pages \
;;    -url http://lfe.github.io/ \
;;    -url http://google.com/ \
;;    -url http://yahoo.com/ \
;;    -s erlang halt -noshell
;; ...
;; real  0m5.313s
;; user  0m2.172s
;; sys   0m0.137s

;; Here is some example usage from the REPL:
;;
;; > (slurp '"examples/http-sync.lfe")
;; #(ok http-sync)
;; > (get-pages (list '"http://lfe.github.io/"))
;; Result: {{"HTTP/1.1",200,"OK"},
;;       [{"cache-control","max-age=600"},
;;        {"connection","keep-alive"},
;;        ...
;; ok
;; >
;;
;; The get-pages function starts the inets service for you. If you would like
;; to call get-page directly, you'll have to start that yourself:
;;
;; > (: inets start)
;; > (get-page '"http://lfe.github.io/")
;; Result: {{"HTTP/1.1",200,"OK"},
;;       [{"cache-control","max-age=600"},
;;        {"connection","keep-alive"},
;;        ...
;; ok
;; >

(defmodule http-sync
  (export all))

(defun parse-args (flag)
  "Given one or more command-line arguments, extract the passed values.

  For example, if the following was passed via the command line:

    $ erl -my-flag my-value-1 -my-flag my-value-2

  One could then extract it in an LFE program by calling this function:

    (let ((args (parse-args 'my-flag)))
      ...
      )
  In this example, the value assigned to the arg variable would be a list
  containing the values my-value-1 and my-value-2."
  (let (((tuple 'ok data) (: init get_argument flag)))
    (: lists merge data)))

(defun get-pages ()
  "With no argument, assume 'url parameter was passed via command line."
  (get-pages
    (parse-args 'url)))

(defun get-pages (urls)
  "Start inets and make (potentially many) HTTP requests."
  (: inets start)
  (: lists map
    (lambda (x)
      (get-page x)) urls))

(defun get-page (url)
  "Make a single HTTP request."
  (case (: httpc request url)
    ((tuple 'ok result)
      (: io format '"Result: ~p~n" (list result)))
    ((tuple 'error reason)
      (: io format '"Error: ~p~n" (list reason)))))
