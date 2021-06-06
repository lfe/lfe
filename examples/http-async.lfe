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

;; File    : http-async.lfe
;; Author  : Duncan McGreggor
;; Purpose : Demonstrate the use of the asynchronous Erlang HTTP client in LFE

;; Note that this example depends upon the parallel-lists project which has
;; been updated on github here:
;;    https://github.com/oubiwann/plists
;;
;; To install to your Erlang system lib path:
;;
;; $ git clone https://github.com/oubiwann/plists.git
;; $ cd plists
;; $ make compile
;; $ sudo ERL_LIBS=`erl -eval 'io:fwrite(code:lib_dir()), halt().' -noshell` \
;;     make install

;; Below is some example usage from a Bash command line. To get a more or less
;; repeatable and comparable sense of performance vs. the sync version, we are
;; hitting a web server running on localhost that is serving a 1MB image at the
;; docroot. On my 2012 MacBook Pro, I had 21 -url options to the big file:
;;
;; $ time erl -pa ./ebin -s lfe_comp file examples/http-async.lfe \
;;    -s http-async get-pages \
;;    -url http://127.0.0.1/bigfile.jpg \
;;    -url http://127.0.0.1/bigfile.jpg \
;;    -url http://127.0.0.1/bigfile.jpg \
;;    -url http://127.0.0.1/bigfile.jpg \
;;    ...
;;    -url http://127.0.0.1/bigfile.jpg \
;;    -s erlang halt -noshell > /dev/null
;;
;; real  0m6.538s
;; user  0m6.499s
;; sys   0m0.206s
;;
;; This doesn't seem like too big of an improvement over the http-sync case,
;; which only took a three seconds longer, though one can see how it could add
;; up.
;;
;; Be sure to watch the web server's access.log file right after running that
;; command: you will see all the requests come in simultaneously, with no delay.
;;
;; Note that if you want to repeat the command above, after that initial
;; compile, you can change it to use the compile .beam file in your current
;; directory:
;;
;; $ time erl -pa ./ -s http-async get-pages \
;;    -url http://127.0.0.1/bigfile.jpg \
;;    ...

;; Here is some example usage from the REPL:
;;
;; $ ./bin/lfe
;;
;; lfe> (slurp "examples/http-async.lfe")
;; #(ok http-async)
;; lfe> (get-pages (list "http://lfe.io/"))
;; Result: {{"HTTP/1.1",200,"OK"},
;;       [{"cache-control","max-age=600"},
;;        {"connection","keep-alive"},
;;        ...
;; ok
;; lfe>
;;
;; The get-pages function starts the inets service for you. If you would like
;; to call get-page directly (without first having called get-pages), you'll
;; have to start that yourself:
;;
;; lfe> (inets:start)
;; lfe> (ssl:start)
;; lfe> (get-page "https://lfe.io/")
;; Result: {{"HTTP/1.1",200,"OK"},
;;       [{"cache-control","max-age=600"},
;;        {"connection","keep-alive"},
;;        ...
;; ok
;; lfe>

(defmodule http-async
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
  (let (((tuple 'ok data) (init:get_argument flag)))
    (lists:merge data)))

(defun get-pages ()
  "With no argument, assume 'url parameter was passed via command line."
  (let ((urls (parse-args 'url)))
    (get-pages urls)))

(defun get-pages (urls)
  "Start inets and make (potentially many) HTTP requests."
  (inets:start)
  (ssl:start)
  (plists:map
    (lambda (x)
      (get-page x)) urls))

(defun get-page (url)
  "Make a single HTTP request."
  (let* ((method 'get)
         (headers ())
         (request-data (tuple url headers))
         (http-options ())
         (request-options (list (tuple 'sync 'false))))
    (httpc:request method request-data http-options request-options)
    (receive
      ((tuple 'http (tuple request-id (tuple 'error reason)))
       (io:format "Error: ~p~n" (list reason)))
      ((tuple 'http (tuple request-id result))
       (io:format "Result: ~p~n" (list result))))))
