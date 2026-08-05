;; Copyright (c) 2026 Duncan McGreggor
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

;; File    : lfe_init_SUITE.lfe
;; Purpose : Verify lfe -eval across OTP versions.

(defmodule lfe_init_SUITE
  "Verify that lfe -eval works across OTP versions."
  (export
   (all 0) (suite 0) (groups 0)
   (init_per_suite 1) (end_per_suite 1)
   (eval_simple_expr 1)
   (eval_io_output 1)
   (eval_exit_status 1)
   (eval_multiple_exprs 1)))

(defun all ()
  '(eval_simple_expr eval_io_output eval_exit_status eval_multiple_exprs))

(defun suite ()
  `(#(timetrap #(seconds 30))))

(defun groups () '())

(defun init_per_suite (config)
  (let* ((root (lfe-root-dir))
         (lfe-bin (filename:join (list root "bin" "lfe")))
         (erl-libs (lfe-erl-libs)))
    (case (filelib:is_file lfe-bin)
      ('true
       (lists:append
        (list (tuple 'lfe_bin lfe-bin) (tuple 'erl_libs erl-libs))
        config))
      ('false
       (tuple 'skip
              (lists:flatten
               (io_lib:format "lfe binary not found at ~s" (list lfe-bin))))))))

(defun end_per_suite (_config) 'ok)

;; Evaluate a simple arithmetic expression, verify output.
(defun eval_simple_expr (config)
  (let ((`#(0 #"3") (run-lfe-eval config
                      "(io:format \"~w\" (list (+ 1 2))) (halt 0)")))
    'ok))

;; Verify that io:format output is captured correctly.
(defun eval_io_output (config)
  (let ((`#(0 #"hello world") (run-lfe-eval config
                                "(io:format \"hello world\" ()) (halt 0)")))
    'ok))

;; Verify that a non-zero exit status propagates.
(defun eval_exit_status (config)
  (let ((`#(42 ,_) (run-lfe-eval config "(halt 42)")))
    'ok))

;; Verify that multiple -eval expressions are evaluated in order.
(defun eval_multiple_exprs (config)
  (let ((`#(0 #"ab") (run-lfe-eval config
                       "(io:format \"a\" ()) (io:format \"b\" ()) (halt 0)")))
    'ok))

;;; --- helpers ---

(defun run-lfe-eval (config expr)
  (let* ((lfe-bin (proplists:get_value 'lfe_bin config))
         (erl-libs (proplists:get_value 'erl_libs config))
         (port (open_port
                (tuple 'spawn_executable lfe-bin)
                (list (tuple 'args (list "-eval" expr))
                      (tuple 'env (list (tuple "ERL_LIBS" erl-libs)))
                      'exit_status 'stderr_to_stdout 'binary 'hide))))
    (collect-port port #"")))

(defun collect-port (port acc)
  (receive
    (`#(,port #(data ,data))
     (collect-port port (binary (acc binary) (data binary))))
    (`#(,port #(exit_status ,status))
     (tuple status (string:trim acc)))
    (after 15000
      (catch (port_close port))
      (ct:fail (tuple 'timeout acc)))))

;; This test must spawn bin/lfe as an external process, because the
;; bug is specifically about how lfe_init:start/0 is invoked via
;; -user lfe_init with -noshell, which is what bin/lfe sets up. We
;; can't reproduce that calling convention from inside an
;; already-running BEAM.
(defun lfe-root-dir ()
  (let* ((test-lib-dir (filename:absname (code:lib_dir 'lfe))) ; rebar3 test lib path
         (dirs-to-root (lists:duplicate 4 "..")))              ; _build/<profile>/lib/lfe -> root
    (filename:absname (filename:join (cons test-lib-dir dirs-to-root)))))

;; ERL_LIBS for the spawned bin/lfe must point at the OTP library
;; directory that actually holds the compiled lfe application, i.e. the
;; parent of code:lib_dir('lfe') (_build/<profile>/lib). Pointing it at
;; the repo root instead only works when a matching profile happens to
;; be pre-built (as CI does via `rebar3 compile`), so `rebar3 ct` alone
;; would fail to find lfe_init. Deriving it from the running app makes
;; the suite independent of which rebar3 profile is built.
(defun lfe-erl-libs ()
  (filename:dirname (filename:absname (code:lib_dir 'lfe))))
