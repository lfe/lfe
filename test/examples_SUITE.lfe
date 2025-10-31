;; Copyright (c) 2025
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

;; File    : examples_SUITE.lfe
;; Purpose : Test suite for all examples in ./examples directory

(include-file "test_server.lfe")

(defmodule examples_SUITE
  "Test suite for LFE examples."
  (export
   (all 0)
   (church_compile 1)
   (church_functional 1)
   (core_macros_compile 1)
   (core_macros_functional 1)
   (ets_demo_compile 1)
   (ets_demo_functional 1)
   (fizzbuzz_compile 1)
   (fizzbuzz_functional 1)
   (gps1_compile 1)
   (gps1_functional 1)
   (guessing_game_compile 1)
   (guessing_game2_compile 1)
   (http_async_compile 1)
   (http_sync_compile 1)
   (internal_state_compile 1)
   (internal_state_functional 1)
   (joes_fav_compile 1)
   (joes_fav_functional 1)
   (lfe_eval_compile 1)
   (lfe_eval_functional 1)
   (messenger_back_compile 1)
   (messenger_back_functional 1)
   (messenger_compile 1)
   (messenger_functional 1)
   (mnesia_demo_compile 1)
   (mnesia_demo_functional 1)
   (object_via_closure_compile 1)
   (object_via_closure_functional 1)
   (object_via_process_compile 1)
   (object_via_process_functional 1)
   (ping_pong_compile 1)
   (ping_pong_functional 1)
   (ring_compile 1)
   (ring_functional 1)
   (sample_lfe_shellscript 1)
   (sample_lfescript 1)
   (simple_erl_exercises_compile 1)
   (simple_erl_exercises_functional 1)))

(defun all ()
  '(church_compile
    church_functional
    core_macros_compile
    core_macros_functional
    ets_demo_compile
    ets_demo_functional
    fizzbuzz_compile
    fizzbuzz_functional
    gps1_compile
    gps1_functional
    guessing_game_compile
    guessing_game2_compile
    http_async_compile
    http_sync_compile
    internal_state_compile
    internal_state_functional
    joes_fav_compile
    joes_fav_functional
    lfe_eval_compile
    lfe_eval_functional
    messenger_back_compile
    messenger_back_functional
    messenger_compile
    messenger_functional
    mnesia_demo_compile
    mnesia_demo_functional
    object_via_closure_compile
    object_via_closure_functional
    object_via_process_compile
    object_via_process_functional
    ping_pong_compile
    ping_pong_functional
    ring_compile
    ring_functional
    sample_lfe_shellscript
    sample_lfescript
    simple_erl_exercises_compile
    simple_erl_exercises_functional))

;; Helper function to get example file path
(defun example-path (filename)
  ;; CT runs from _build/test/logs/ct_run.../
  ;; We need to navigate up to project root
  (let* (((tuple 'ok cwd) (file:get_cwd))
         ;; Go up 4 levels from ct_run directory to get to project root
         (project-root (filename:join (list cwd ".." ".." ".." "..")))
         ;; Normalize the path
         (normalized-root (filename:absname project-root)))
    (filename:join (list normalized-root "examples" filename))))

;; Helper function for compiling example files
(defun compile-example (filename)
  (lfe_comp:file (example-path filename) '(return)))

;;; church.lfe tests
(defun church_compile (config)
  (line (test-pat `#(ok (#(ok church ,_)) ())
                  (compile-example "church.lfe"))))

(defun church_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "church.lfe")
  ;; Test church numerals conversion
  (line (=:= 0 (church:church->int1 (church:zero))))
  (line (=:= 1 (church:church->int1 (church:one))))
  (line (=:= 2 (church:church->int1 (church:two))))
  (line (=:= 3 (church:church->int1 (church:three))))
  (line (=:= 4 (church:church->int1 (church:four))))
  (line (=:= 5 (church:church->int1 (church:five))))
  ;; Test church->int2 with function references
  (line (=:= 5 (church:church->int2 #'church:five/0)))
  ;; Test get-church
  (line (=:= 10 (church:church->int1 (church:get-church 10))))
  (line (=:= 25 (church:church->int1 (church:get-church 25)))))

;;; core-macros.lfe tests
(defun core_macros_compile (config)
  ;; core-macros.lfe now defines a module with exported macros
  (line (test-pat `#(ok (#(ok core-macros ,_)) ())
                  (compile-example "core-macros.lfe"))))

(defun core_macros_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "core-macros.lfe")
  ;; Test the macros by expanding and evaluating expressions
  (let ((env (lfe_env:new)))
    ;; Test the caar macro - (car (car '((1 2) 3))) should be 1
    (let* ((expr1 '(core-macros:caar (quote ((1 2) 3))))
           ((tuple 'yes expanded1) (lfe_macro:expand_expr expr1 env))
           (result1 (lfe_eval:expr expanded1 env)))
      (line (=:= 1 result1)))
    ;; Test cadr macro - (car (cdr '(1 2 3))) should be 2
    (let* ((expr2 '(core-macros:cadr (quote (1 2 3))))
           ((tuple 'yes expanded2) (lfe_macro:expand_expr expr2 env))
           (result2 (lfe_eval:expr expanded2 env)))
      (line (=:= 2 result2)))
    ;; Test cdar macro - (cdr (car '((1 2 3) 4))) should be (2 3)
    (let* ((expr3 '(core-macros:cdar (quote ((1 2 3) 4))))
           ((tuple 'yes expanded3) (lfe_macro:expand_expr expr3 env))
           (result3 (lfe_eval:expr expanded3 env)))
      (line (=:= '(2 3) result3)))
    ;; Test cddr macro - (cdr (cdr '(1 2 3 4))) should be (3 4)
    (let* ((expr4 '(core-macros:cddr (quote (1 2 3 4))))
           ((tuple 'yes expanded4) (lfe_macro:expand_expr expr4 env))
           (result4 (lfe_eval:expr expanded4 env)))
      (line (=:= '(3 4) result4)))
    ;; Test list* macro - (list* 1 2 '(3 4)) should be (1 2 3 4)
    (let* ((expr5 '(core-macros:list* 1 2 (quote (3 4))))
           ((tuple 'yes expanded5) (lfe_macro:expand_expr expr5 env))
           (result5 (lfe_eval:expr expanded5 env)))
      (line (=:= '(1 2 3 4) result5)))
    ;; Test andalso macro
    (let* ((expr6 '(core-macros:andalso (quote true) (quote true)))
           ((tuple 'yes expanded6) (lfe_macro:expand_expr expr6 env))
           (result6 (lfe_eval:expr expanded6 env)))
      (line (=:= 'true result6)))
    (let* ((expr7 '(core-macros:andalso (quote true) (quote false)))
           ((tuple 'yes expanded7) (lfe_macro:expand_expr expr7 env))
           (result7 (lfe_eval:expr expanded7 env)))
      (line (=:= 'false result7)))
    ;; Test orelse macro
    (let* ((expr8 '(core-macros:orelse (quote false) (quote true)))
           ((tuple 'yes expanded8) (lfe_macro:expand_expr expr8 env))
           (result8 (lfe_eval:expr expanded8 env)))
      (line (=:= 'true result8)))
    (let* ((expr9 '(core-macros:orelse (quote false) (quote false)))
           ((tuple 'yes expanded9) (lfe_macro:expand_expr expr9 env))
           (result9 (lfe_eval:expr expanded9 env)))
      (line (=:= 'false result9)))))

;;; ets_demo.lfe tests
(defun ets_demo_compile (config)
  (line (test-pat `#(ok (#(ok ets-demo ,_)) ())
                  (compile-example "ets-demo.lfe"))))

(defun ets_demo_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "ets-demo.lfe")
  ;; Test ETS table creation and operations
  (let ((db (ets-demo:new)))
    ;; Test by_place function
    (let (((tuple matches objects) (ets-demo:by_place db 'london)))
      ;; Verify we got 4 people in London
      (line (=:= 4 (length matches)))
      (line (=:= 4 (length objects))))
    ;; Test by_place_ms function
    (let ((results (ets-demo:by_place_ms db 'paris)))
      ;; Should have people in Paris
      (line (=:= 'true (> (length results) 0))))
    ;; Test not_painter function
    (let ((results (ets-demo:not_painter db 'rome)))
      ;; Should have non-painters in Rome
      (line (=:= 'true (> (length results) 0))))
    ;; Clean up
    (ets:delete db)))

;;; fizzbuzz.lfe tests
(defun fizzbuzz_compile (config)
  (line (test-pat `#(ok (#(ok fizzbuzz ,_)) ())
                  (compile-example "fizzbuzz.lfe"))))

(defun fizzbuzz_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "fizzbuzz.lfe")
  ;; Test basic fizzbuzz functionality
  (line (=:= '(1) (fizzbuzz:buzz 1)))
  (line (=:= '(1 2 "Buzz") (fizzbuzz:buzz 3)))
  (line (=:= '(1 2 "Buzz" 4 "Fizz") (fizzbuzz:buzz 5)))
  (line (=:= '(1 2 "Buzz" 4 "Fizz" "Buzz" 7 8 "Buzz" "Fizz")
             (fizzbuzz:buzz 10)))
  (line (=:= '(1 2 "Buzz" 4 "Fizz" "Buzz" 7 8 "Buzz" "Fizz" 11 "Buzz" 13 14 "FizzBuzz")
             (fizzbuzz:buzz 15)))
  ;; Test buzz1 with guards
  (line (=:= '(1 2 "Buzz") (fizzbuzz:buzz1 3)))
  (line (=:= 'error (fizzbuzz:buzz1 -1)))
  (line (=:= 'error (fizzbuzz:buzz1 0)))
  (line (=:= 'error (fizzbuzz:buzz1 "not-a-number"))))

;;; gps1.lfe tests
(defun gps1_compile (config)
  ;; Accept any warnings
  (line (test-pat `#(ok (#(ok gps1 ,_)) ,_)
                  (compile-example "gps1.lfe"))))

(defun gps1_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "gps1.lfe")
  ;; Test successful GPS run
  (line (=:= 'solved
             (gps1:gps '(son-at-home car-needs-battery have-money have-phone-book)
                       '(son-at-school)
                       (gps1:school-ops))))
  ;; Test trivial case (goal already satisfied)
  (line (=:= 'solved
             (gps1:gps '(son-at-home)
                       '(son-at-home)
                       (gps1:school-ops)))))

;;; guessing-game.lfe tests
(defun guessing_game_compile (config)
  (line (test-pat `#(ok (#(ok guessing-game ,_)) ())
                  (compile-example "guessing-game.lfe"))))

;;; guessing-game2.lfe tests
(defun guessing_game2_compile (config)
  (line (test-pat `#(ok (#(ok guessing-game2 ,_)) ())
                  (compile-example "guessing-game2.lfe"))))

;;; http-async.lfe tests
(defun http_async_compile (config)
  (line (test-pat `#(ok (#(ok http-async ,_)) ())
                  (compile-example "http-async.lfe"))))

;;; http-sync.lfe tests
(defun http_sync_compile (config)
  (line (test-pat `#(ok (#(ok http-sync ,_)) ())
                  (compile-example "http-sync.lfe"))))

;;; internal-state.lfe tests
(defun internal_state_compile (config)
  (line (test-pat `#(ok (#(ok internal-state ,_)) ())
                  (compile-example "internal-state.lfe"))))

(defun internal_state_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "internal-state.lfe")
  ;; Test closure-based account implementation
  (let ((acct (internal-state:new-account "Alice" 100.0 0.06)))
    (line (=:= "Alice" (internal-state:send acct 'name)))
    (line (=:= 100.0 (internal-state:send acct 'balance)))
    ;; Test interest application
    (let ((acct2 (internal-state:send acct 'apply-interest)))
      (line (=:= 106.0 (internal-state:send acct2 'balance)))
      ;; Test withdraw
      (let ((acct3 (internal-state:send acct2 'withdraw 50.0)))
        (line (=:= 56.0 (internal-state:send acct3 'balance)))
        ;; Test deposit
        (let ((acct4 (internal-state:send acct3 'deposit 44.0)))
          (line (=:= 100.0 (internal-state:send acct4 'balance)))))))
  ;; Test process-based account implementation
  (let ((acct (internal-state:init-account "Bob" 200.0 0.05)))
    (line (=:= "Bob" (internal-state:snd acct 'name)))
    (line (=:= 200.0 (internal-state:snd acct 'balance)))
    (line (=:= 210.0 (internal-state:snd acct 'apply-interest)))
    (line (=:= 210.0 (internal-state:snd acct 'balance)))
    (line (=:= 310.0 (internal-state:snd acct 'deposit 100.0)))
    (line (=:= 210.0 (internal-state:snd acct 'withdraw 100.0)))))

;;; joes-fav.lfe tests
(defun joes_fav_compile (config)
  (line (test-pat `#(ok (#(ok joes-fav ,_)) ())
                  (compile-example "joes-fav.lfe"))))

(defun joes_fav_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "joes-fav.lfe")
  ;; Test factorial function
  (line (=:= 1 (joes-fav:factorial 0)))
  (line (=:= 1 (joes-fav:factorial 1)))
  (line (=:= 2 (joes-fav:factorial 2)))
  (line (=:= 6 (joes-fav:factorial 3)))
  (line (=:= 24 (joes-fav:factorial 4)))
  (line (=:= 120 (joes-fav:factorial 5)))
  (line (=:= 3628800 (joes-fav:factorial 10)))
  ;; Test universal server with factorial server
  (line (=:= 3628800 (joes-fav:run-it))))

;;; lfe_eval.lfe tests
(defun lfe_eval_compile (config)
  ;; lfe-eval compiles with warnings (unused functions, deprecated functions)
  ;; but should compile successfully
  (line (test-pat `#(ok (#(ok lfe-eval ,_)) ,_)
                  (compile-example "lfe-eval.lfe"))))

(defun lfe_eval_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "lfe-eval.lfe")
  ;; Test basic expression evaluation
  ;; Self-evaluating literals
  (line (=:= 42 (lfe-eval:expr 42)))
  (line (=:= 'atom (lfe-eval:expr '(quote atom))))
  (line (=:= '(1 2 3) (lfe-eval:expr '(quote (1 2 3)))))
  ;; Arithmetic operations using call form (module and function must be quoted)
  (line (=:= 5 (lfe-eval:expr '(call (quote erlang) (quote +) 2 3))))
  (line (=:= 6 (lfe-eval:expr '(call (quote erlang) (quote *) 2 3))))
  (line (=:= 10 (lfe-eval:expr '(call (quote erlang) (quote -) 15 5))))
  (line (=:= 3 (lfe-eval:expr '(call (quote erlang) (quote div) 9 3))))
  ;; List operations (built-in to the evaluator)
  (line (=:= '(1 2 3) (lfe-eval:expr '(list 1 2 3))))
  (line (=:= '(1 2 3 4) (lfe-eval:expr '(cons 1 (quote (2 3 4))))))
  (line (=:= 1 (lfe-eval:expr '(car (quote (1 2 3))))))
  (line (=:= '(2 3) (lfe-eval:expr '(cdr (quote (1 2 3))))))
  ;; Tuple operations
  (line (=:= #(a b c) (lfe-eval:expr '(tuple (quote a) (quote b) (quote c)))))
  ;; Comparisons using call form
  (line (=:= 'true (lfe-eval:expr '(call (quote erlang) (quote =:=) 5 5))))
  (line (=:= 'false (lfe-eval:expr '(call (quote erlang) (quote =:=) 5 6))))
  (line (=:= 'true (lfe-eval:expr '(call (quote erlang) (quote <) 3 5))))
  (line (=:= 'false (lfe-eval:expr '(call (quote erlang) (quote >) 3 5))))
  ;; Conditional evaluation (if)
  (line (=:= 'yes (lfe-eval:expr '(if (quote true) (quote yes) (quote no)))))
  (line (=:= 'no (lfe-eval:expr '(if (quote false) (quote yes) (quote no)))))
  ;; Let bindings
  (line (=:= 42 (lfe-eval:expr '(let ((x 42)) x))))
  (line (=:= 7 (lfe-eval:expr '(let ((x 3) (y 4)) (call (quote erlang) (quote +) x y)))))
  ;; Lambda and function application using funcall
  (line (=:= 5 (lfe-eval:expr '(funcall (lambda (x) (call (quote erlang) (quote +) x 2)) 3))))
  (line (=:= 12 (lfe-eval:expr '(funcall (lambda (x y) (call (quote erlang) (quote +) x y)) 5 7))))
  ;; Nested expressions
  (line (=:= 14 (lfe-eval:expr '(call (quote erlang) (quote +) (call (quote erlang) (quote *) 2 3) (call (quote erlang) (quote *) 4 2)))))
  ;; Progn - sequential evaluation
  (line (=:= 3 (lfe-eval:expr '(progn 1 2 3))))
  ;; Case expression
  (line (=:= 'two (lfe-eval:expr '(case 2 (1 (quote one)) (2 (quote two)) (_ (quote other))))))
  ;; Pattern matching with match/3
  (let ((env (lfe_env:new)))
    ;; Match a simple value
    (let ((result1 (lfe-eval:match 42 42 env)))
      (line (=/= 'no result1)))
    ;; Match a list pattern
    (let* (((tuple 'yes bindings) (lfe-eval:match '(cons x xs) '(1 2 3) env))
           ;; Add bindings to environment to create new environment
           (env-with-bindings (lfe_env:add_vbindings bindings env)))
      (line (=/= 'no (tuple 'yes bindings)))
      ;; Check that x was bound to 1 in the new environment
      (line (=:= 1 (lfe-eval:expr 'x env-with-bindings))))
    ;; Match should fail with incompatible patterns
    (line (=:= 'no (lfe-eval:match 42 43 env))))
  ;; Test body/1 - evaluate a sequence of expressions
  (line (=:= 42 (lfe-eval:body '(1 2 3 42))))
  ;; Test deprecated eval function with call form
  (line (=:= 10 (lfe-eval:eval '(call (quote erlang) (quote +) 5 5)))))

;;; messenger-back.lfe tests
(defun messenger_back_compile (config)
  (line (test-pat `#(ok (#(ok messenger-back ,_)) ())
                  (compile-example "messenger-back.lfe"))))

(defun messenger_back_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "messenger-back.lfe")
  ;; Test bidirectional message passing
  ;; Send a message and verify we receive it back
  (messenger-back:send-message (self) "Test message 1")
  (receive
    ((tuple msg1)
     (line (=:= "Test message 1" msg1)))
    (after 1000
      (line (=:= 'timeout 'should-receive-message))))
  ;; Send another message
  (messenger-back:send-message (self) "Test message 2")
  (receive
    ((tuple msg2)
     (line (=:= "Test message 2" msg2)))
    (after 1000
      (line (=:= 'timeout 'should-receive-message))))
  ;; Send a third message to ensure it's working consistently
  (messenger-back:send-message (self) "Mostly harmless")
  (receive
    ((tuple msg3)
     (line (=:= "Mostly harmless" msg3)))
    (after 1000
      (line (=:= 'timeout 'should-receive-message)))))

;;; messenger.lfe tests
(defun messenger_compile (config)
  (line (test-pat `#(ok (#(ok messenger ,_)) ())
                  (compile-example "messenger.lfe"))))

(defun messenger_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "messenger.lfe")
  ;; Spawn a messenger process
  (let ((pid (spawn 'messenger 'print-result '())))
    ;; Verify the process is alive
    (line (=:= 'true (is_process_alive pid)))
    ;; Send it some messages
    (! pid "Test message 1")
    (! pid "Test message 2")
    (! pid "Test message 3")
    ;; Give it a moment to process
    (timer:sleep 100)
    ;; Verify the process is still alive and processing messages
    (line (=:= 'true (is_process_alive pid)))
    ;; Clean up - kill the process
    (exit pid 'kill)
    ;; Verify it's dead
    (timer:sleep 50)
    (line (=:= 'false (is_process_alive pid)))))

;;; mnesia-demo.lfe tests
(defun mnesia_demo_compile (config)
  (line (test-pat `#(ok (#(ok mnesia-demo ,_)) ())
                  (compile-example "mnesia-demo.lfe"))))

(defun mnesia_demo_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "mnesia-demo.lfe")
  ;; Initialize the mnesia database
  (mnesia-demo:new)
  ;; Test by_place function - should return people in London
  (let (((tuple 'atomic results) (mnesia-demo:by_place 'london)))
    ;; Should have 4 people in London
    (line (=:= 4 (length results)))
    ;; Verify they're all person records with place='london
    ;; Records are tuples: #(person name place job), so place is at index 3
    (line (lists:all
           (lambda (p) (=:= 'london (element 3 p)))
           results)))
  ;; Test by_place_ms with match specification
  (let (((tuple 'atomic results) (mnesia-demo:by_place_ms 'paris)))
    ;; Should have people in Paris
    (line (=:= 'true (> (length results) 0))))
  ;; Clean up - stop mnesia
  (mnesia:stop))

;;; object-via-closure.lfe tests
(defun object_via_closure_compile (config)
  (line (test-pat `#(ok (#(ok object-via-closure ,_)) ())
                  (compile-example "object-via-closure.lfe"))))

(defun object_via_closure_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "object-via-closure.lfe")
  ;; Test fish class creation and methods
  (let ((fish (object-via-closure:fish-class "Salmon")))
    ;; Test species
    (line (=:= "Salmon" (object-via-closure:send fish 'species)))
    ;; Test children count
    (line (=:= 0 (object-via-closure:send fish 'children-count)))
    ;; Test id exists
    (let ((id (object-via-closure:send fish 'id)))
      (line (is_list id))
      (line (> (length id) 0)))
    ;; Test reproduce
    (let (((list fish2 baby) (object-via-closure:send fish 'reproduce)))
      (line (=:= 1 (object-via-closure:send fish2 'children-count)))
      (line (=:= 0 (object-via-closure:send baby 'children-count)))
      (line (=:= "Salmon" (object-via-closure:send baby 'species))))))

;;; object-via-process.lfe tests
(defun object_via_process_compile (config)
  (line (test-pat `#(ok (#(ok object-via-process ,_)) ())
                  (compile-example "object-via-process.lfe"))))

(defun object_via_process_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "object-via-process.lfe")
  ;; Test fish class creation and methods
  (let ((fish (object-via-process:init-fish "Trout")))
    ;; Test species
    (line (=:= "Trout" (object-via-process:send fish 'species)))
    ;; Test children count
    (line (=:= 0 (object-via-process:send fish 'children-count)))
    ;; Test id exists
    (let ((id (object-via-process:send fish 'id)))
      (line (is_list id))
      (line (> (length id) 0)))
    ;; Test reproduce
    (let ((baby (object-via-process:send fish 'reproduce)))
      (line (=:= 1 (object-via-process:send fish 'children-count)))
      (line (=:= 0 (object-via-process:send baby 'children-count)))
      (line (=:= "Trout" (object-via-process:send baby 'species))))))

;;; ping_pong.lfe tests
(defun ping_pong_compile (config)
  (line (test-pat `#(ok (#(ok ping-pong ,_)) ())
                  (compile-example "ping-pong.lfe"))))

(defun ping_pong_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "ping-pong.lfe")
  ;; Test gen_server functionality
  (line (test-pat `#(ok ,_) (ping-pong:start_link)))
  (line (=:= '#(pong 1) (ping-pong:ping)))
  (line (=:= '#(pong 2) (ping-pong:ping)))
  (line (=:= '#(pong 3) (ping-pong:ping)))
  (line (=:= 'ok (ping-pong:stop))))

;;; ring.lfe tests
(defun ring_compile (config)
  (line (test-pat `#(ok (#(ok ring ,_)) ())
                  (compile-example "ring.lfe"))))

(defun ring_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "ring.lfe")
  ;; We can't test ring:main directly because it calls erlang:halt
  ;; Instead, test the roundtrip function which is the core logic
  ;; Create a simple 3-process ring and send 5 messages
  (let* ((self-pid (self))
         ;; Spawn 3 ring processes
         (pid1 (spawn 'ring 'roundtrip `(1 ,self-pid)))
         (pid2 (spawn 'ring 'roundtrip `(2 ,pid1)))
         (pid3 (spawn 'ring 'roundtrip `(3 ,pid2))))
    ;; Start the message passing with 5 (will decrement to 1)
    (! pid3 5)
    ;; Wait to receive the final message (value 1)
    (receive
      (1 (line (=:= 'true 'true)))  ;; Got the message back
      (after 1000
        (line (=:= 'timeout 'should-not-timeout))))))

;;; sample-lfe-shellscript tests
(defun sample_lfe_shellscript (config)
  (let ((path (example-path "sample-lfe-shellscript")))
    ;; Verify the script exists and is executable
    (line (=:= 'true (filelib:is_file path)))
    ;; Test execution with valid input (using lfe interpreter)
    (let* ((cmd (++ path " 5"))
           (output (os:cmd cmd)))
      ;; Should output "factorial 5 = 120\n"
      (line (=:= "factorial 5 = 120\n" output)))
    ;; Test execution with no args (should show usage)
    (let* ((cmd path)
           (output (os:cmd cmd)))
      ;; Should contain "usage:" in the output
      (line (=/= 'nomatch (string:find output "usage:"))))
    ;; Test with another factorial value
    (let* ((cmd (++ path " 7"))
           (output (os:cmd cmd)))
      (line (=:= "factorial 7 = 5040\n" output)))))

;;; sample-lfescript tests
(defun sample_lfescript (config)
  (let ((path (example-path "sample-lfescript")))
    ;; Verify the script exists and is executable
    (line (=:= 'true (filelib:is_file path)))
    ;; Test execution with valid input
    (let* ((cmd (++ "lfescript " path " 5"))
           (output (os:cmd cmd)))
      ;; Should output "factorial 5 = 120\n"
      (line (=:= "factorial 5 = 120\n" output)))
    ;; Test execution with no args (should show usage)
    (let* ((cmd (++ "lfescript " path))
           (output (os:cmd cmd)))
      ;; Should contain "usage:" in the output
      (line (=/= 'nomatch (string:find output "usage:"))))
    ;; Test with another factorial value
    (let* ((cmd (++ "lfescript " path " 10"))
           (output (os:cmd cmd)))
      (line (=:= "factorial 10 = 3628800\n" output)))))

;;; simple-erl-exercises.lfe tests
(defun simple_erl_exercises_compile (config)
  (line (test-pat `#(ok (#(ok simple-erl-exercises ,_)) ())
                  (compile-example "simple-erl-exercises.lfe"))))

(defun simple_erl_exercises_functional (config)
  ;; Ensure the module is compiled and loaded first
  (compile-example "simple-erl-exercises.lfe")
  ;; Test temperature conversion
  (line (test-pat `#(f ,_) (simple-erl-exercises:convert #(c 0))))
  (line (test-pat `#(c ,_) (simple-erl-exercises:convert #(f 32))))
  ;; Test perimeter calculation
  (line (test-pat `#(square ,_) (simple-erl-exercises:perimeter #(square 5))))
  (line (test-pat `#(circle ,_) (simple-erl-exercises:perimeter #(circle 3))))
  (line (test-pat `#(triangle ,_) (simple-erl-exercises:perimeter #(triangle 3 4 5))))
  ;; Test min/max functions
  (line (=:= 1 (simple-erl-exercises:min '(5 3 1 9 2))))
  (line (=:= 9 (simple-erl-exercises:max '(5 3 1 9 2))))
  ;; Test min_max
  (line (=:= #(1 9) (simple-erl-exercises:min_max '(5 3 1 9 2))))
  (line (=:= #(1 9) (simple-erl-exercises:min_max2 '(5 3 1 9 2)))))
