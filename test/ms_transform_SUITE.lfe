;; Copyright (c) 2008-2020 Robert Virding
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed tfo in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; File    : ms_transform_SUITE.lfe
;; Author  : Robert Virding
;; Purpose : Match specification test suite.

;; This is a direct translation of ms_transfom_SUITE.erl from R14B02 except
;; for tests with guards containing ';'. We have usually removed these
;; or been careful with these as they don't handle errors the same way
;; as 'or' (which is all we have).
;;
;; Note that some of these tests are not LFE specific but more general
;; guard tests but we include them anyway for completeness.
;;
;; As ets-ms is a macro we can expand it directly inline which means
;; that many errors/inconsistencies will be detected at compile
;; time. Should we write it to a file or in memory and compile at
;; run-time?
;;
;; NOTE: ets-ms generates a pattern so when testing the pattern with
;; test-pat it *must* be used in the pattern not in the value.

(include-file "test_server.lfe")

(defmodule ms_transform_SUITE
  (export (all 0) (suite 0) (groups 0) (init_per_suite 1) (end_per_suite 1)
          (init_per_group 2) (end_per_group 2)
          (init_per_testcase 2) (end_per_testcase 2))
  (export (basic_ets 1) (basic_dbg 1) (from_shell 1) (records 1)
          (record_index 1) (multipass 1) (top_match 1) (old_guards 1)
          (autoimported 1) (semicolon 1) (bitsyntax 1)
          (record_defaults 1) (andalso_orelse 1)
          (float_1_function 1) (action_function 1)
          (warnings 1) (no_warnings 1)))

(defmacro MODULE () `'ms_transform_SUITE)

(defun all ()
  ;; (: test_lib recompile (MODULE))
  (list 'from_shell 'basic_ets 'basic_dbg 'records
        'record_index 'multipass 'bitsyntax 'record_defaults
        'andalso_orelse 'float_1_function 'action_function
        'warnings 'no_warnings 'top_match 'old_guards
        'autoimported 'semicolon
        ))

;;(defun suite () (list (tuple 'ct_hooks (list 'ts_install_cth))))
(defun suite () ())

(defun groups () ())

(defun init_per_suite (config) config)

(defun end_per_suite (config) 'ok)

(defun init_per_group (name config) config)

(defun end_per_group (name config) config)

(defun init_per_testcase (func config)
  (let ((dog (: test_server timetrap (: test_server seconds 360))))
    (cons (tuple 'watchdog dog) config)))

(defun end_per_testcase (func config)
  (let ((dog (config 'watchdog config)))
    (: test_server timetrap_cancel dog)))

(defun warnings
  (['suite] ())
  (['doc] '"Check that shadowed variables in fun head generate warning")
  ([config] (when (is_list config))
   ;; Not relevant to LFE.
   'ok))

(defun no_warnings
  (['suite] ())
  (['doc] '"Check that variables bound in other function clauses don't generate warning.")
  ([config] (when (is_list config))
   ;; Not relevant to LFE.
   'ok))

(defun andalso_orelse
  (['suite] ())
  (['doc] '"Tests that andalso and orelse are allowed in guards.")
  ([config] (when (is_list config))
   (setup config)
   ;; [{{'$1','$2'},
   ;;  [{'and',{is_integer,'$1'},{'>',{'+','$1',5},'$2'}}],
   ;;  [{'andalso','$1','$2'}]}]
   (test-pat '(#(#($1 $2)
                 (#(and #(is_integer $1) #(> #(+ $1 5) $2)))
                 (#(andalso $1 $2))))
             (ets-ms ([(tuple a b)]
                      (when (and (is_integer a) (> (+ a 5) b)))
                      (andalso a b))))
   ;; [{{'$1','$2'},
   ;;  [{'or',{is_atom,'$1'},{'>',{'+','$1',5},'$2'}}],
   ;;  [{'orelse','$1','$2'}]}]
   (test-pat '(#(#($1 $2)
                 (#(or #(is_atom $1) #(> #(+ $1 5) $2)))
                 (#(orelse $1 $2))))
             (ets-ms ([(tuple a b)]
                      (when (or (is_atom a) (> (+ a 5) b)))
                      (orelse a b))))
   ;; [{{'$1','$2'},
   ;;  [{'andalso',{is_integer,'$1'},{'>',{'+','$1',5},'$2'}}],
   ;;  ['$1']}]
   (test-pat '(#(#($1 $2)
                 (#(andalso #(is_integer $1) #(> #(+ $1 5) $2)))
                 ($1)))
             (ets-ms ([(tuple a b)]
                      (when (andalso (is_integer a) (> (+ a 5) b)))
                      a)))
   ;; [{{'$1','$2'},
   ;;  [{'orelse',{is_atom,'$1'},{'>',{'+','$1',5},'$2'}}],
   ;;  ['$1']}]
   (test-pat '(#(#($1 $2)
                 (#(orelse #(is_atom $1) #(> #(+ $1 5) $2)))
                 ($1)))
             (ets-ms ([(tuple a b)]
                      (when (orelse (is_atom a) (> (+ a 5) b)))
                      a)))

   'ok))

(defun bitsyntax
  (['suite] ())
  (['doc] '"Tests that bitsyntax works and does not work where appropriate")
  ([config] (when (is_list config))
   (line (setup config))
   (line (let ((a 27))
           (test-pat '(#(_ () (#b(0 27 0 27))))
                     (ets-ms ([_] (binary (a (size 16)) (27 (size 16)))))
                     )))
   (line (let ((a 13))                  ;Must fit in 4 bits!
           (test-pat '(#(#(#b(15 47) $1 $2)
                         (#(=:= $1 #b(0 13)) #(=:= $2 #b(27 28 19)))
                         (#b(220 0 13))))
                     (ets-ms ([(tuple #b(15 47) b c)]
                              (when (=:= b (binary (a (size 16))))
                                    (=:= c (binary 27 28 19)))
                              (binary (a (size 4)) (12 (size 4))
                                      (13 (size 16)))))
                     )))

   'ok))

(defrecord d a b c (d 'foppa))          ;Changed record name from a

(defun record_defaults
  (['suite] ())
  (['doc] '"Tests that record defaults works")
  ([config] (when (is_list config))
   (line (setup config))
   ;; [{{<<27>>,{a,5,'$1',hej,hej}},
   ;;   [],
   ;;   [{{a,hej,{'*','$1',2},flurp,flurp}}]}
   (line (test-pat '(#(#(#b(27) #(d 5 $1 hej hej))
                       ()
                       (#(#(d hej #(* $1 2) flurp flurp)))))
                   (ets-ms ([(tuple #b(27) (match-d a 5 b b _ 'hej))]
                            (make-d a 'hej b (* b 2) _ 'flurp)))))

   'ok))

(defun basic_ets
  (['suite] ())
  (['doc] '"Tests basic ets:fun2ms")
  ([config] (when (is_list config))
   (line (setup config))
   (line (test-pat '(#(#(a b) () (true)))
                   (ets-ms ([(tuple 'a 'b)] 'true))))
   (line (test-pat '(#(#($1 foo) (#(is_list $1)) (#(#(#(hd $1) $_))))
                     #(#($1 $1) (#(is_tuple $1)) (#(#(#(element 1 $1) $*)))))
                   (ets-ms ([(tuple x 'foo)] (when (is_list x))
                            (tuple (hd x) (object)))
                           ([(tuple x x)] (when (is_tuple x))
                            (tuple (element 1 x) (bindings))))
                   ))
   (line (test-pat '(#(#($1 $2) () (#(#($2 $1)))))
                   (ets-ms ([(tuple a b)] (tuple b a)))))
   (line (test-pat '(#(#($1 $2) () (($2 $1))))
                   (ets-ms ([(tuple a b)] (list b a)))))

   'ok))

(defun basic_dbg
  (['suite] ())
  (['doc] '"Tests basic dbg:fun2ms using the trace-ms macro")
  ([config] (when (is_list config))
   (line (setup config))
   (line (test-pat '(#((a b) () (#(message banan) #(return_trace))))
                   (trace-ms ([(list 'a 'b)]
                              (message 'banan) (return_trace)))))
   (line (test-pat '(#(($1 $2) () (#(#($2 $1)))))
                   (trace-ms ([(list a b)] (tuple b a)))))
   (line (test-pat '(#(($1 $2) () (($2 $1))))
                   (trace-ms ([(list a b)] (list b a)))))
   (line (test-pat '(#(($1 $2) () ($*)))
                   (trace-ms ([(list a b)] (bindings)))))
   (line (test-pat '(#(($1 $2) () ($_)))
                   (trace-ms ([(list a b)] (object)))))
   (line (test-pat '(#(() () (#(return_trace))))
                   (trace-ms ([()] (return_trace)))))
   'ok))

(defun from_shell
  (['suite] ())
  (['doc] '"Test calling of ets/dbg:fun2ms from the shell")
  ([config] (when (is_list config))
   ;; Not relevant to LFE.
   'ok))

(defrecord t (t1 ()) (t2 'foo) t3 t4)

;; Some of these tests must be compiled as we cannot build a pattern
;; of them.

(defun records
  (['suite] ())
  (['doc] '"Tests expansion of records in fun2ms")
  ([config] (when (is_list config))
   (line (setup config))

   ;; [{[{t,'$1','$2',foo,'_'}],[{is_list,'$1'}],[{{{hd,'$1'},'$_'}}]},
   ;;  {[{t,'_','_','_','_'}],[{'==',{element,2,{hd,'$_'}},nisse}],[{{'$*'}}]}]

   (line
    (test-pat
     '(#(#(t $1 $2 foo _) (#(is_list $1)) (#(#(#(hd $1) $_))))
       #(#(t _ _ _ _) (#(== #(element 2 $_) nisse)) (#(#($*)))))
     (ets-ms ([(match-t t1 x t2 y t3 'foo _ '_)] (when (is_list x))
              (tuple (hd x) (object)))
             ([(match-t _ '_)] (when (== (t-t1 (object)) 'nisse))
              (tuple (bindings))))
     ))

   ;; [{{t,'$1','$2','_',foo},[{'==',{element,4,'$_'},7},{is_list,'$1'}],
   ;;   [{{{hd,'$1'},'$_'}}]},
   ;;  {'$1',[{is_record,'$1',t,5}],
   ;;   [{{{element,2,'$1'},
   ;;      {{t,'$1',foo,undefined,undefined}},
   ;;      {{t,{element,2,'$1'},{element,3,'$1'},{element,4,'$1'},boooo}}}}]}]
   (line
    (test-pat
     '(#(#(t $1 $2 _ foo) (#(== #(element 4 $_) 7) #(is_list $1))
         (#(#(#(hd $1) $_))))
       #($1
         (#(is_record $1 t))
         (#(#(#(element 2 $1)
              #(#(t $1 foo undefined undefined))
              #(setelement 5 $1 boooo))))))
     (ets-ms ([(match-t t1 x t2 y t4 'foo _ '_)]
              (when (== (t-t3 (object)) 7) (is_list x))
              (tuple (hd x) (object)))
             ([a]
              (when (is-t a))
              (tuple (t-t1 a) (make-t t1 a) (set-t-t4 a 'boooo))))
     ))

   ;; [{[{t,'$1','$2',foo,'_'}],[{is_list,'$1'}],[{{{hd,'$1'},'$_'}}]},
   ;;  {[{t,'_','_','_','_'}],[{'==',{element,2,{hd,'$_'}},nisse}],[{{'$*'}}]}]
   (line
    (test-pat
     '(#((#(t $1 $2 foo _)) (#(is_list $1)) (#(#(#(hd $1) $_))))
       #((#(t _ _ _ _)) (#(== #(element 2 #(hd $_)) nisse)) (#(#($*)))))
     (trace-ms ([(list (match-t t1 x t2 y t3 'foo _ '_))] (when (is_list x))
                (tuple (hd x) (object)))
               ([(list (match-t _ '_))] (when (== (t-t1 (hd (object))) 'nisse))
                (tuple (bindings))))
     ))

   'ok))

(defrecord a a b)

(defun record_index
  (['suite] ())
  (['doc] '"Tests expansion of records in fun2ms, part 2")
  ([config] (when (is_list config))
   (line (setup config))
   (line (test-pat '(#(#(2) () (true)))
                   (ets-ms ([(tuple (a-a))] 'true))))
   (line (test-pat '(#(#(2) () (2)))
                   (ets-ms ([(tuple (a-a))] (a-a)))))
   (line (test-pat '(#(#(2 $1) (#(> $1 2)) (2)))
                   (ets-ms ([(tuple (a-a) a)] (when (> a (a-a))) (a-a)))))

   'ok))

;; Already defined.
;; (defrecord a a b)

(defun top_match
  (['suite] ())
  (['doc] '"Tests matching on top level in head to give alias for object()")
  ([config] (when (is_list config))
   (line (setup config))
   (line (test-pat '(#(#(a 3 _) () ($_)))
                   (ets-ms ([(= (match-a a 3 _ '_) a)] a))))
   (line (test-pat '(#(#(a 3 _) () ($_)))
                   (ets-ms ([(= (match-a a 3 _ '_) a)] a))))
   (line (test-pat '(#((a b) () ($_)))
                   (trace-ms ([(= a (list 'a 'b))] a))))
   (line (test-pat '(#((a b) () ($_)))
                   (trace-ms ([(= (list 'a 'b) a)] a))))

   'ok))

(defun multipass
  (['suite] ())
  (['doc] '"Tests that multi-defined fields in records give errors.")
  ([config] (when (is_list config))

   'ok))

(defun old_guards
  (['suite] ())
  (['doc] '"Tests that old type tests in guards are translated")
  ([config] (when (is_list config))
   ;; Not relevant to LFE.
   'ok))

(defun autoimported
  (['suite] ())
  (['doc] '"Tests use of autoimported bif's used like erlang:'+'(A,B) in guards and body.")
  ([config] (when (is_list config))

   'ok))

(defun semicolon
  (['suite] ())
  (['doc] '"Tests semicolon in guards of match_specs.")
  ([config] (when (is_list config))
   ;; Not relevant to LFE.
   'ok))

(defun float_1_function
  (['suite] ())
  (['doc] '"OTP-5297. The function float/1.")
  ([config] (when (is_list config))
   ;; Not relevant to LFE.
   'ok))

(defun action_function
  (['suite] ())
  (['doc] '"Test all 'action functions'.")
  ([config] (when (is_list config))
   (line (setup config))
   (line (test-pat '(#(($1 $2) () (#(set_seq_token label 0)
                                   #(get_seq_token)
                                   #(message $1)
                                   #(return_trace)
                                   #(exception_trace))))
                   (trace-ms ([(list x y)]
                              (set_seq_token 'label 0)
                              (get_seq_token)
                              (message x)
                              (return_trace)
                              (exception_trace)))))
   (line (test-pat '(#(($1 $2) () (#(process_dump)
                                   #(enable_trace send)
                                   #(enable_trace $2 send)
                                   #(disable_trace procs)
                                   #(disable_trace $2 procs))))
                   (trace-ms ([(list x y)]
                              (process_dump)
                              (enable_trace 'send)
                              (enable_trace y 'send)
                              (disable_trace 'procs)
                              (disable_trace y 'procs)))))
   (line (let ((a 16))
           (test-pat '(#(($1 $2) () (#(display $1)
                                     #(caller)
                                     #(set_tcw 16) ;#(const 16)?
                                     #(silent true)
                                     #(trace (send) (procs))
                                     #(trace $2 (procs) (send)))))
                     (trace-ms ([(list x y)]
                                (display x)
                                (caller)
                                (set_tcw a)
                                (silent 'true)
                                (trace (list 'send) (list 'procs))
                                (trace y (list 'procs) (list 'send)))))))

   'ok))

;; Utilites

(defun setup (config)
  (put 'mts_config config)
  (put 'mts_tf_counter 0))

(defun temp-name ()
  (let* ((conf (get 'mts_config))
         (c (get 'mts_tf_counter)))
    (put 'mts_tf_counter (+ c 1))
    (: filename join (list (config 'priv_dir conf)
                           (++ '"tempfile" (integer_to_list c) '".tmp")))))
(defun compile-and-run (recs expr)
  (let ((fn (++ (temp-name) ".lfe"))
        (prog (++ "(defmodule tmp (export (tmp 0)))\n"
                  recs
                  "(defun tmp () " expr ")\n")))
    (file:write_file fn prog)
    (let ((`#(ok (#(ok tmp ,bin))) (lfe_comp:file fn '(binary))))
      (code:load_binary 'tmp fn bin)
      (tmp:tmp))))

(defun do-eval (s)
  (let (((tuple 'ok e) (lfe_io:read_string s)))
    (lfe_eval:expr e)))
