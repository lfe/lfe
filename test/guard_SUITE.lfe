;; Copyright (c) 2008-2013 Robert Virding
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

;; File    : guard_SUITE.lfe
;; Author  : Robert Virding
;; Purpose : Guard test suite from R14B02.

;; This is a direct translation of guard_SUITE.erl from R14B02 except
;; for tests with guards containing ';'. We have usually removed these
;; or been careful with these as they don't handle errors the same way
;; as 'or' (which is all we have).
;;
;; Note that some of these tests are not LFE specific but more general
;; guard tests but we include them anyway for completeness.

(include-file "test_server.lfe")

(defmodule guard_SUITE
  (export (all 0) (suite 0) (groups 0) (init_per_suite 1) (end_per_suite 1)
      (init_per_group 2) (end_per_group 2)
      (misc 1) (const_cond 1) (basic_not 1) (complex_not 1) (nested_nots 1)
      (semicolon 1) (complex_semicolon 1) (comma 1)
      (or_guard 1) (more_or_guards 1)
      (complex_or_guards 1) (and_guard 1)
       (xor_guard 1) (more_xor_guards 1)
      (old_guard_tests 1)
      (build_in_guard 1) (gbif 1)
      (t_is_boolean 1) (is_function_2 1)
      (tricky 1) (rel_ops 1) (literal_type_tests 1)
      (basic_andalso_orelse 1) (traverse_dcd 1)
      (check_qlc_hrl 1) (andalso_semi 1) (t_tuple_size 1) (binary_part 1)
      ))

(defmacro MODULE () `'guard_SUITE)

(defun all ()
  ;; (: test_lib recompile (MODULE))
  (list 'misc 'const_cond 'basic_not 'complex_not 'nested_nots
    'semicolon 'complex_semicolon 'comma 'or_guard
    'more_or_guards 'complex_or_guards 'and_guard 'xor_guard
    'more_xor_guards 'build_in_guard 'old_guard_tests 'gbif
    't_is_boolean 'is_function_2 'tricky 'rel_ops
    'literal_type_tests 'basic_andalso_orelse 'traverse_dcd
    'check_qlc_hrl 'andalso_semi 't_tuple_size 'binary_part))

;;(defun suite () (list (tuple 'ct_hooks (list 'ts_install_cth))))
(defun suite () ())

(defun groups () ())

(defun init_per_suite (config) config)

(defun end_per_suite (config) 'ok)

(defun init_per_group (name config) config)

(defun end_per_group (name config) config)

(defun misc
  ([config] (when (is_list config))
   (line (test-pat 42 (case (id 42)
            (x (when (- x)) 'ok)
            (x x))))
   (line (test-pat (tuple 'a 'b 'c)
           (misc-1 '(#(#(a b c)) #((4)) #((3)) #(-2)))))
   (line (test-pat 'none (misc-1 '(#(#(a b c)) #((4)) #((3)) #(-3)))))
   (line (test-pat 'none (misc-1 '(#(#(a b c)) #((4)) #((7)) #(-2)))))
   (line (test-pat 'none (misc-1 '(#(#(a b c)) #((4)) #((3)) #((1 2 3))))))

   (line (test-pat (tuple 'ok 'buf #b()) (get-data #(o true raw) 0 'buf)))
   (line (test-pat (tuple 'ok 'buf #b()) (get-data #(o true raw) 42 'buf)))
   (line (test-pat (tuple 'ok 'buf #b()) (get-data #(o false raw) 0 'buf)))
   (line (test-pat 'error (get-data #(o false raw) 42 'buf)))

   (line (test-pat (tuple 'ok 'buf #b()) (get-data #(o true 0) 0 'buf)))
   (line (test-pat (tuple 'ok 'buf #b()) (get-data #(o true 0) 42 'buf)))
   (line (test-pat (tuple 'ok 'buf #b()) (get-data #(o false 0) 0 'buf)))
   (line (test-pat 'error (get-data #(o false 0) 42 'buf)))
   'ok))

;; This is semicolon safe.
(defun misc-1
  ([(list (tuple w) (tuple x) (tuple y) (tuple z))]
   (eif (andalso (> x y) (=:= (abs z) 2)) (id w)
    'true 'none)))

;; This is semicolon safe.
(defun get-data
  ([(tuple 'o active raw) bytes buffer] (when (or (=:= raw 'raw) (=:= raw 0)))
   (eif (orelse (=/= active 'false) (=:= bytes 0)) (tuple 'ok buffer #b())
    'true 'error)))

(defun const_cond
  ([config] (when (is_list config))
   (line (test-pat 'ok (const-cond #() 0)))
   (line (test-pat 'ok (const-cond #(a) 1)))
   (line (test-pat 'error (const-cond #(a b) 3)))
   (line (test-pat 'error (const-cond #(a) 0)))
   (line (test-pat 'error (const-cond #(a b) 1)))
   'ok))

(defun const-cond (t sz)
  (case t
    (_ (when 'false) 'never)
    (_ (when (is_tuple t) (== 'eq 'eq) (== (tuple_size t) sz)) 'ok)
    (_ (when (is_tuple t) (== 'eq 'leq) (== (tuple_size t) sz)) 'ok)
    (_ 'error)))

(defun basic_not
  ([config] (when (is_list config))
   (let* ((true (id 'true))
      (false (id 'false))
      (glurf (id 'glurf))
      (a (id 5))
      (b (id 37.5))
      (c (id -1))
      (d (id 5))
      (atuple (tuple false true glurf)))

     ;; These are all semicolon safe.
     (line (check (lambda () (eif (not 'false) 'ok 'true 'error)) 'ok))
     (line (check (lambda () (eif (not 'true) 'ok 'true 'error)) 'error))
     (line (check (lambda () (eif (not false) 'ok 'true 'error)) 'ok))
     (line (check (lambda () (eif (not true) 'ok 'true 'error)) 'error))

     (line (check (lambda () (eif (> a b) 'gt (< a b) 'lt (== a b) 'eq)) 'lt))
     (line (check (lambda () (eif (> a c) 'gt (< a c) 'lt (== a b) 'eq)) 'gt))
     (line (check (lambda () (eif (> a d) 'gt (< a d) 'lt (== a d) 'eq)) 'eq))

     (line (check (lambda () (eif (not (> 7 453)) 'le (not (< 7 453)) 'ge
                  (not (== 7 453)) 'ne 'true 'eq)) 'le))
     (line (check (lambda () (eif (not (> 7 -8)) 'le (not (< 7 -8)) 'ge
                  (not (== 7 -8)) 'ne 'true 'eq)) 'ge))
     (line (check (lambda () (eif (not (> 7 7)) 'le (not (< 7 7)) 'ge
                  (not (== 7 7)) 'ne 'true 'eq)) 'le))

     (line (check (lambda () (eif (not (> a b)) 'le (not (< a b)) 'le
                  (not (== a b)) 'ne 'true 'eq)) 'le))
     (line (check (lambda () (eif (not (> a c)) 'le (not (< a c)) 'ge
                  (not (== a c)) 'ne 'true 'eq)) 'ge))
     (line (check (lambda () (eif (not (> a d)) 'le (not (< a d)) 'ge
                  (not (== a d)) 'ne 'true 'eq)) 'le))

     (line (check (lambda () (eif (not (element 1 atuple)) 'ok 'true 'error)) 'ok))
     (line (check (lambda () (eif (not (element 2 atuple)) 'ok 'true 'error)) 'error))
     (line (check (lambda () (eif (not (element 3 atuple)) 'ok 'true 'error)) 'error))

     (line (check (lambda () (eif (not 'glurf) 'ok 'true 'error)) 'error))
     (line (check (lambda () (eif (not glurf) 'ok 'true 'error)) 'error))

     'ok)))

(defun complex_not
  ([config] (when (is_list config))
   (let ((atuple (id #(false true gurka))))
     ;; These are all semicolon safe.
     (line (check (lambda () (eif (not (element 1 atuple)) 'ok 'true 'error)) 'ok))
     (line (check (lambda () (eif (not (element 2 atuple)) 'ok 'true 'error)) 'error))

     (line (check (lambda () (eif (not (== (element 3 atuple) 'gurka)) 'ok
                  'true 'error)) 'error))
     (line (check (lambda () (eif (not (=/= (element 3 atuple) 'gurka)) 'ok
                  'true 'error)) 'ok))

     (line (check (lambda () (eif (== (tuple 'a (not (element 2 atuple)))
                      #(a false)) 'ok 'true 'error)) 'ok))
     (line (check (lambda () (eif (== (tuple 'a (not (element 1 atuple)))
                      #(a false)) 'ok 'true 'error)) 'error))

     (line (check (lambda () (eif (not (or (element 1 atuple)
                       (element 3 atuple)))
                  'ok 'true 'error)) 'error))
     ;; Orelse
     (line (check (lambda () (eif (not (orelse (element 1 atuple)
                           (element 3 atuple)))
                  'ok 'true 'error)) 'error))

     'ok)))

(defun nested_nots
  ([config] (when (is_list config))
   ;; These are all semicolon safe.
   (line (test-pat 'true (nested-not-1 0 0)))
   (line (test-pat 'true (nested-not-1 0 1)))
   (line (test-pat 'true (nested-not-1 'a 'b)))
   (line (test-pat 'true (nested-not-1 10 0)))
   (line (test-pat 'false (nested-not-1 'z 'a)))
   (line (test-pat 'false (nested-not-1 3.4 #(anything goes))))
   (line (test-pat 'false (nested-not-1 3.4 'atom)))
   (line (test-pat 'true (nested-not-1 3.0 '(list))))

   (line (test-pat 'true (nested-not-2 'false 'false 42)))
   (line (test-pat 'true (nested-not-2 'false 'true 42)))
   (line (test-pat 'true (nested-not-2 'true 'false 42)))
   (line (test-pat 'true (nested-not-2 'true 'true 42)))
   (line (test-pat 'true (nested-not-2 'false 'false 'atom)))
   (line (test-pat 'false (nested-not-2 'false 'true 'atom)))
   (line (test-pat 'false (nested-not-2 'true 'false 'atom)))
   (line (test-pat 'false (nested-not-2 'true 'true 'atom)))
   'ok))

(defun nested-not-1
  ([x y] (when (not (and (or (> x y) (not (is_atom x)))
                 (or (is_atom y) (== x 3.4)))))
   'true)
  ([_ _] 'false))

(defun nested-not-2 (x y z)
  (nested-not-2 x y z 'true))

(defun nested-not-2
  ([x y z true] (when (not (and true (not (or (and (not x) (not y))
                          (not (is_atom z)))))))
   'true)
  ([_ _ _ _] 'false))

(defun semicolon
  ([config] (when (is_list config))
   ;; Not relevant for LFE.
   'ok))

(defun complex_semicolon
  ([config] (when (is_list config))
   ;; Not relevant for LFE.
   'ok))

;; Use (progn ...) as equivalent of comma, this is reasonable.
(defun comma
  ([config] (when (is_list config))

   ;; ',' combinations of literal true/false.
   (line (check (lambda () (eif (progn 'true 'false) 'ok 'true 'error)) 'error))
   (line (check (lambda () (eif (progn 'false 'true) 'ok 'true 'error)) 'error))
   (line (check (lambda () (eif (progn 'true 'true) 'ok)) 'ok))
   (line (check (lambda () (eif (progn 'false 'false) 'ok 'true 'error)) 'error))
   (line (check (lambda () (let (((tuple 'EXIT (tuple (tuple 'case_clause _) _))
                  (catch (eif (progn 'true 'false) 'ok
                          (progn 'false 'true) 'ok
                          (progn 'false 'false) 'ok))))
                 'exit))
        'exit))

   (let ((true (id 'true))
     (false (id 'false))
     (glurf (id 'glurf))
     (atuple (id #(a b c))))

     ;; ',' combinations of true/false in variables.
     (line (check (lambda () (eif (progn true false) 'ok 'true 'error)) 'error))
     (line (check (lambda () (eif (progn false true) 'ok 'true 'error)) 'error))
     (line (check (lambda () (eif (progn true true) 'ok 'true 'error)) 'ok))
     ;; These used to crash the compiler!
     (line (check (lambda () (eif (progn false false) 'ok 'true 'error)) 'error))
     (line (check (lambda () (let (((tuple 'EXIT (tuple (tuple 'case_clause _) _))
                    (catch (eif (progn true false) 'ok
                        (progn false true) 'ok
                        (progn false false) 'ok))))
                   'exit))
          'exit))

     ;; ',' combinations of true/false, and non-boolean in variables.
     (line (check (lambda () (eif (progn true glurf) 'ok 'true 'error)) 'error))
     (line (check (lambda () (eif (progn glurf true) 'ok 'true 'error)) 'error))
     (line (check (lambda () (eif (progn true true) 'ok)) 'ok))
     ;; These used to crash the compiler!
     (line (check (lambda () (eif (progn glurf glurf) 'ok 'true 'error)) 'error))
     (line (check (lambda () (let (((tuple 'EXIT (tuple (tuple 'case_clause _) _))
                    (catch (eif (progn true glurf) 'ok
                        (progn glurf true) 'ok
                        (progn glurf glurf) 'ok))))
                   'exit))
          'exit))

     ;; ',' combinations of true/false with errors.
     (line (check (lambda () (eif (progn true (element 42 atuple)) 'ok
                  'true 'error)) 'error))
     (line (check (lambda () (eif (progn (element 42 atuple) true) 'ok
                  'true 'error)) 'error))
     (line (check (lambda () (eif (progn true true) 'ok)) 'ok))
     (line (check (lambda () (eif (progn (element 42 atuple)
                     (element 42 atuple))
                  'ok 'true 'error)) 'error))
     (line (check (lambda ()
            (let (((tuple 'EXIT (tuple (tuple 'case_clause _) _))
               (catch (eif (progn true (element 42 atuple)) 'ok
                       (progn (element 42 atuple) true) 'ok
                       (progn (element 42 atuple)
                          (element 42 atuple)) 'ok))))
                   'exit))
          'exit)))

   'ok))

(defun or_guard
  ([config] (when (is_list config))
   (let ((true (id 'true))
     (false (id 'false))
     (glurf (id 'glurf)))

     ;; 'or' combinations of literal true/false.
     (line (check (lambda () (eif (or 'true 'false) 'ok)) 'ok))
     (line (check (lambda () (eif (or 'false 'true) 'ok)) 'ok))
     (line (check (lambda () (eif (or 'true 'true) 'ok)) 'ok))
     (line (check (lambda () (eif (or 'false 'false) 'ok 'true 'error)) 'error))

     (line (check (lambda () (eif (or 'glurf 'true) 'ok 'true 'error)) 'error))
     (line (check (lambda () (eif (or 'true 'glurf) 'ok 'true 'error)) 'error))
     (line (check (lambda () (eif (or 'glurf 'glurf) 'ok 'true 'error)) 'error))

     (line (check (lambda () (let (((tuple 'EXIT (tuple (tuple 'case_clause _) _))
                    (catch (eif (or 'false 'false) 'ok))))
                   'exit))
          'exit))

     ;; 'or' combinations using variables containing true/false.
     (line (check (lambda () (eif (or true false) 'ok)) 'ok))
     (line (check (lambda () (eif (or false true) 'ok)) 'ok))
     (line (check (lambda () (eif (or true true) 'ok)) 'ok))
     ;; These used to crash the compiler!
     (line (check (lambda () (eif (or false false) 'ok 'true 'error)) 'error))

     (line (check (lambda () (eif (or true glurf) 'ok 'true 'error)) 'error))
     (line (check (lambda () (eif (or glurf true) 'ok 'true 'error)) 'error))
     ;; These used to crash the compiler!
     (line (check (lambda () (eif (or glurf glurf) 'ok 'true 'error)) 'error))

     (line (check (lambda ()
                 (let (((tuple 'EXIT (tuple (tuple 'case_clause _) _))
                    (catch (eif (or false false) 'ok))))
                   'exit))
               'exit)))

   'ok))

(defun more_or_guards
  ([config] (when (is_list config))
   (let* ((true (id 'true))
      (false (id 'false))
      (atuple (id #(false true gurks))))

     (line (check (lambda ()
            (eif (or (element 42 atuple) false) 'ok 'true 'error))
          'error))

     (line (check (lambda ()
            (eif (or false (element 42 atuple)) 'ok 'true 'error))
          'error))

     (line (check (lambda ()
            (eif (or (element 18 atuple) (element 42 atuple)) 'ok
             'true 'error))
          'error))

     (line (check (lambda ()
            (eif (or true (element 42 atuple)) 'ok
             'true 'error))
          'error))

     (line (check (lambda ()
            (eif (or (element 42 atuple) true) 'ok
             'true 'error))
          'error))

     (line (check (lambda ()
            (eif (or (or (element 1 atuple) (element 42 atuple)) true)
             'ok 'true 'error))
          'error))

     (line (check (lambda ()
            (eif (or (or (element 1 atuple) true) (element 42 atuple))
             'ok 'true 'error))
          'error))

     (line (check (lambda ()
            (eif (or (== (binary (false (size 8))) #b(0))
                 (element 2 atuple)) 'ok
             'true 'error))
          'error))

     (line (check (lambda ()
            (eif (or (element 2 atuple)
                 (== (binary (true (size 8))) #b(0))) 'ok
             'true 'error))
          'error))

     (line (check (lambda ()
            (eif (or (element 2 atuple) (element 42 atuple)) 'ok
             'true 'error))
          'error))

     (line (check (lambda ()
            (eif (or (or (element 1 atuple) (element 2 atuple))
                 (element 19 atuple)) 'ok
             'true 'error))
          'error)))
   'ok))

(defun complex_or_guards
  ([config] (when (is_list config))
   ;; complex-or-1/2
   (line (test-pat 'ok (complex-or-1 #(a b c d) #(1 2 3))))
   (line (test-pat 'ok (complex-or-1 #(a b c d) #(1))))
   (line (test-pat 'ok (complex-or-1 #(a) #(1 2 3))))
   (line (test-pat 'error (complex-or-1 #(a) #(1))))

   (line (test-pat 'error (complex-or-1 1 2)))
   (line (test-pat 'error (complex-or-1 () #(a b c d))))
   (line (test-pat 'error (complex-or-1 #(a b c d) ())))

   ;; complex-or-2/1
   (line (test-pat 'ok (complex-or-2 #(true #()))))
   (line (test-pat 'ok (complex-or-2 #(false #(a)))))
   (line (test-pat 'ok (complex-or-2 #(false #(a b c)))))
   (line (test-pat 'ok (complex-or-2 #(true #(a b c)))))

   (line (test-pat 'error (complex-or-2 #(blurf #(a b c)))))

   (line (test-pat 'error (complex-or-2 #(true))))
   (line (test-pat 'error (complex-or-2 #(true no_tuple))))
   (line (test-pat 'error (complex-or-2 #(true ()))))

   ;; complex-or-3/2
   (line (test-pat 'ok (complex-or-3 #(true) #())))
   (line (test-pat 'ok (complex-or-3 #(false) #(a))))
   (line (test-pat 'ok (complex-or-3 #(false) #(a b c))))
   (line (test-pat 'ok (complex-or-3 #(true) #(a b c d))))
   (line (test-pat 'ok (complex-or-3 #(false) #b(1 2 3))))
   (line (test-pat 'ok (complex-or-3 #(true) #b(1 2 3 4))))

   (line (test-pat 'error (complex-or-3 'blurf #(a b c))))

   (line (test-pat 'error (complex-or-3 #(false) #b(1 2 3 4))))
   (line (test-pat 'error (complex-or-3 () #b(1 2))))
   (line (test-pat 'error (complex-or-3 #(true) 45)))
   (line (test-pat 'error (complex-or-3 #b() #b())))

   ;; complex-or-4/2
   (line (test-pat 'ok (complex-or-4 #b(1 2 3) #(true))))
   (line (test-pat 'ok (complex-or-4 #b(1 2 3) #(false))))
   (line (test-pat 'ok (complex-or-4 #b(1 2 3) #(true))))
   (line (test-pat 'ok (complex-or-4 #(1 2 3) #(true))))
   (line (test-pat 'error (complex-or-4 #(1 2 3 4) #(false))))

   (line (test-pat 'error (complex-or-4 #(1 2 3 4) ())))
   (line (test-pat 'error (complex-or-4 () #(true))))

   ;; complex-or-5/2
   (line (test-pat 'ok (complex-or-5 #b(1) #(false))))
   (line (test-pat 'ok (complex-or-5 #b(1 2 3) #(true))))
   (line (test-pat 'ok (complex-or-5 #b(1 2 3 4) #(false))))
   (line (test-pat 'ok (complex-or-5 #(1 2 3) #(false))))
   (line (test-pat 'ok (complex-or-5 #(1 2 3 4) #(false))))

   (line (test-pat 'error (complex-or-5 'blurf #(false))))
   (line (test-pat 'error (complex-or-5 #b(1) 'klarf)))
   (line (test-pat 'error (complex-or-5 'blurf 'klarf)))

   ;; complex-or-6/2
   (line (test-pat 'ok (complex-or-6 #(true true) #(1 2 3 4))))
   (line (test-pat 'ok (complex-or-6 #(true true) #b(1 2 3 4))))
   (line (test-pat 'ok (complex-or-6 #(false false) #b(1 2 3 4))))
   (line (test-pat 'ok (complex-or-6 #(false true) #b(1))))
   (line (test-pat 'ok (complex-or-6 #(true false) #(1))))
   (line (test-pat 'ok (complex-or-6 #(true true) #(1))))

   (line (test-pat 'error (complex-or-6 #(false false) #(1))))

   (line (test-pat 'error (complex-or-6 #(true) #(1 2 3 4))))
   (line (test-pat 'error (complex-or-6 #() #(1 2 3 4))))
   (line (test-pat 'error (complex-or-6 () #(1 2 3 4))))
   (line (test-pat 'error (complex-or-6 () #(1 2 3 4))))
   (line (test-pat 'error (complex-or-6 #(true false) 'klurf)))

   'ok))

(defun complex-or-1 (a b)
  (eif (or (and (< 3 (tuple_size a)) (< (tuple_size a) 9))
       (and (< 2 (tuple_size b)) (< (tuple_size b) 7)))
       'ok 'true 'error))

(defun complex-or-2 (tuple)
  (eif (or (element 1 tuple) (not (> (tuple_size (element 2 tuple)) 3)))
       'ok 'true 'error))

(defun complex-or-3 (a b)
  (eif (or (not (> (size b) 3)) (element 1 a)) 'ok 'true 'error))

(defun complex-or-4 (a b)
  (eif (or (not (and (is_tuple a) (> (size a) 3))) (element 1 b))
       'ok 'true 'error))

(defun complex-or-5 (a b)
  (eif (or (not (and (is_tuple a) (> (size a) 3))) (not (element 1 b)))
       'ok 'true 'error))

(defun complex-or-6 (a b)
  (eif (or (not (and (not (element 1 a)) (not (element 2 a))))
       (not (not (> (size b) 3))))
       'ok 'true 'error))

(defun and_guard
  ([config] (when (is_list config))
   ;; 'and' combinations of literal true/false.

   (line (check (lambda () (eif (and 'true 'false) 'ok 'true 'error)) 'error))
   (line (check (lambda () (eif (and 'false 'true) 'ok 'true 'error)) 'error))
   (line (check (lambda () (eif (and 'true 'true) 'ok)) 'ok))
   (line (check (lambda () (eif (and 'false 'false) 'ok 'true 'error)) 'error))

   (line (check (lambda () (eif (and 'true 'glurf) 'ok 'true 'error)) 'error))
   (line (check (lambda () (eif (and 'glurf 'true) 'ok 'true 'error)) 'error))
   (line (check (lambda () (eif (and 'glurf 'glurf) 'ok 'true 'error)) 'error))

   (line (check (lambda ()
          (let (((tuple 'EXIT (tuple (tuple 'case_clause _) _))
             (catch (eif (and 'true 'false) 'ok
                     (and 'false 'true) 'ok
                     (and 'false 'false) 'ok))))
            'exit))
        'exit))

   (let* ((true (id 'true))
      (false (id 'false))
      (glurf (id 'glurf))
      (atuple (id #(a b c))))

     ;; 'and' combinations of true/false in variables.

     (line (check (lambda () (eif (and true false) 'ok 'true 'error)) 'error))
     (line (check (lambda () (eif (and false true) 'ok 'true 'error)) 'error))
     (line (check (lambda () (eif (and true true) 'ok)) 'ok))
     (line (check (lambda () (eif (and false false) 'ok 'true 'error)) 'error))

     (line (check (lambda ()
            (let (((tuple 'EXIT (tuple (tuple 'case_clause _) _))
               (catch (eif (and true false) 'ok
                       (and false true) 'ok
                       (and false false) 'ok))))
              'exit))
          'exit))

     ;; 'and' combinations of true/false and a non-boolean in variables.

     (line (check (lambda () (eif (and true glurf) 'ok 'true 'error)) 'error))
     (line (check (lambda () (eif (and glurf true) 'ok 'true 'error)) 'error))
     (line (check (lambda () (eif (and glurf glurf) 'ok 'true 'error)) 'error))

     (line (check (lambda ()
            (let (((tuple 'EXIT (tuple (tuple 'case_clause _) _))
               (catch (eif (and 'true 'glurf) 'ok
                       (and 'glurf 'true) 'ok
                       (and 'glurf 'glurf) 'ok))))
              'exit))
          'exit))

     ;; 'and' combinations of true/false with errors.
     (line (check (lambda () (eif (and true (element 42 atuple)) 'ok
                  'true 'error)) 'error))
     (line (check (lambda () (eif (and (element 42 atuple) true) 'ok
                  'true 'error)) 'error))
     (line (check (lambda () (eif (and true true) 'ok)) 'ok))
     (line (check (lambda () (eif (and (element 42 atuple)
                       (element 42 atuple)) 'ok
                       'true 'error)) 'error))
     (line (check (lambda ()
            (let (((tuple 'EXIT (tuple (tuple 'case_clause _) _))
               (catch (eif (and true (element 42 atuple)) 'ok
                       (and (element 42 atuple) true) 'ok
                       (and (element 42 atuple)
                        (element 42 atuple)) 'ok))))
              'exit))
          'exit))

     (line (test-pat 'ok (relprod #(Set a b) #(Set a b)))))
   'ok))

(defun relprod
  ([r1 r2] (when (and (=:= (: erlang size r1) 3)
              (=:= (: erlang element 1 r1) 'Set))
         (and (=:= (: erlang size r2) 3)
              (=:= (: erlang element 1 r2) 'Set)))
   'ok))

(defun xor_guard
  ([config] (when (is_list config))
   ;; 'xor' combinations of literal true/false.
   (line (check (lambda () (eif (xor 'true 'false) 'ok)) 'ok))
   (line (check (lambda () (eif (xor 'false 'true) 'ok)) 'ok))
   (line (check (lambda () (eif (xor 'true 'true) 'ok 'true 'error)) 'error))
   (line (check (lambda () (eif (xor 'false 'false) 'ok 'true 'error)) 'error))

   (line (check (lambda ()
          (let (((tuple 'EXIT (tuple (tuple 'case_clause _) _))
             (catch (eif (xor 'false 'false) 'ok))))
            'exit))
        'exit))
   (line (check (lambda ()
          (let (((tuple 'EXIT (tuple (tuple 'case_clause _) _))
             (catch (eif (xor 'true 'true) 'ok))))
            'exit))
        'exit))

   ;; 'xor' combinations using variables containing true/false.
   (let ((true (id 'true))
     (false (id 'false)))
     (line (check (lambda () (eif (xor true false) 'ok)) 'ok))
     (line (check (lambda () (eif (xor false true) 'ok)) 'ok))
     (line (check (lambda () (eif (xor true true) 'ok 'true 'error)) 'error))
     (line (check (lambda () (eif (xor false false) 'ok 'true 'error)) 'error))

     (line (check (lambda ()
            (let (((tuple 'EXIT (tuple (tuple 'case_clause _) _))
               (catch (eif (xor false false) 'ok))))
              'exit))
          'exit))
     (line (check (lambda ()
            (let (((tuple 'EXIT (tuple (tuple 'case_clause _) _))
               (catch (eif (xor true true) 'ok))))
              'exit))
          'exit))
     )

   'ok))

(defun more_xor_guards
  ([config] (when (is_list config))
   (let ((true (id 'true))
     (false (id 'false))
     (atuple (id #(false true gurka))))

     (line (check (lambda ()
            (eif (xor (element 42 atuple) false) 'ok 'true 'error))
          'error))

     (line (check (lambda ()
            (eif (xor (xor false (element 42 atuple)) false)
             'ok 'true 'error))
          'error))

     (line (check (lambda ()
            (eif (xor (element 18 atuple) (element 42 atuple))
             'ok 'true 'error))
          'error))

     (line (check (lambda ()
            (eif (xor true (element 42 atuple)) 'ok 'true 'error))
          'error))

     (line (check (lambda ()
            (eif (xor (element 42 atuple) true) 'ok 'true 'error))
          'error))

   'ok)))

(defun build_in_guard
  ([config] (when (is_list config))
   (let* ((subbin #b((5.0 float)))
      (b (binary 1 (subbin binary) (3.5 float))))
     (line (test-pat 'ok (eif (=:= b (binary 1 (subbin binary) (3.5 float)))
                  'ok)))
     )))

(defun old_guard_tests
  ([config] (when (is_list config))
   ;; Not relevant in LFE.
   'ok))

(defun gbif
  ([config] (when (is_list config))
   (line (test-pat 'error (gbif-1 1 #(false true))))
   (line (test-pat 'ok (gbif-1 2 #(false true))))
   'ok))

(defun gbif-1
  ([p t] (when (element p t)) 'ok)
  ([_ _] 'error))


(defun t_is_boolean
  ([config] (when (is_list config))

   (line (test-pat 'true (is_boolean 'true)))
   (line (test-pat 'true (is_boolean 'false)))
   (line (test-pat 'true (is_boolean (id 'true))))
   (line (test-pat 'true (is_boolean (id 'false))))

   (line (test-pat 'false (is_boolean 'glurf)))
   (line (test-pat 'false (is_boolean (id 'glurf))))

   (line (test-pat 'false (is_boolean ())))
   (line (test-pat 'false (is_boolean (id ()))))
   (line (test-pat 'false (is_boolean 42)))
   (line (test-pat 'false (is_boolean (id -42))))

   (line (test-pat 'false (is_boolean (: math pi))))
   (line (test-pat 'false (is_boolean 384793478934378924978439789873478934897)))

   (line (test-pat 'false (is_boolean (id (self)))))
   (line (test-pat 'false (is_boolean (id #(x y z)))))
   (line (test-pat 'false (is_boolean (id '(a b c)))))
   (line (test-pat 'false (is_boolean (id (make_ref)))))
   (line (test-pat 'false (is_boolean (id #b(1 2 3)))))
   (line (test-pat 'false (is_boolean (tuple (id 'x) 'y 'z))))
   (line (test-pat 'false (is_boolean (list (id 'a) 'b 'c))))

   (line (test-pat 'ok (bool 'true)))
   (line (test-pat 'ok (bool 'false)))
   (line (test-pat 'ok (bool (id 'true))))
   (line (test-pat 'ok (bool (id 'false))))

   (line (test-pat 'error (bool 'glurf)))
   (line (test-pat 'error (bool (id 'glurf))))

   (line (test-pat 'error (bool ())))
   (line (test-pat 'error (bool (id ()))))
   (line (test-pat 'error (bool 42)))
   (line (test-pat 'error (bool (id -42))))

   (line (test-pat 'error (bool (id (self)))))
   (line (test-pat 'error (bool (id #(x y z)))))
   (line (test-pat 'error (bool (id '(a b c)))))
   (line (test-pat 'error (bool (id (make_ref)))))
   (line (test-pat 'error (bool (id #b(1 2 3)))))

   (line (test-pat 'true (my-is-bool 'true)))
   (line (test-pat 'true (my-is-bool 'false)))
   (line (test-pat 'false (my-is-bool ())))
   (line (test-pat 'false (my-is-bool '(1 2 3 4))))
   (line (test-pat 'false (my-is-bool #(a b c))))

   'ok))

(defun bool
  ([x] (when (is_boolean x)) 'ok)
  ([_] 'error))

(defun my-is-bool (v)
  (let* ((r0 (my-is-bool-a v))
     (res (when (=:= res r0)) (my-is-bool-b v)))
    res))

(defun my-is-bool-a (v)
  (case v
    ('true 'true)
    ('false 'true)
    (_ 'false)))

(defun my-is-bool-b (v)
  (case v
    ('false 'true)
    ('true 'true)
    (_ 'false)))

(defun is_function_2
  ([config] (when (is_list config))
   (line (test-pat 'true (is_function (id (function guard_SUITE all 1)) 1)))
   (line (test-pat 'true (is_function (id (lambda () 'ok)) 0)))
   (line (test-pat 'false (is_function (id (function guard_SUITE all 1)) 0)))
   (line (test-pat 'false (is_function (id (lambda () 'ok)) 1)))

   (let ((F (lambda (_) 'ok)))
     (eif (is_function F 1) 'ok))))

(defun tricky
  ([config] (when (is_list config))
   (line (test-pat 'not_ok (tricky-1 1 2)))
   (line (test-pat 'not_ok (tricky-1 1 'blurf)))
   (line (test-pat 'not_ok (tricky-1 'foo 2)))
   (line (test-pat 'not_ok (tricky-1 'a 'b)))

   (line (test-pat 'error (tricky-2 0.5)))
   (line (test-pat 'error (tricky-2 'a)))
   (line (test-pat 'error (tricky-2 #(a b c))))

   (line (test-pat 'false (rb 100000 '(1) 42)))
   (line (test-pat 'true (rb 100000 '() 42)))
   (line (test-pat 'true (rb 555 '(a b c) 19)))
   'ok))

(defun tricky-1
  ([x y] (when (abs (or (== x 1) (== y 2)))) 'ok)
  ([_ _] 'not_ok))

(defun tricky-2
  ([x] (when (or (float x) (float x))) 'ok)
  ([_] 'error))

;; From dets_v9:read_buckets/11, simplified.

(defun rb
  ([size toread sofar] (when (or (< (+ sofar size) 81920) (== toread ())))
   'true)
  ([_ _ _] 'false))

(defmacro T (op a b)
  `(let* (('ok (eif (,op ,a ,b) 'ok 'true 'error))
      ('ok (eif (not (,op ,a ,b)) 'error 'true 'ok)))
     (funcall
      (lambda (x y true false)
    (let* (('ok (eif (,op x y) 'ok 'true 'error))
           ('ok (eif (or (or false (,op x y)) false) 'ok 'true 'error))
           ('ok (eif (and (,op x y) true) 'ok 'true 'error))
           ('ok (eif (not (,op x y)) 'error 'true 'ok))
           ('ok (eif (or (or false (not (,op x y))) false) 'error
             'true 'ok)))
      'ok)) (id ,a) (id ,b) (id 'true) (id 'false))))

(defmacro F (op a b)
  `(let* (('ok (eif (,op ,a ,b) 'error 'true 'ok))
      ('ok (eif (not (,op ,a ,b)) 'ok 'true 'error)))
     (funcall
      (lambda (x y true false)
    (let* (('ok (eif (,op ,a ,b) 'error 'true 'ok))
           ('ok (eif (or (or false (,op x y)) false) 'error 'true 'ok))
           ('ok (eif (or (not (,op x y)) false) 'ok 'true 'error))
           ('ok (eif (and (not (,op x y)) true) 'ok 'true 'error)))
      'ok)) (id ,a) (id ,b) (id 'true) (id 'false))))

(defun rel_ops
  ([config] (when (is_list config))
   (line (T =/= 1 1.0))
   (line (F =/= 2 2))
   (line (F =/= #(a) #(a)))

   (line (F /= 'a 'a))
   (line (F /= 0 0.0))
   (line (T /= 0 1))
   (line (F /= #(a) #(a)))

   (line (T == 1 1.0))
   (line (F == 'a #()))

   (line (F =:= 1 1.0))
   (line (T =:= 42.0 42.0))

   (line (F > 'a 'b))
   (line (T > 42 1.0))
   (line (F > 42 42.0))

   (line (T < 'a 'b))
   (line (F < 42 1.0))
   (line (F < 42 42.0))

   (line (T =< 1.5 5))
   (line (F =< -9 -100.344))
   (line (T =< 42 42.0))

   (line (T >= 42 42.0))
   (line (F >= 'a 'b))
   (line (T >= 1.0 0))

   ;; Coverage of beam_block:is_exact_eq_ok/1 and collect/1.
   (line (test-pat 'true (/= 'any_atom (id 42))))
   (line (test-pat 'true (/= () (id 42))))

   'ok))

;; -undef(TestOp).


;; Test type tests on literal values. (From emulator test suites.)
(defun literal_type_tests
  ([config] (when (is_list config))
   (case 'guard_suite
     ('guard_suite (literal-type-tests-1 config))
     (_ #(skip "Enough to run this case once.")))))

(defun literal-type-tests-1 (config)
  'ok)
;; literal_type_tests_1(Config) ->
;;     %% Generate an Erlang module with all different type of type tests.
;;     ?line Tests = make_test([{T,L} || T <- type_tests(), L <- literals()] ++
;;                 [{is_function,L1,L2} ||
;;                 L1 <- literals(), L2 <- literals()]),
;;     ?line Mod = literal_test,
;;     ?line Func = {function, 0, test, 0, [{clause,0,[],[],Tests}]},
;;     ?line Form = [{attribute,0,module,Mod},
;;           {attribute,0,compile,export_all},
;;           Func, {eof,0}],

;;     %% Print generated code for inspection.
;;     ?line lists:foreach(fun (F) -> io:put_chars([erl_pp:form(F),"\n"]) end, Form),

;;     %% Test compile:form/1.  This implies full optimization (default).
;;     ?line {ok,Mod,Code1} = compile:forms(Form),
;;     ?line smoke_disasm(Config, Mod, Code1),
;;     ?line {module,Mod} = code:load_binary(Mod, Mod, Code1),
;;     ?line Mod:test(),
;;     ?line true = code:delete(Mod),
;;     ?line code:purge(Mod),

;;     %% Test compile:form/2.  Turn off all optimizations.
;;     ?line {ok,Mod,Code2} = compile:forms(Form, [binary,report,time,
;;                         no_copt,no_postopt]),
;;     ?line smoke_disasm(Config, Mod, Code2),
;;     ?line {module,Mod} = code:load_binary(Mod, Mod, Code2),
;;     ?line Mod:test(),
;;     ?line true = code:delete(Mod),
;;     ?line code:purge(Mod),
;;     ok.

;; make_test([{T,L1,L2}|Ts]) ->
;;     [test(T, L1, L2)|make_test(Ts)];
;; make_test([{T,L}|Ts]) ->
;;     [test(T, L)|make_test(Ts)];
;; make_test([]) -> [].

;; test(T, L) ->
;;     S0 = io_lib:format("begin io:format(\"~~p~~n\", [{~p,~p}]), if ~w(~w) -> true; true -> false end end. ", [T,L,T,L]),
;;     S = lists:flatten(S0),
;;     {ok,Toks,_Line} = erl_scan:string(S),
;;     {ok,E} = erl_parse:parse_exprs(Toks),
;;     {value,Val,_Bs} = erl_eval:exprs(E, []),
;;     {match,0,{atom,0,Val},hd(E)}.

;; test(T, L1, L2) ->
;;     S0 = io_lib:format("begin io:format(\"~~p~~n\", [{~p,~p,~p}]), if ~w(~w, ~w) -> true; true -> false end end. ", [T,L1,L2,T,L1,L2]),
;;     S = lists:flatten(S0),
;;     {ok,Toks,_Line} = erl_scan:string(S),
;;     {ok,E} = erl_parse:parse_exprs(Toks),
;;     {value,Val,_Bs} = erl_eval:exprs(E, []),
;;     {match,0,{atom,0,Val},hd(E)}.

;; smoke_disasm(Config, Mod, Bin) ->
;;     Priv = ?config(priv_dir, Config),
;;     File = filename:join(Priv, atom_to_list(Mod)++".beam"),
;;     ok = file:write_file(File, Bin),
;;     test_lib:smoke_disasm(File).

;; literals() ->
;;     [42,
;;      3.14,
;;      -3,
;;      32982724987789283473473838474,
;;      [],
;;      xxxx,
;;      {a,b,c},
;;      [a,list],
;;      <<1,2,3>>,
;;      <<42:17>>].

;; type_tests() ->
;;     [is_boolean,
;;      is_integer,
;;      is_float,
;;      is_number,
;;      is_atom,
;;      is_list,
;;      is_tuple,
;;      is_pid,
;;      is_reference,
;;      is_port,
;;      is_binary,
;;      is_function].

(defun basic_andalso_orelse
  ([config] (when (is_list config))
   (let ((t (id #(type integers 23 42))))
     (line (test-pat 65 (eif (andalso (=:= (element 1 t) 'type)
                      (=:= (tuple_size t) 4)
                      (=:= (element 2 t) 'integers))
                 (+ (element 3 t) (element 4 t))
                 'true 'error)))
     (line (test-pat 65 (case ()
              (() (andalso (=:= (element 1 t) 'type)
                       (=:= (tuple_size t) 4)
                       (=:= (element 2 t) 'integers))
               (+ (element 3 t) (element 4 t))))))
     (line (test-pat 42 (basic-rt #(type integers 40 2))))
     (line (test-pat 5.0 (basic-rt #(vector #(3.0 4.0)))))
     (line (test-pat 20 (basic-rt '(+ 3 7))))
     (line (test-pat (tuple 'Set 'a 'b) (basic-rt #(#(Set a b) #(Set a b)))))
     (line (test-pat 12 (basic-rt #(klurf 4))))

     (line (test-pat 'error (basic-rt #(type integers 40 2 3))))
     (line (test-pat 'error (basic-rt #(kalle integers 40 2))))
     (line (test-pat 'error (basic-rt #(kalle integers 40 2))))
     (line (test-pat 'error (basic-rt #(1 2))))
     (line (test-pat 'error (basic-rt ())))

     (let ((rel-prod-body (lambda (r1 r2)
                (eif (andalso (=:= (: erlang size r1) 3)
                      (=:= (: erlang element 1 r1) 'Set)
                      (=:= (: erlang size r2) 3)
                      (=:= (: erlang element 1 r2) 'Set))
                 'ok))))
       (line (test-pat 'ok (funcall rel-prod-body #(Set a b) #(Set a b)))))

     ;; 'andalso'/'orelse' with calls known to fail already at compile time.
     ;; Used to crash the code generator.
     (let (('error (funcall (lambda ()
                  (let ((r #(vars true)))
                (eif (andalso (is_record r 'vars 2)
                          (element 99 r))
                     'ok
                     'true 'error)))
                ))
       ('error (funcall (lambda (x)
                  (let ((l #(a b c)))
                (eif (andalso (is_list x)
                          (> (length l) 4))
                     'ok
                     'true 'error)))
                ())))
       ())

     'ok)))

(defun basic-rt
  ([t] (when (andalso (is_tuple t) (=:= (tuple_size t) 4)
              (=:= (element 1 t) 'type) (== (element 2 t) 'integers)))
   (+ (element 3 t) (element 4 t)))
  ([t] (when (andalso (is_tuple t) (=:= (tuple_size t) 2)
              (=:= (element 1 t) 'vector)))
   (let (((tuple x y) (element 2 t)))
     (eif (progn (is_float x) (is_float y)) (: math sqrt (+ (* x x) (* y y))))
     ))
  ([(list '+ a b)]
    (* (id (+ a b)) 2))
  ([(tuple r1 r2)] (when (andalso (=:= (: erlang size r1) 3)
                  (=:= (: erlang element 1 r1) 'Set)
                  (=:= (: erlang size r2) 3)
                  (=:= (: erlang element 1 r2) 'Set)))
   (let* ((r1 (id r1))
      (r2 (id r2)))
     r1))
  ([t] (when (andalso (is_tuple t) (=:= (tuple_size t) 2)
              (=:= (element 1 t) 'klurf)))
   (* 3 (id (element 2 t))))
  ([_] 'error))

(defun traverse_dcd
  ([config] (when (is_list config))
   (let* ((l0 '(#(log_header dcd_log "1.0" a b c)
        #(log_header dcd_log "2.0" a b c)
        #(log_header dcd_log "0.0" a b c) blurf))
      (#(cont (#(log_header dcd_log "0.0" a b c) blurf) log funny)
       (traverse-dcd (tuple 'cont l0) 'log 'funny))
      (l1 '(#(log_header dcd_log "1.0")))
      ((tuple 'cont l1 'log 'funny)
       (traverse-dcd (tuple 'cont l1) 'log 'funny))
      (l2 '(#(a tuple)))
      ((tuple 'cont l2 'log 'funny)
       (traverse-dcd (tuple 'cont l2) 'log 'funny)))
     'ok)))

;; The function starts out with 3 arguments in {x,0}, {x,1}, {x,2}.
;; The outer match of a two tuple will places the first element in
;; {x,3} and second in {x,4}. The guard for the first clause must make
;; ensure that all of those registers are restored before entering the
;; second clause.
;;
;; (From mnesia_checkpoint.erl, modified.)

(defun traverse-dcd
  ([(tuple cont (cons logh rest)) log fun]
   (when (andalso (is_tuple logh) (=:= (tuple_size logh) 6)
          (=:= (element 1 logh) 'log_header)
          (== (element 2 logh) 'dcd_log))
     (andalso (is_tuple logh) (=:= (tuple_size logh) 6)
          (=:= (element 1 logh) 'log_header)
          (>= (element 3 logh) '"1.0")))
   (traverse-dcd (tuple cont rest) log fun))
  ([(tuple cont recs) log fun]
   (tuple cont recs log fun)))

(defun check_qlc_hrl
  ([config] (when (is_list config))
   (let ((st #(r1 false dum)))
     (line (test-pat 'foo (cqlc 'qlc 'q '(#(lc 1 2 3)) st)))
     (line (test-pat 'foo (cqlc 'qlc 'q '(#(lc 1 2 3) b) st)))
     (line (test-pat st (cqlc 'qlc 'q '() st)))
     (line (test-pat st (cqlc 'qlc 'blurf '(#(lc 1 2 3) b) st)))
     (line (test-pat st (cqlc 'q 'q '(#(lc 1 2 3) b) st)))
     (line (test-pat st (cqlc 'qlc 'q '(#(lc 1 2 3) b c) st)))
     (line (test-pat st (cqlc 'qlc 'q '(a b) st)))
     (line (test-pat (tuple 'r1 'true 'kalle)
             (cqlc 'qlc 'q '(#(lc 1 2 3) b) #(r1 true kalle))))

     'ok)))

;; From erl_lint.erl; original name was check_qlc_hrl/4.
(defun cqlc (m f as st)
  (let ((arity (length as)))
    (case as
      ((cons (tuple 'lc _ _ _) _)
       (when (=:= m 'qlc) (=:= f 'q) (< arity 3)
         (not (and (orelse (=:= (element 1 st) 'r1) 'fail)
               (and (=:= (tuple_size st) 3)
                (element 2 st)))))
       'foo)
      (_ st))))

;; OTP-7679: Thanks to Hunter Morris. (almost anyway)
(defun andalso_semi
  ([config] (when (is_list config))
   (line (test-pat 'ok (andalso-semi-foo 0)))
   (line (test-pat 'ok (andalso-semi-foo 1)))
   (line (fc (catch (andalso-semi-foo 2))))

   (line (test-pat 'ok (andalso-semi-bar '(a b c))))
   (line (test-pat 'ok (andalso-semi-bar 1)))
   (line (fc (catch (andalso-semi-bar '(a b)))))
   'ok))

(defun andalso-semi-foo
  ([bar] (when (or (andalso (is_integer bar) (=:= bar 0)) (=:= bar 1)))
   'ok))

(defun andalso-semi-bar
  ([bar] (when (or (andalso (is_list bar) (=:= (length bar) 3)) (=:= bar 1)))
   'ok))

(defun t_tuple_size                ;Cannot redefine tuple_size
  ([config] (when (is_list config))
   (line (test-pat 10 (do-tuple-size #(1 2 3 4))))
   (line (fc (catch (do-tuple-size #(1 2 3)))))
   (line (fc (catch (do-tuple-size 42))))

   (line (test-pat 'error (ludicrous-tuple-size #(a b c))))
   (line (test-pat 'error (ludicrous-tuple-size '(a b c))))

   ;; Test the "unsafe case" - the register assigned the tuple size is
   ;; not killed.
   ;; Compile case not relevant for LFE.
   'ok))

(defun do-tuple-size
  ([t] (when (=:= (tuple_size t) 4))
   (let (((tuple a b c d) t))
     (+ a b c d))))

(defun ludicrous-tuple-size
  ([t] (when (=:= (tuple_size t) #x7777777777777777777777777777777777)) 'ok)
  ([t] (when (=:= (tuple_size t) #x10000000000000000)) 'ok)
  ([t] (when (=:= (tuple_size t) (- (bsl 1 64) 1))) 'ok)
  ([t] (when (=:= (tuple_size t) #xFFFFFFFFFFFFFFFF)) 'ok)
  ([_] 'error))

(defmacro MASK-ERROR (e) `(mask-error (catch ,e)))
(defun mask-error
  ([(tuple 'EXIT (tuple err _))] err)
  ([else] else))

(defun binary_part
  (['doc] '"Tests the binary_part/2,3 guard (GC) bif's")
  ([config] (when (is_list config))
   ;; This is more or less a copy of what the guard_SUITE in emulator
   ;; does to cover the guard bif's
   (line (test-pat 1 (bp-test #b(1 2 3))))
   (line (test-pat 2 (bp-test #b(2 1 3))))
   (line (test-pat 'error (bp-test #b(1))))
   (line (test-pat 'error (bp-test #b())))
   (line (test-pat 'error (bp-test 'apa)))
   (line (test-pat 3 (bp-test #b(2 3 3))))

   ;; With one variable (pos)
   (line (test-pat 1 (bp-test #b(1 2 3) 1)))
   (line (test-pat 2 (bp-test #b(2 1 3) 1)))
   (line (test-pat 'error (bp-test #b(1) 1)))
   (line (test-pat 'error (bp-test #b() 1)))
   (line (test-pat 'error (bp-test 'apa 1)))
   (line (test-pat 3 (bp-test #b(2 3 3) 1)))

   ;; With one variable (length)
   (line (test-pat 1 (bp-test-y #b(1 2 3) 1)))
   (line (test-pat 2 (bp-test-y #b(2 1 3) 1)))
   (line (test-pat 'error (bp-test-y #b(1) 1)))
   (line (test-pat 'error (bp-test-y #b() 1)))
   (line (test-pat 'error (bp-test-y 'apa 1)))
   (line (test-pat 3 (bp-test-y #b(2 3 3) 2)))

   ;; With one variable (whole tuple)
   (line (test-pat 1 (bp-test-x #b(1 2 3) #(1 1))))
   (line (test-pat 2 (bp-test-x #b(2 1 3) #(1 1))))
   (line (test-pat 'error (bp-test-x #b(1) #(1 1))))
   (line (test-pat 'error (bp-test-x #b() #(1 1))))
   (line (test-pat 'error (bp-test-x 'apa #(1 1))))
   (line (test-pat 3 (bp-test-x #b(2 3 3) #(1 2))))

   ;; With two variables
   (line (test-pat 1 (bp-test #b(1 2 3) 1 1)))
   (line (test-pat 2 (bp-test #b(2 1 3) 1 1)))
   (line (test-pat 'error (bp-test #b(1) 1 1)))
   (line (test-pat 'error (bp-test #b() 1 1)))
   (line (test-pat 'error (bp-test 'apa 1 1)))
   (line (test-pat 3 (bp-test #b(2 3 3) 1 2)))

   ;; Direct (autoimported) call, these will be evaluated by the compiler...
   (line (test-pat #b(2) (binary_part #b(1 2 3) 1 1)))
   (line (test-pat #b(1) (binary_part #b(2 1 3) 1 1)))

   ;; Compiler warnings due to constant evaluation expected (3)
   (line (test-pat 'badarg (MASK-ERROR (binary_part #b(1) 1 1))))
   (line (test-pat 'badarg (MASK-ERROR (binary_part #b() 1 1))))
   (line (test-pat 'badarg (MASK-ERROR (binary_part 'apa 1 1))))
   (line (test-pat #b(3 3) (binary_part #b(2 3 3) 1 2)))

   ;; Direct call through apply
   (line (test-pat #b(2) (apply 'erlang 'binary_part '(#b(1 2 3) 1 1))))
   (line (test-pat #b(1) (apply 'erlang 'binary_part '(#b(2 1 3) 1 1))))

   ;; Constant propagation
   (let ((bin #b(1 2 3)))
     (line (test-pat 'ok (eif (=:= (binary_part bin 1 1) #b(2)) 'ok
                  ;; Compiler warning, clause cannot match (expected)
                  'true 'error)))
     (line (test-pat 'ok (eif (=:= (binary_part bin #(1 1)) #b(2)) 'ok
                  ;; Compiler warning, clause cannot match (expected)
                  'true 'error))))

   'ok))

(defun bp-test
  ([b] (when (=:= (length b) 137)) 1)
  ([b] (when (=:= (binary_part b #(1 1)) #b(2))) 1)
  ([b] (when (=:= (: erlang binary_part b 1 1) #b(1))) 2)
  ([b] (when (=:= (: erlang binary_part b #(1 2)) #b(3 3))) 3)
  ([_] 'error))

(defun bp-test
  ([b a] (when (=:= (length b) a)) 1)
  ([b a] (when (=:= (binary_part b (tuple a 1)) #b(2))) 1)
  ([b a] (when (=:= (: erlang binary_part b a 1) #b(1))) 2)
  ([b a] (when (=:= (: erlang binary_part b (tuple a 2)) #b(3 3))) 3)
  ([_ _] 'error))

(defun bp-test-x
  ([b a] (when (=:= (length b) a)) 1)
  ([b a] (when (=:= (binary_part b a ) #b(2))) 1)
  ([b a] (when (=:= (: erlang binary_part b a) #b(1))) 2)
  ([b a] (when (=:= (: erlang binary_part b a) #b(3 3))) 3)
  ([_ _] 'error))

(defun bp-test-y
  ([b a] (when (=:= (length b) a)) 1)
  ([b a] (when (=:= (binary_part b (tuple 1 a)) #b(2))) 1)
  ([b a] (when (=:= (: erlang binary_part b 1 a) #b(1))) 2)
  ([b a] (when (=:= (: erlang binary_part b (tuple 1 a)) #b(3 3))) 3)
  ([_ _] 'error))

(defun bp-test
  ([b a _] (when (=:= (length b) a)) 1)

  ([b a c] (when (=:= (binary_part b (tuple a c)) #b(2))) 1)
  ([b a c] (when (=:= (: erlang binary_part b a c) #b(1))) 2)
  ([b a c] (when (=:= (: erlang binary_part b (tuple a c)) #b(3 3))) 3)
  ([_ _ _] 'error))

;; Call this function to turn off constant propagation.
(defun id (i) i)

(defun check (f result)
  (case (funcall f)
    (r (when (=:= r result)) 'ok)
    (other
     (: lfe_io format '"Expected: ~p\n" (list result))
     (: lfe_io format '"     Got: ~p\n" (list other))
     (: test_server fail))))

(defun fc
  ([(tuple 'EXIT (tuple 'function_clause _))] 'ok)
  ([(tuple 'EXIT (tuple (tuple 'case_clause _) _))] 'ok))

;;  ([(tuple 'EXIT (tuple (tuple 'case_clause _) _))]
;;   (when (=:= 'guard_SUITE 'guard_inline_SUITE)) 'ok))
