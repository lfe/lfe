;; Copyright (c) 2016 Eric Bailey
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

;; File    : clj_SUITE.lfe
;; Author  : Eric Bailey
;; Purpose : Test clj exports.

(include-file "test_server.lfe")

(defmodule clj_SUITE
  "Test clj exports."
  (export (all 0)))

(defmacro is* (expr) `(let (('true ,expr)) ,expr))

(defmacro is (expr) `(line (is* ,expr)))

(defmacro is-not (expr) `(is-equal 'false ,expr))

(defmacro is-equal (lhs rhs) `(line (is* (=:= ,lhs ,rhs))))

(defmacro deftest
  (`(,name . ,body)
   `(progn (defun ,name (_config) ,@body)
           (extend-module () ((export (,name 1)))))))

(defun all ()
  '(identity
    ;; clj-com-tests
    comp partial -> ->>
    ;; clj-p-tests
    string? list? tuple? atom? dict? set? proplist? proplist-kv? undef? nil?
    true? false?
    ;; in? not-in? test-in-with-guard in?-guard not-in?-guard
    identical? empty? every? any? not-any? element?
    ;; clj-seq-tests
    seq drop take next-and-take range split-at partition interleave
    get-in-nth get-in-keys reduce repeat))

(deftest identity
  (lc ((<- x '(atom
               0 42
               0.0 3.14
               #\c
               "" "abc"
               #"" #"abc"
               () (1 2)
               #() #(1 2)
               ;; FIXME: only test maps on 17
               ;; #m() #m(a 1 b 2)
               )))
    (is-equal (clj:identity x) x))
  (is-equal 3 (clj:identity (+ 1 2)))
  (is (clj:identity (> 5 0))))


;;; clj-comp-tests

(deftest comp
  (flet ((c0 (x) (funcall (clj:comp) x)))
    (lc ((<- x '(atom
                 0 42
                 0.0 3.14
                 #\c
                 "" "abc"
                 #"" #"abc"
                 () (1 2)
                 #() #(1 2)
                 ;; FIXME: only test maps on 17
                 ;; #m() #m(a 1 b 2)
                 )))
      (is-equal (clj:identity x) (c0 x)))
    (is-equal (clj:identity (+ 1 2 3)) (c0 6))
    (is-equal (clj:identity (quote foo)) (c0 'foo)))
  (let ((asin-result (funcall (clj:comp #'math:sin/1 #'math:asin/1) 0.5)))
    (is-equal "0.5" (car (io_lib:format "~.1f" `(,asin-result)))))
  (is-equal 1.5
            (funcall (clj:comp `(,(lambda (x) (+ x 1))
                                 ,#'math:sin/1
                                 ,#'math:asin/1)) 0.5))
  (is-equal '(1 2 3 4)
            (lists:filter (clj:comp #'not/1 #'clj:zero?/1)
              '(0 1 0 2 0 3 0 4)))
  (let ((asin-result (clj:comp #'math:sin/1 #'math:asin/1 0.5)))
    (is-equal "0.5" (car (io_lib:format "~.1f" `(,asin-result))))))

(deftest partial
  (flet (;; (p0 (x) (funcall (clj:partial inc) x))
         (p1 (x) (funcall (clj:partial #'+/2 20) x))
         ;; (p2 (x) (funcall (clj:partial conj #(1 2)) x))
         )
    ;; (is-equal 41 (p0 40))
    (is-equal 40 (p1 20))
    ;; (is-equal #(1 2 3) (p2 3))
    (is-equal 3 (funcall (clj:partial #'+/2 1) 2))
    ;; FIXME: exception error: #(unbound_func #(+ 3))
    ;; (is-equal 6 (funcall (clj:partial #'+/3 1) '(2 3)))
    ))

(deftest ->
  (is-equal '(#(a 1) #(b 2) #(c 3) #(d 4) #(e 5) #(f 6))
            (clj:-> '(#(a 1) #(b 2) #(c 3))
                    (++ '(#(d 4)))
                    (++ '(#(e 5)))
                    (++ '(#(f 6)))))
  (is-equal '("L" "F" "E")
            (clj:-> "a b c d e"
                    (string:to_upper)
                    (string:tokens " ")
                    (lists:merge '("X" "F" "L"))
                    (lists:sort)
                    (lists:reverse)
                    (lists:sublist 2 3))))

(deftest ->>
  (is-equal '(#(f 6) #(e 5) #(d 4) #(a 1) #(b 2) #(c 3))
            (clj:->> '(#(a 1) #(b 2) #(c 3))
                     (++ '(#(d 4)))
                     (++ '(#(e 5)))
                     (++ '(#(f 6)))))
  (is-equal 1540.0
            (clj:->> (clj:seq 42)
                     (lists:map (lambda (x) (math:pow x 2)))
                     (lists:filter (clj:comp #'clj:even?/1 #'round/1))
                     (clj:take 10)
                     (lists:foldl #'+/2 0))))

;;; clj-p-tests

(deftest string?
  (is (clj:string? "string data! yaya!"))
  (is-not (clj:string? (list "my" "string" "data"))))

;; XXX add a unit test for (unicode? ...)

(deftest list?
  (is-not (clj:list? "string data! yaya!"))
  (is (clj:list? (list "my" "string" "data"))))

(deftest tuple?
  (is-not (clj:tuple? "string data! yaya!"))
  (is (clj:tuple? (tuple "my" "string" "data"))))

(deftest atom?
  (is-not (clj:atom? "string data! yaya!"))
  (is (clj:atom? 'my-atom))
  (is (clj:atom? '|more atom data|)))

(deftest dict?
  (is-not (clj:dict? "a string"))
  (is-not (clj:dict? '("a" "list")))
  (is-not (clj:dict? #b("a binary")))
  (is-not (clj:dict? #("a" "tuple")))
  (is-not (clj:dict? '(#("a" "tuple"))))
  (is (clj:dict? (dict:from_list '(#("a" "tuple"))))))

(deftest set?
  (is-not (clj:set? '(c a b)))
  (is (clj:set? '(a b c)))
  (is (clj:set? '()))
  (is (clj:set? (sets:new)))
  (is (clj:set? (ordsets:new))))

(deftest proplist?
  (is-not (clj:proplist? 1))
  (is-not (clj:proplist? '(1)))
  (is-not (clj:proplist? '(1 2)))
  (is-not (clj:proplist? '((1 2))))
  (is-not (clj:proplist? '(#(1 2))))
  (is-not (clj:proplist? '(#(a 1) #(2 b) #(c 3))))
  (is (clj:proplist? '(a)))
  (is (clj:proplist? '(a b c)))
  (is (clj:proplist? '(#(a 1) b c)))
  (is (clj:proplist? '(#(a 1) #(b 2) c)))
  (is (clj:proplist? '(#(a 1) #(b 2) #(c 3)))))

(deftest proplist-kv?
  (is (clj:proplist-kv? 'a))
  (is-not (clj:proplist-kv? "a"))
  (is-not (clj:proplist-kv? 1))
  (is (clj:proplist-kv? '#(a b)))
  (is-not (clj:proplist-kv? '(a b))))

(deftest undef?
  (is-not (clj:undef? 42))
  (is-not (clj:undef? 'undef))
  (is (clj:undef? 'undefined)))

(deftest nil?
  (is-not (clj:nil? 32))
  (is-not (clj:nil? 'undefined))
  (is (clj:nil? 'nil))
  (is (clj:nil? '())))

(deftest true?
  (is-not (clj:true? 'false))
  (is (clj:true? 'true)))

(deftest false?
  (is-not (clj:false? 'true))
  (is (clj:false? 'false)))

#|
(deftest in?
  (is-not (in? 0 '(1 2 3 4 5 6)))
  (is (in? 6 '(1 2 3 4 5 6)))
  (is-not (in? "z" '("a" "b" "c" "d" "e")))
  (is (in? "e" '("a" "b" "c" "d" "e")))
  (is-not (in? 'z '(a b c d e)))
  (is (in? 'e '(a b c d e))))

(deftest not-in?
  (is (not-in? 0 '(1 2 3 4 5 6)))
  (is-not (not-in? 6 '(1 2 3 4 5 6)))
  (is (not-in? "z" '("a" "b" "c" "d" "e")))
  (is-not (not-in? "e" '("a" "b" "c" "d" "e")))
  (is (not-in? 'z '(a b c d e)))
  (is-not (not-in? 'e '(a b c d e))))

(defun test-in-with-guard
  ((arg) (when (in? arg '(a b c)))
   'found)
  ((_) 'not-found))

(deftest in?-guard
  (is-equal (test-in-with-guard 'a) 'found)
  (is-equal (test-in-with-guard 'b) 'found)
  (is-equal (test-in-with-guard 'c) 'found)
  (is-equal (test-in-with-guard 'd) 'not-found))

(defun test-not-in-with-guard
  ((arg) (when (not-in? arg '(i j k)))
   'not-found)
  ((_) 'found))

(deftest not-in?-guard
  (is-equal (test-not-in-with-guard 'i) 'found)
  (is-equal (test-not-in-with-guard 'j) 'found)
  (is-equal (test-not-in-with-guard 'k) 'found)
  (is-equal (test-not-in-with-guard 'a) 'not-found))
|#

(deftest identical?
  (is (clj:identical? '(a b c) '(a b c)))
  (is-not (clj:identical? '(a b c) '(a b d))))

(deftest empty?
  (is (clj:empty? '()))
  (is-not (clj:empty? '(1 2 3))))

(deftest every?
  (is (clj:every? #'clj:zero?/1 '(0 0 0 0 0)))
  (is-not (clj:every? #'clj:zero?/1 '(0 0 0 0 1))))

(deftest any?
  (is (clj:any? #'clj:zero?/1 '(0 1 1 1 1)))
  (is-not (clj:any? #'clj:zero?/1 '(1 1 1 1 1))))

(deftest not-any?
  (is-not (clj:not-any? #'clj:zero?/1 '(0 1 1 1 1)))
  (is (clj:not-any? #'clj:zero?/1 '(1 1 1 1 1))))

(deftest element?
  (is (clj:element? 'a '(a b c)))
  (is-not (clj:element? 'z '(a b c)))
  (is (clj:element? 'a (sets:from_list '(a b c))))
  (is-not (clj:element? 'z (sets:from_list '(a b c))))
  (is (clj:element? 'a (ordsets:from_list '(a b c))))
  (is-not (clj:element? 'z (ordsets:from_list '(a b c)))))


;; clj-seq-tests

(deftest seq
  (is-equal '(1 2 3 4) (clj:seq 4))
  (is-equal '(2 3 4) (clj:seq 2 4))
  (is-equal '(2 4 6 8 10) (clj:seq 2 10 2)))

(deftest drop
  (is-equal '(6 7 8 9 10 11 12) (clj:drop 5 '(1 2 3 4 5 6 7 8 9 10 11 12)))
  (is-equal '() (clj:drop 'all '(1 2 3 4 5 6 7 8 9 10 11 12))))

(deftest take
  (is-equal '(1 2 3 4) (clj:take 4 (clj:range)))
  (is-equal '(1 2 3 4 5) (clj:take 5 '(1 2 3 4 5 6 7 8 9 10 11 12)))
  ;; FIXME
  ;; (is-error function_cluase (clj:take -1 (clj:range)))
  (is-equal '(1 2 3 4 5 6 7 8 9 10 11 12)
            (clj:take 'all '(1 2 3 4 5 6 7 8 9 10 11 12))))

(deftest next-and-take
  (is-equal
   '(1 6 21 66 201 606 1821 5466 16401 49206)
   (clj:take 10 (clj:next (lambda (x y) (* 3 (+ x y))) 1 1)))
  (is-equal
   '(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536)
   (clj:take 17 (clj:next (lambda (x _) (* 2 x)) 1 1)))
  (is-equal
   '(1 4.0 25.0 676.0 458329.0 210066388900.0 4.4127887745906175e22)
   (clj:take 7 (clj:next (lambda (x _) (math:pow (+ x 1) 2)) 1 1))))

(deftest range
  (is-equal 1 (car (funcall (clj:range))))
  (is-equal 2 (car (funcall (cdr (funcall (clj:range))))))
  (is-equal 3 (car (funcall (cdr (funcall (cdr (funcall (clj:range))))))))
  (is-equal 4 (clj:->> (funcall (clj:range))
                       (cdr) (funcall) (cdr) (funcall) (cdr) (funcall) (car))))

(deftest split-at
  (is-equal
   #((1 2 3) (4 5 6 7 8 9 10 11 12))
   (clj:split-at 3 '(1 2 3 4 5 6 7 8 9 10 11 12))))

(deftest partition
  (is-equal
   '()
   (clj:partition 0 '()))
  (is-equal
   '()
   (clj:partition 1 '()))
  (is-equal
   '()
   (clj:partition 100 '()))
  (is-equal
   '(1 2 3 4 5 6 7 8 9 10 11 12)
   (clj:partition 0 '(1 2 3 4 5 6 7 8 9 10 11 12)))
  (is-equal
   '((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12))
   (clj:partition 1 '(1 2 3 4 5 6 7 8 9 10 11 12)))
  (is-equal
   '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12))
   (clj:partition 2 '(1 2 3 4 5 6 7 8 9 10 11 12)))
  (is-equal
   '((1 2 3 4 5) (6 7 8 9 10) (11 12))
   (clj:partition 5 '(1 2 3 4 5 6 7 8 9 10 11 12)))
  (is-equal
   '((1 2 3 4 5 6 7) (8 9 10 11 12))
   (clj:partition 7 '(1 2 3 4 5 6 7 8 9 10 11 12)))
  (is-equal
   '((1 2 3 4 5 6 7 8 9 10 11) (12))
   (clj:partition 11 '(1 2 3 4 5 6 7 8 9 10 11 12)))
  (is-equal
   '((1 2 3 4 5 6 7 8 9 10 11 12))
   (clj:partition 12 '(1 2 3 4 5 6 7 8 9 10 11 12))))

(deftest interleave
  (is-equal '(a 1 b 2 c 3) (clj:interleave '(a b c) '(1 2 3))))

(defun get-in-data-1 ()
  '((1)
    (1 2 3)
    (1 2 (3 4 (5 6 (7 8 9))))))

(deftest get-in-nth
  (is-equal 1 (clj:get-in (get-in-data-1) '(1 1)))
  (is-equal 3 (clj:get-in (get-in-data-1) '(2 3)))
  (is-equal 9 (clj:get-in (get-in-data-1) '(3 3 3 3 3)))
  (is-equal 'undefined (clj:get-in (get-in-data-1) '(4)))
  (is-equal 'undefined (clj:get-in (get-in-data-1) '(4 3 3 3))))

(defun get-in-data-2 ()
  '(#(key-1 val-1)
    #(key-2 val-2)
    #(key-3 (#(key-4 val-4)
             #(key-5 val-5)
             #(key-6 (#(key-7 val-7)
                      #(key-8 val-8)
                      #(key-9 val-9)))))))

(deftest get-in-keys
  (is-equal 'val-1 (clj:get-in (get-in-data-2) '(key-1)))
  (is-equal 'val-5 (clj:get-in (get-in-data-2) '(key-3 key-5) ))
  (is-equal 'val-9 (clj:get-in (get-in-data-2) '(key-3 key-6 key-9)))
  (is-equal 'undefined (clj:get-in (get-in-data-2) '(key-18)))
  (is-equal 'undefined (clj:get-in (get-in-data-2) '(key-3 key-6 key-89)))
  (is-equal 'undefined
            (clj:get-in (get-in-data-2) '(key-3 key-6 key-89 key-100))))

(deftest reduce
  (is-equal 6 (clj:reduce (lambda (x acc) (+ x acc)) '(1 2 3)))
  (is-equal 6 (clj:reduce #'+/2 '(1 2 3)))
  (is-equal 6 (clj:reduce (fun + 2) '(1 2 3)))
  (is-equal 6 (clj:reduce (lambda (x acc) (+ x acc)) 0 '(1 2 3)))
  (is-equal 6 (clj:reduce #'+/2 0 '(1 2 3)))
  (is-equal 6 (clj:reduce (fun + 2) 0 '(1 2 3))))

(deftest repeat
  (is-equal '(1 1 1) (clj:repeat 3 1))
  (is-equal '("xo" "xo") (clj:repeat 2 "xo"))
  (is-equal '() (clj:repeat 0 "oh noes"))
  (is-equal '(ok ok ok ok) (clj:repeat 4 'ok))
  ;; FIXME:
  ;; (is-error 'function_clause (clj:repeat -1 0))
  (is-equal '(1 1 1) (clj:repeat 3 (lambda () 1)))
  ;; FIXME:
  ;; (is-error 'function_clause (clj:repeat -1 (lambda () 1)))
  (is-equal 2 (length (clj:repeat 2 #'random:uniform/0))))


(defun check (f result)
  (case (funcall f)
    (r (when (=:= r result)) 'ok)
    (other
     (lfe_io:format "Expected: ~p\n" (list result))
     (lfe_io:format "     Got: ~p\n" (list other))
     (test_server:fail))))
