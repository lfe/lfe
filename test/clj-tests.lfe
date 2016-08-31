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

;; File    : clj-tests.lfe
;; Author  : Eric Bailey
;; Purpose : Test clj exports.


(defmodule clj-tests
  "Test clj exports.")

(include-file "ltest-macros.lfe")

;;; HACK

(defmacro IFF-MAPS expr `(andalso (erl_internal:bif 'is_map 1) ,@expr))

;;; identity and constantly

(deftest identity
  (are* [x] (=:= (clj:identity x) x)
        'atom
        0 42
        0.0 3.14
        #\c
        "" "abc"
        #"" #"abc"
        () (1 2)
        #() #(1 2)
        (IFF-MAPS (call 'maps 'new))
        (IFF-MAPS (call 'maps 'from_list '[#(a 1) #(b 2)])))
  (is-equal 3 (clj:identity (+ 1 2)))
  (is (clj:identity (> 5 0))))

(deftest constantly
  (flet ((c0 (x) (funcall (clj:constantly 10) x)))
    (are* [x] (=:= 10 (c0 x))
          'nil
          42
          "foo")))


;;; clj-comp-tests

(deftest comp
  (flet ((c0 (x) (funcall (clj:comp) x)))
    (are* [x] (=:= (clj:identity x) (c0 x))
          'atom
          0 42
          0.0 3.14
          #\c
          "" "abc"
          #"" #"abc"
          () (1 2)
          #() #(1 2)
          (IFF-MAPS (call 'maps 'new))
          (IFF-MAPS (call 'maps 'from_list '(#(a 1) #(b 2)))))
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
         (p1 (x) (funcall (clj:partial (fun + 2) 20) x))
         ;; (p2 (x) (funcall (clj:partial conj #(1 2)) x))
         )
    ;; (is-equal 41 (p0 40))
    (is-equal 40 (p1 20))
    ;; (is-equal #(1 2 3) (p2 3))
    (is-equal 3 (funcall (clj:partial (fun + 2) 1) 2))
    (is-equal 6 (funcall (clj:partial (fun + 3) 1) '(2 3)))))

(deftest ->
  (are* [x y] (=:= x y)

        '(#(a 1) #(b 2) #(c 3) #(d 4) #(e 5) #(f 6))
        (clj:-> '(#(a 1) #(b 2) #(c 3))
                (++ '(#(d 4)))
                (++ '(#(e 5)))
                (++ '(#(f 6))))

        '("L" "F" "E")
        (clj:-> "a b c d e"
                (string:to_upper)
                (string:tokens " ")
                (lists:merge '("X" "F" "L"))
                (lists:sort)
                (lists:reverse)
                (lists:sublist 2 3))))

(deftest ->>
  (are* [x y] (=:= x y)
        '(#(f 6) #(e 5) #(d 4) #(a 1) #(b 2) #(c 3))
        (clj:->> '(#(a 1) #(b 2) #(c 3))
                 (++ '(#(d 4)))
                 (++ '(#(e 5)))
                 (++ '(#(f 6))))
        1540.0
        (clj:->> (clj:seq 42)
                 (lists:map (lambda (x) (math:pow x 2)))
                 (lists:filter (clj:comp #'clj:even?/1 #'round/1))
                 (clj:take 10)
                 (lists:foldl (fun + 2) 0))))

;;; clj-p-tests

(deftest string?
  (is-not (clj:string? (list "my" "string" "data")))
  (is (clj:string? "string data! yaya!")))

;; TODO: add a unit test for (unicode? ...)

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
  (are* [x] (not (clj:dict? x))
        "a string"
        '("a" "list")
        #b("a binary")
        #("a" "tuple")
        '(#("a" "tuple")))
  (is (clj:dict? (dict:from_list '(#("a" "tuple"))))))

(deftest set?
  (is-not (clj:set? '(c a b)))
  (are* [x] (clj:set? x)
        '(a b c)
        ()
        (sets:new)
        (ordsets:new)))

(deftest proplist?
  (are* [x] (not (clj:proplist? x))
        1
        '(1)
        '(1 2)
        '((1 2))
        '(#(1 2))
        '(#(a 1) #(2 b) #(c 3)))
  (are* [x] (clj:proplist? x)
        '(a)
        '(a b c)
        '(#(a 1) b c)
        '(#(a 1) #(b 2) c)
        '(#(a 1) #(b 2) #(c 3))))

(deftest proplist-kv?
  (are* [x] (not (clj:proplist-kv? x))
        "a"
        1
        '(a b))
  (is (clj:proplist-kv? 'a))
  (is (clj:proplist-kv? '#(a b))))

(deftest undef?
  (is-not (clj:undef? 42))
  (is-not (clj:undef? 'undef))
  (is (clj:undef? 'undefined)))

(deftest nil?
  (is-not (clj:nil? 32))
  (is-not (clj:nil? 'undefined))
  (is (clj:nil? 'nil))
  (is (clj:nil? ())))

(deftest true?
  (is-not (clj:true? 'false))
  (is (clj:true? 'true)))

(deftest false?
  (is-not (clj:false? 'true))
  (is (clj:false? 'false)))

(deftest identical?
  (is-not (clj:identical? '(a b c) '(a b d)))
  (is (clj:identical? '(a b c) '(a b c))))

(deftest empty?
  (is-not (clj:empty? '(1 2 3)))
  (is (clj:empty? ())))

(deftest every?
  (is-not (clj:every? #'clj:zero?/1 '(0 0 0 0 1)))
  (is (clj:every? #'clj:zero?/1 '(0 0 0 0 0))))

(deftest any?
  (is-not (clj:any? #'clj:zero?/1 '(1 1 1 1 1)))
  (is (clj:any? #'clj:zero?/1 '(0 1 1 1 1))))

(deftest not-any?
  (is-not (clj:not-any? #'clj:zero?/1 '(0 1 1 1 1)))
  (is (clj:not-any? #'clj:zero?/1 '(1 1 1 1 1))))

(deftest element?
  (are* [data] (not (clj:element? 'z data))
        '(a b c)
        (sets:from_list '(a b c))
        (ordsets:from_list '(a b c)))
  (are* [data] (clj:element? 'a data)
        '(a b c)
        (sets:from_list '(a b c))
        (ordsets:from_list '(a b c))))


;;; clj-seq-testas

(deftest seq
  (are* [x y] (=:= x y)
        '(1 2 3 4)    (clj:seq 4)
        '(2 3 4)      (clj:seq 2 4)
        '(2 4 6 8 10) (clj:seq 2 10 2)))

(deftest drop
  (are* [x y] (=:= x y)
        '(6 7 8 9 10 11 12) (clj:drop 5    '(1 2 3 4 5 6 7 8 9 10 11 12))
        ()                  (clj:drop 'all '(1 2 3 4 5 6 7 8 9 10 11 12))))

(deftest take
  (are* [n xs] (=:= xs (clj:take n '(1 2 3 4 5 6 7 8 9 10 11 12)))
        5    '(1 2 3 4 5)
        'all '(1 2 3 4 5 6 7 8 9 10 11 12))
  (is-equal '(1 2 3 4) (clj:take 4 (clj:range)))
  (is-error 'function_clause (clj:take -1 (clj:range))))

(deftest next-and-take
  (are* [x y] (=:= x y)

        '(1 6 21 66 201 606 1821 5466 16401 49206)
        (clj:take 10 (clj:next (lambda (x y) (* 3 (+ x y))) 1 1))

        '(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536)
        (clj:take 17 (clj:next (lambda (x _) (* 2 x)) 1 1))

        '(1 4.0 25.0 676.0 458329.0 210066388900.0 4.4127887745906175e22)
        (clj:take 7 (clj:next (lambda (x _) (math:pow (+ x 1) 2)) 1 1))))

(deftest range
  (are* [x y] (=:= x y)
        1 (car (funcall (clj:range)))
        2 (car (funcall (cdr (funcall (clj:range)))))
        3 (car (funcall (cdr (funcall (cdr (funcall (clj:range)))))))
        4 (clj:->> (funcall (clj:range))
                   (cdr) (funcall) (cdr) (funcall) (cdr) (funcall) (car))))

(deftest split-at
  (is-equal
   #((1 2 3) (4 5 6 7 8 9 10 11 12))
   (clj:split-at 3 '(1 2 3 4 5 6 7 8 9 10 11 12))))

(deftest partition
  (are* [n parts] (=:= parts (clj:partition n ()))
        0   ()
        1   ()
        100 ())
  (are* [n parts] (=:= parts (clj:partition n '(1 2 3 4 5 6 7 8 9 10 11 12)))
        0 '(1 2 3 4 5 6 7 8 9 10 11 12)
        1 '((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12))
        2 '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12))
        5 '((1 2 3 4 5) (6 7 8 9 10))
        7 '((1 2 3 4 5 6 7))
        11 '((1 2 3 4 5 6 7 8 9 10 11))
        12 '((1 2 3 4 5 6 7 8 9 10 11 12))))

(deftest partition-all
  (are* [n pts] (=:= pts (clj:partition-all n '(1 2 3 4 5 6 7 8 9 10 11 12)))
        5 '((1 2 3 4 5) (6 7 8 9 10) (11 12))
        7 '((1 2 3 4 5 6 7) (8 9 10 11 12))
        11 '((1 2 3 4 5 6 7 8 9 10 11) (12))))

(deftest interleave
  (is-equal '(a 1 b 2 c 3) (clj:interleave '(a b c) '(1 2 3))))

(deftest get-in-nth
  (let ((data '((1)
                (1 2 3)
                (1 2 (3 4 (5 6 (7 8 9)))))))
    (are* [val keys] (=:= val (clj:get-in data keys))
          1          '(1 1)
          3          '(2 3)
          9          '(3 3 3 3 3)
          'undefined '(4)
          'undefined '(4 3 3 3))))

(deftest get-in-keys
  (let ((data '(#(key-1 val-1)
                #(key-2 val-2)
                #(key-3 (#(key-4 val-4)
                         #(key-5 val-5)
                         #(key-6 (#(key-7 val-7)
                                  #(key-8 val-8)
                                  #(key-9 val-9))))))))
    (are* [val keys] (=:= val (clj:get-in data keys))
          'val-1     '(key-1)
          'val-5     '(key-3 key-5)
          'val-9     '(key-3 key-6 key-9)
          'undefined '(key-18)
          'undefined '(key-3 key-6 key-89)
          'undefined '(key-3 key-6 key-89 key-100))))

(deftest reduce
  (let ((lst '(1 2 3)))
    (are* [f] (=:= 6 (clj:reduce f lst))
          (lambda (x acc) (+ x acc))
          (fun + 2)
          #'+/2)
    (are* [f] (=:= 6 (clj:reduce f 0 lst))
          (lambda (x acc) (+ x acc))
          (fun + 2)
          #'+/2)))

(deftest repeat
  (are* [x y] (=:= x y)
        '(1 1 1)       (clj:repeat 3 1)
        '("xo" "xo")   (clj:repeat 2 "xo")
        ()             (clj:repeat 0 "oh noes")
        '(ok ok ok ok) (clj:repeat 4 'ok)
        '(1 1 1)       (clj:repeat 3 (lambda () 1))
        2              (length (clj:repeat 2 #'random:uniform/0)))
  (is-error 'function_clause (clj:repeat -1 0))
  (is-error 'function_clause (clj:repeat -1 (lambda () 1))))
