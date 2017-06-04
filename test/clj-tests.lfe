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

(defmacro ok? (x) `(=:= 'ok ,x))

;; HACK
(defmacro IFF-MAPS expr `(andalso (erl_internal:bif 'is_map 1) ,@expr))

;;; defn

(defun test-defn (f a def)
  (let* ((forms                      `((defmodule dummy) ,def))
         (`#(ok (#(ok dummy ,beam)))  (lfe_comp:forms forms))
         (`#(ok ,docs)                (lfe_doc:get_module_docs beam)))
    (lfe_io:format "~s/~w ~p~n" (list f a docs))
    (lfe_doc:function_docs f a docs)))

(deftest defn
  (are* [f a def] (ok? (is-match `#(ok ,_doc) (test-defn f a def)))
        'foo 1 '(clj:defn foo [bar] bar)
        'foo 1 '(clj:defn foo [bar] "doc" bar)
        'foo 2 '(clj:defn foo [_bar baz] baz)
        'foo 2 '(clj:defn foo [_bar baz] "doc" baz)
        'foo 3 '(clj:defn foo ([_bar _baz qux] qux))
        'foo 3 '(clj:defn foo "doc" ([_bar _baz qux] qux))
        'foo 3 '(clj:defn foo ([`#(bar ,_bar) _baz qux] qux))
        'foo 3 '(clj:defn foo "doc" ([`#(bar ,_bar) _baz qux] qux))))

;;; Threading macros.

(deftest ->
  (are* [x y] (ok? (is-match x y))

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
  (are* [x y] (ok? (is-match x y))
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

;; Ported from #'clojure.test-clojure.macros/as->test
(deftest as->
  (flet ((inc (x) (+ x 1)))
    (is-match 0 (clj:as-> 0 x))
    (is-match 1 (clj:as-> 0 x (inc x)))
    (is-match 2 (clj:as-> '[0 1] x
                  (lists:map #'inc/1 x)
                  (lists:reverse x)
                  (car x)))))

;; Ported from #'clojure.test-clojure.macros/cond->test
(deftest cond->
  (is-match  0 (clj:cond-> 0))
  (is-match -1 (clj:cond-> 0 'true  (+ 1) 'true  (- 2)))
  (is-match  0 (clj:cond-> 0 'false (+ 1)))
  (is-match -1 (clj:cond-> 1 'true  (- 2) 'false (+ 1))))

;; Ported from #'clojure.test-clojure.macros/cond->>test
(deftest cond->>
  (is-match 0 (clj:cond->> 0))
  (is-match 1 (clj:cond->> 0 'true  (+ 1) 'true  (- 2)))
  (is-match 0 (clj:cond->> 0 'false (+ 1)))
  (is-match 1 (clj:cond->> 1 'true  (- 2) 'false (+ 1))))

;; Ported from #'clojure.test-clojure.macros/some->test
(deftest some->test
  (is-match 'undefined (clj:some-> 'undefined))
  (is-match  0         (clj:some-> 0))
  (is-match -1         (clj:some-> 1 (- 2)))
  (is-match 'undefined (clj:some-> 1 (progn 'undefined) (- 2))))

;; Ported from #'clojure.test-clojure.macros/some->>test
(deftest some->>test
  (is-match 'undefined (clj:some->> 'undefined))
  (is-match 0 (clj:some->> 0))
  (is-match 1 (clj:some->> 1 (- 2)))
  (flet ((constantly-undefined (_) 'undefined))
    (is-match 'undefined (clj:some->> 1 (constantly-undefined) (- 2)))))

(deftest doto
  (is-not-match 6 (clj:doto 42 (/ 7)))
  (let ((answer 42))
    (is-match answer (clj:doto answer (+ 1) (- 10) (progn 'undefined) (list))))
  (let ((bin #"never you change"))
    (is-match bin (clj:doto bin (io:put_chars)))))

;;; Conditional macros

;; Ported from #'clojure.test-clojure.control/test-condp
(deftest condp
  (are* [x] (ok? (is-match 'pass x))
        (clj:condp #'=:=/2 1
          1 'pass
          2 'fail)
        (clj:condp #'=:=/2 1
          2 'fail
          1 'pass)
        (clj:condp #'=:=/2 1
          2 'fail
          'pass)
        (clj:condp #'=:=/2 1
          'pass)
        (clj:condp #'=:=/2 1
          2 'fail
          (clj:identity 'pass))
        (clj:condp #'+/2 1
          1 >> (lambda (y) (if (=:= y 2) 'pass 'fail)))
        (clj:condp #'+/2 1
          1 >> (lambda (y) (if (=:= y 3) 'fail 'pass))))
  (is-error 'no-matching-clause (clj:condp #'=:=/2 1))
  (is-error 'no-matching-clause (clj:condp #'=:=/2 1 2 'fail)))

(deftest if-not
  (is-not (clj:if-not 'true 'then 'false))
  (are* [x] (clj:true? x)
        (clj:if-not 'false 'true)
        (clj:if-not 'false 'true 'else)))

;; Ported from #'clojure.test-clojure.control/test-when
(deftest iff
  (are* [x y] (ok? (is-match x y))
        1 (clj:iff 'true 1)
        () (clj:iff 'true)
        'undefined (clj:iff 'false)
        'undefined (clj:iff 'undefined)
        'undefined (clj:iff 'false (error 'bad-iff))))

(deftest when-not
  (is-match 'undefined (clj:when-not 'true 'ok))
  (is-match 'true (clj:when-not 'false 'true))
  (is-match 42 (clj:when-not 'undefined 42)))

(deftest not=
  (is-not (clj:not= 42))
  (is-not (clj:not= 42 42))
  (is (clj:not= 42 123)))

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

;;; clj-p-tests

(deftest string?
  (is-not (clj:string? (list "my" "string" "data")))
  (is (clj:string? "string data! yaya!")))

(deftest unicode?
  (is-not (clj:unicode? (lists:seq 1 7)))
  (are* [s] (clj:unicode? s)
        "simple"
        "Schrödinger"
        "Umeå Vråljazz Giganter"))

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

(deftest binary?
  (is-not (clj:binary? "string"))
  (is-not (clj:binary? (binary (42 (size 6)))))
  (is (clj:binary? #"binary"))
  (is (clj:binary? #b(125 114 196))))

(deftest bitstring?
  (is-not (clj:bitstring? "string"))
  (are* [x] (clj:bitstring? x)
        (binary (42 (size 6)))
        #"binary"
        #b(125 114 196)))

(deftest boolean?
  (are* [x] (not (clj:boolean? x))
        42
        (andalso 'true 42)
        (orelse  'false 42))
  (are* [x] (clj:boolean? x)
        'true
        'false
        (=/= 42 123)))

(deftest bool?
  (are* [x] (not (clj:bool? x))
        42
        (andalso 'true 42)
        (orelse  'false 42))
  (are* [x] (clj:bool? x)
        'true
        'false
        (=/= 42 123)))

(deftest float?
  (is-not (clj:float? 42))
  (is (clj:float? 42.0)))

(deftest function?
  (are* [x] (not (clj:function? x))
        '|#'+/2|
        '(fun + 2)
        'function
        42)
  (are* [x] (clj:function? x)
        #'+/2
        (fun + 2)
        (lambda (y) (+ y 1))
        (flet ((id (x) x)) #'id/1)))

(deftest func?
  (are* [x] (not (clj:func? x))
        '|#'+/2|
        '(fun + 2)
        'func
        42)
  (are* [x] (clj:func? x)
        #'+/2
        (fun + 2)
        (lambda (y) (+ y 1))
        (flet ((id (x) x)) #'id/1)))

(deftest integer?
  (is-not (clj:integer? 42.0))
  (is (clj:integer? 42)))

(deftest int?
  (is-not (clj:int? 42.0))
  (is (clj:int? 42)))

(deftest number?
  (are* [x] (not (clj:number? x))
        'forty-two
        '(42)
        #(40 2)
        "42"
        #"42")
  (are* [x] (clj:number? x)
        42
        42.0
        (/ 4 2)
        (+ 1.0 2 3)))

;; Based on record_SUITE.
(defrecord foo a b c d)
(deftest record?
  (is (clj:record? (make-foo) 'foo))
  (is-not (clj:record? (make-foo) 'barf))
  ;; This fails due a bug: https://github.com/rvirding/lfe/issues/266
  ;; (is-not (clj:record? #(foo) 'foo))
  (is-not (clj:record? [] 'foo))
  (is-not (clj:record? 'a 'foo)))

(deftest reference?
  (is-not (clj:reference? 42))
  (is (clj:reference? (make_ref))))

(deftest map?
  (is-not (clj:map? 42))
  (IFF-MAPS (is (clj:map? (call 'maps 'new)))))

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

(deftest undefined?
  (is-not (clj:undefined? 42))
  (is-not (clj:undefined? 'undef))
  (is (clj:undefined? 'undefined)))

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

(deftest falsy?
  (is-not (clj:falsy? 'true))
  (is-not (clj:falsy? 42))
  (is-not (clj:falsy? ()))
  (is (clj:falsy? 'false))
  (is (clj:falsy? 'undefined))
  (is (clj:falsy? (proplists:get_value 42 ()))))

(deftest odd?
  (is-not (clj:odd? 42))
  (is (clj:odd? 333)))

(deftest even?
  (is-not (clj:even? 333))
  (is (clj:even? 42)))

(deftest zero?
  (is-not (clj:zero? 42))
  (is (clj:zero? 0)))

(deftest pos?
  (is-not (clj:pos? -42))
  (is (clj:pos? 42)))

(deftest neg?
  (is-not (clj:neg? 42))
  (is (clj:neg? -42)))

(deftest identical?
  (is-not (clj:identical? '(a b c) '(a b d)))
  (is (clj:identical? '(a b c) '(a b c))))

;;; Other macros.

(deftest str
  (are* [x y] (ok? (is-match x y))
        "" (clj:str)
        "123" (clj:str 1 2 3)
        "abc" (clj:str "a" "b" "c")
        "abc" (clj:str 'a 'b "c")
        "1a2b" (clj:str 1 'a 2 "b")
        "2.0c" (clj:str 2.0 'c)
        "roughly 3.14"
        (let ((pi-string (clj:str "roughly" " " 3.14)))
          pi-string)
        "200 and a half"
        (let ((number-str (clj:str 200 " " 'and " " 'a " " 'half)))
          number-str)
        "eighty plus 1000"
        (let ((x "eighty ")
              (y "plus ")
              (z 1000))
          (clj:str x y z))
        "a1b2"
        (let ((a 'a)
              (b 'b)
              (one 1)
              (two 2))
          (clj:str a one b two))))

;; Based on OTP's queue_SUITE.
(deftest queue?
  (is-not (clj:queue? '[1 2 3 4 5]))
  (clj:as-> (queue:new) q
    (queue:in 1 q)
    (queue:in 2 q)
    (queue:in 3 q)
    (let ((`#(#(value 1) ,q1) (queue:out q)))
      q1)
    (queue:in 4 q)
    (queue:in 5 q)
    (is (clj:queue? q))))

(deftest empty?
  (is-not (clj:empty? '(1 2 3)))
  (IFF-MAPS (is-not (clj:empty? (call 'maps 'from_list '[#(a 1) #(b 2)]))))
  (is (clj:empty? ()))
  (IFF-MAPS (is (clj:empty? (call 'maps 'new)))))

(deftest every?
  (is-not (clj:every? #'clj:zero?/1 '(0 0 0 0 1)))
  (is (clj:every? #'clj:zero?/1 '(0 0 0 0 0))))

;; Based on lists_SUITE.
(deftest all?
  (is-error (lists:all 'func []))
  (flet ((pred (_a) 'true))
    (is (lists:all #'pred/1 [])))
  (let ((l '[1 2 3]))
    (is (lists:all (lambda (n) (clj:integer? n)) l))
    (is-not (lists:all (lambda (n) (=:= (rem n 2) 0)) l))))

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

;;; clj-seq-tests

(deftest seq
  (are* [x y] (ok? (is-match x y))
        '(1 2 3 4)    (clj:seq 4)
        '(2 3 4)      (clj:seq 2 4)
        '(2 4 6 8 10) (clj:seq 2 10 2)))

(deftest conj
  (is-match '(1 2 3 4) (clj:conj '(2 3 4) 1))
  (is-match '((1) 2 3 4) (clj:conj '(2 3 4) '(1)))
  (is-match '(1 2 3 4) (clj:conj '(4) 3 2 1))
  (is-match #(a b c d) (clj:conj #(a b c) 'd))
  (is-match #(a b c d) (clj:conj #(a) 'b 'c 'd))
  (is-match #(a b c #(d)) (clj:conj #(a b c) #(d)))
  (IFF-MAPS
   (is-equal (maps:from_list '(#(a 1) #(b 2)))
             (clj:conj (maps:from_list '(#(b 2)))
                       (maps:from_list '(#(a 1)))))))

(deftest drop
  (are* [x y] (ok? (is-match x y))
        '(6 7 8 9 10 11 12) (clj:drop 5    (lists:seq 1 12))
        ()                  (clj:drop 'all (lists:seq 1 12))))

(deftest take
  (are* [n xs] (ok? (is-match xs (clj:take n (lists:seq 1 12))))
        5    '(1 2 3 4 5)
        'all '(1 2 3 4 5 6 7 8 9 10 11 12))
  (is-equal '(1 2 3 4) (clj:take 4 (clj:range)))
  (is-error 'function_clause (clj:take -1 (clj:range))))

(deftest next-and-take
  (are* [x y] (ok? (is-match x y))

        '(1 6 21 66 201 606 1821 5466 16401 49206)
        (clj:take 10 (clj:next (lambda (x y) (* 3 (+ x y))) 1 1))

        '(1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536)
        (clj:take 17 (clj:next (lambda (x _) (* 2 x)) 1 1))

        '(1 4.0 25.0 676.0 458329.0 210066388900.0 4.4127887745906175e22)
        (clj:take 7 (clj:next (lambda (x _) (math:pow (+ x 1) 2)) 1 1)))
  (is-not-error (clj:take 3 (clj:next (match-lambda
                                         ([1 _] 2)
                                         ([2 _] 3)
                                         ([3 _] 4)
                                         ([4 _] (error 'overzealous-take)))))))

(deftest lazy-seq-and-take-and-drop
  (are* [x y] (ok? (is-match x y))

        ()
        (clj:lazy-seq ())

        ()
        (clj:take 1 (clj:lazy-seq ()))

        '(1 2)
        (clj:take 2 (clj:lazy-seq '(1 2 3)))

        '(1 2 3)
        (clj:take 3 (clj:lazy-seq '(1 2 3)))

        '(1 2 3)
        (clj:take 4 (clj:lazy-seq '(1 2 3))))

  (are* [x y] (ok? (is-match x y))

        ()
        (clj:take 1 (clj:drop 1 (clj:lazy-seq ())))

        ()
        (clj:take 1 (clj:drop 1 (clj:lazy-seq '(1))))

        ()
        (clj:take 1 (clj:drop 4 (clj:lazy-seq '(1 2 3))))

        '(2 3)
        (clj:take 2 (clj:drop 1 (clj:lazy-seq '(1 2 3))))

        '(2 3)
        (clj:take 2 (clj:drop 1 (clj:range 1)))))

(deftest lazy-seq-evaluation
  (flet ((form-list (last-element)
                    `(lists:map #'funcall/1
                                '((lambda () 1)
                                  (lambda () ,last-element)))))

    (let ((bad-list (form-list '(error 'evaluation_error)))
          (good-list (form-list 2)))

      (is-error 'evaluation_error (eval bad-list))
      (is-error 'evaluation_error (eval `(clj:take 1 (clj:lazy-seq ,bad-list))))

      (is-not-error (eval `(clj:lazy-seq ,bad-list)))
      (is-not-error (eval `(clj:lazy-seq (clj:lazy-seq ,bad-list))))

      (is-match '(1 2) (eval `(clj:take 3 (clj:lazy-seq ,good-list)))))))

(deftest cycle-and-take
  (are* [x y] (ok? (is-match x y))

        ()
        (clj:cycle ())

        '(1 1 1)
        (clj:take 3 (clj:cycle '(1)))

        '(1 2 3)
        (clj:take 3 (clj:cycle '(1 2 3)))

        '(1 2)
        (clj:take 2 (clj:cycle '(1 2 3)))

        '(1 2 3 1)
        (clj:take 4 (clj:cycle '(1 2 3)))

        '(1 2 3 1 2 3 1)
        (clj:take 7 (clj:cycle '(1 2 3))))

  (are* [x y] (ok? (is-match x y))

        '(3 3 3 3)
        (clj:take 4 (clj:cycle (clj:repeat 3)))

        '(3 4 5 6)
        (clj:take 4 (clj:cycle (clj:range 3)))

        '(1 2 3 1)
        (clj:take 4 (clj:cycle (clj:cycle '(1 2 3)))))

  (are* [x y] (ok? (is-match x y))

        ()
        (clj:cycle (clj:lazy-seq ()))

        '(1 1 1)
        (clj:take 3 (clj:cycle (clj:lazy-seq '(1))))

        '(1 2 3 1 2 3 1)
        (clj:take 7 (clj:cycle (clj:lazy-seq '(1 2 3))))

        '(1 2 3 1 2 3 1)
        (clj:take 7 (clj:lazy-seq (clj:cycle '(1 2 3))))

        '(2 3 2 3 2)
        (clj:take 5 (clj:cycle (clj:drop 1 (clj:lazy-seq '(1 2 3)))))))

(deftest range
  (are* [x y] (ok? (is-match x y))
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
  (are* [n parts] (ok? (is-match parts (clj:partition n ())))
        0   ()
        1   ()
        100 ())
  (are* [n parts] (ok? (is-match parts (clj:partition n (lists:seq 1 12))))
        0 '(1 2 3 4 5 6 7 8 9 10 11 12)
        1 '((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12))
        2 '((1 2) (3 4) (5 6) (7 8) (9 10) (11 12))
        5 '((1 2 3 4 5) (6 7 8 9 10))
        7 '((1 2 3 4 5 6 7))
        11 '((1 2 3 4 5 6 7 8 9 10 11))
        12 '((1 2 3 4 5 6 7 8 9 10 11 12))))

(deftest partition-all
  (are* [n pts] (ok? (is-match pts (clj:partition-all n (lists:seq 1 12))))
        5 '((1 2 3 4 5) (6 7 8 9 10) (11 12))
        7 '((1 2 3 4 5 6 7) (8 9 10 11 12))
        11 '((1 2 3 4 5 6 7 8 9 10 11) (12))))

(deftest interleave
  (is-equal '(a 1 b 2 c 3) (clj:interleave '(a b c) '(1 2 3))))

(deftest get-in-nth
  (let ((data '((1)
                (1 2 3)
                (1 2 (3 4 (5 6 (7 8 9)))))))
    (are* [val keys] (ok? (is-match val (clj:get-in data keys)))
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
    (are* [val keys] (ok? (is-match val (clj:get-in data keys)))
          'val-1     '(key-1)
          'val-5     '(key-3 key-5)
          'val-9     '(key-3 key-6 key-9)
          'undefined '(key-18)
          'undefined '(key-3 key-6 key-89)
          'undefined '(key-3 key-6 key-89 key-100))))

(deftest get-in-default
  (let ((data '(#(key-1 val-1)
                #(key-2 val-2)
                #(key-3 (#(key-4 val-4)
                         #(key-5 val-5)
                         #(key-6 (#(key-7 val-7)
                                  #(key-8 val-8)
                                  #(key-9 val-9))))))))
    (are* [keys] (=/= 'default (clj:get-in data keys 'default))
          '(key-1)
          '(key-3 key-5)
          '(key-3 key-6 key-9))
    (are* [keys] (ok? (is-match 'default (clj:get-in data keys 'default)))
          '(key-18)
          '(key-3 key-6 key-89)
          '(key-3 key-6 key-89 key-100))))

(deftest get-in-map
  (IFF-MAPS
   (let* ((c (call 'maps 'from_list '[#(c 123)]))
          (b (call 'maps 'from_list `[#(b ,c)]))
          (m (call 'maps 'from_list `[#(a ,b)])))
     (are* [data keys] (ok? (is-match data (clj:get-in m keys)))
           b          '(a)
           c          '(a b)
           123        '(a b c)
           'undefined '(x y z)))))

(deftest reduce
  (let ((lst '(1 2 3)))
    (are* [f] (ok? (is-match 6 (clj:reduce f lst)))
          (lambda (x acc) (+ x acc))
          (fun + 2)
          #'+/2)
    (are* [f] (ok? (is-match 6 (clj:reduce f 0 lst)))
          (lambda (x acc) (+ x acc))
          (fun + 2)
          #'+/2)))

(deftest repeat
  (are* [x y] (ok? (is-match x y))
        '(1 1 1)       (clj:repeat 3 1)
        '("xo" "xo")   (clj:repeat 2 "xo")
        ()             (clj:repeat 0 "oh noes")
        '(ok ok ok ok) (clj:repeat 4 'ok)
        '(1 1 1)       (clj:repeat 3 (lambda () 1))
        2              (length (clj:repeat 2 #'random:uniform/0)))
  (is-error 'function_clause (clj:repeat -1 0))
  (is-error 'function_clause (clj:repeat -1 (lambda () 1))))

;;; Other functions.

(deftest identity
  (are* [x] (ok? (is-match x (clj:identity x)))
        'atom
        0 42
        0.0 3.14
        #\c
        "" "abc"
        #"" #"abc"
        () (1 2)
        #() #(1 2))
  (are* [x] (ok? (is-equal x (clj:identity x)))
        (IFF-MAPS (call 'maps 'new))
        (IFF-MAPS (call 'maps 'from_list '[#(a 1) #(b 2)])))
  (is-equal 3 (clj:identity (+ 1 2)))
  (is (clj:identity (> 5 0))))

(deftest constantly
  (flet ((c0 (x) (funcall (clj:constantly 10) x)))
    (are* [x] (ok? (is-match 10 (c0 x)))
          'nil
          42
          "foo")))

(deftest inc
  (is-match 2 (clj:inc 1))
  (is-match 4.0 (clj:inc 3.0)))

(deftest dec
  (is-match 2 (clj:dec 3))
  (is-match 4.0 (clj:dec 5.0)))
