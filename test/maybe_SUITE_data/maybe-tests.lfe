;; -*- mode: LFE; indent-tabs-mode: nil -*-

(defmodule maybe-tests
  (export (t1 1) (t2 1) (t3 1) (t4 1)
          (tl1a 1) (tl1b 1) (tl2a 1) (tl2b 1)
          (tl3a 1) (tl3b 1) (tl4a 1) (tl4b 1))
  )

;;
;; t1, t2, t3, t4 flat maybe tests where t1 and t2 return values after ?=.
;;

(defun t1 (g)
  (maybe
    ;; (foo g)
    (?= `#(ok ,a) (a g))
    ;; (bar g)
    (?= `#(ok ,b) (b g))
    (+ a b)
    )
  )

;; t1(G) ->
;;     maybe
;;         %% foo(G),
;;         {ok,A} ?= a(G),
;;         %% bar(G),
;;         {ok,B} ?= b(G),
;;         A + B
;;     end.

(defun t2 (g)
  (maybe
    ;; (foo g)
    (?= `#(ok ,a) (a g))
    ;; (bar g)
    (?= `#(ok ,b) (b g))
    (+ a b)
    else
    (['error] #(got error))
    (['wrong] 2 #(got wrong))           ;This will generate a warning
    )
  )

;; t2(G) ->
;;     maybe
;;         %% foo(G),
;;         {ok,A} ?= a(G),
;;         %% bar(G),
;;         {ok,B} ?= b(G),
;;         A + B
;;     else
;;         error ->
;;             {got,error};
;;         wrong ->
;;             2, {got,wrong}
;;     end.

(defun t3 (g)
  (maybe
    ;; (foo g)
    (?= `#(ok ,a) (a g))                ;This will generate a warning
    ;; (bar g)
    (?= `#(ok ,b) (b g))                ;This value should be returned
    )
  )

;; t3(G) ->
;;     maybe
;;         %% foo(G),
;;         {ok, A} ?= a(G),                 %This will generate a warning
;;         %% bar(G),
;;         {ok, B} ?= b(G)                  %This will generate a warning
;;     end.

(defun t4 (g)
  (maybe
    ;; (foo g)
    (?= `#(ok ,a) (a g))                ;This will generate a warning
    ;; (bar g)
    (?= `#(ok ,b) (b g))                ;This value should be returned
    else
    (['error] #(got error))
    (['wrong] 2 #(got wrong))           ;This will generate a warning
    )
  )

;; t4(G) ->
;;     maybe
;;         %% foo(G),
;;         {ok, A} ?= a(G),                %This will generate a warning
;;         %% bar(G),
;;         {ok, B} ?= b(G)                 %This will generate a warning
;;     else
;;         error ->
;;             {got,error};
;;         wrong ->
;;             2, {got,wrong}              %This will generate a warning
;;     end.

;;
;; tl1, tl2, tl3, tl4 maybe tests with let where t1 and t2 return
;; values after ?=.
;;

(defun tl1a (g)
  (maybe
    ;; (foo g)
    (?= `#(ok ,a) (a g))
    (let ((x (foo g))
          (`#(blah ,y) (bar g)))
      (a x))
    (?= `#(ok ,b) (b g))
    (+ a b)
    )
  )

;; tl1a(G) ->
;;     maybe
;;         %% foo(G),
;;         {ok,A} ?= a(G),
;;         X = foo(G),
;;         {blah,Y} = bar(G),
;;         {ok,B1} ?= b(G),
;;         a(X),
;;         %% bar(G),
;;         {ok,B} ?= b(G),
;;         A + B
;;     end.

(defun tl1b (g)
  (maybe
    ;; (foo g)
    (?= `#(ok ,a) (a g))
    (let ((x (foo g))
          (`#(blah ,y) (bar g)))
      (a x)
      (?= `#(ok ,b) (b g))
      (+ a b))
    )
  )

;; tl1b(G) ->
;;     maybe
;;         %% foo(G),
;;         {ok,A} ?= a(G),
;;         X = foo(G),
;;         {blah,Y} = bar(G),
;;         a(X),
;;         {ok,B1} ?= b(G),
;;         A + B1
;;     end.

(defun tl2a (g)
  (maybe
     ;; (foo g)
    (?= `#(ok ,a) (a g))
    (let ((x (foo g))
          (`#(blah ,y) (bar g)))
      (a x))
    (?= `#(ok ,b) (b g))
    (+ a b)
    else
    (['error] #(got error))
    (['wrong] 2 #(got wrong))           ;This will generate a warning
    )
  )

;; tl2a(G) ->
;;     maybe
;;         %% foo(G),
;;         {ok,A} ?= a(G),
;;         %% bar(G),
;;         {ok,B} ?= b(G),
;;         A + B
;;     else
;;         error ->
;;             {got,error};
;;         wrong ->
;;             2, {got,wrong}
;;     end.
 
(defun tl2b (g)
  (maybe
     ;; (foo g)
    (?= `#(ok ,a) (a g))
    (let ((x (foo g))
          (`#(blah ,y) (bar g)))
      (a x)
      (?= `#(ok ,b) (b g))    
      (+ a b))
    else
    (['error] #(got error))
    (['wrong] 2 #(got wrong))           ;This will generate a warning
    )
  )

;; tl2b(G) ->
;;     maybe
;;         %% foo(G),
;;         {ok,A} ?= a(G),
;;         %% bar(G),
;;         {ok,B} ?= b(G),
;;         A + B
;;     else
;;         error ->
;;             {got,error};
;;         wrong ->
;;             2, {got,wrong}
;;     end.

(defun tl3a (g)
  (maybe
     ;; (foo g)
    (?= `#(ok ,a) (a g))
    (let ((x (foo g))
          (`#(blah ,y) (bar g)))
      (a x))
    (?= `#(ok ,b) (b g))                ;This value should be returned
    )
  )

;; tl3a(G) ->
;;     maybe
;;         %% foo(G),
;;         {ok,A} ?= a(G),
;;         %% bar(G),
;;         {ok,B} ?= b(G)
;;     end.
 
(defun tl3b (g)
  (maybe
     ;; (foo g)
    (?= `#(ok ,a) (a g))
    (let ((x (foo g))
          (`#(blah ,y) (bar g)))
      (a x)
      (?= `#(ok ,b) (b g)))             ;This value should be returned
    )
  )

;; tl3b(G) ->
;;     maybe
;;         %% foo(G),
;;         {ok,A} ?= a(G),
;;         %% bar(G),
;;         {ok,B} ?= b(G)
;;     end.

(defun tl4a (g)
  (maybe
     ;; (foo g)
    (?= `#(ok ,a) (a g))
    (let ((x (foo g))
          (`#(blah ,y) (bar g)))
      (a x))
    (?= `#(ok ,b) (b g))                ;This value should be returned
    else
    (['error] #(got error))
    (['wrong] 2 #(got wrong))           ;This will generate a warning
    )
  )

;; tl4a(G) ->
;;     maybe
;;         %% foo(G),
;;         {ok,A} ?= a(G),
;;         %% bar(G),
;;         {ok,B} ?= b(G),
;;     else
;;         error ->
;;             {got,error};
;;         wrong ->
;;             2, {got,wrong}
;;     end.
 
(defun tl4b (g)
  (maybe
     ;; (foo g)
    (?= `#(ok ,a) (a g))
    (let ((x (foo g))
          (`#(blah ,y) (bar g)))
      (a x)
      (?= `#(ok ,b) (b g)))             ;This value should be returned
    else
    (['error] #(got error))
    (['wrong] 2 #(got wrong))           ;This will generate a warning
    )
  )

;; tl4b(G) ->
;;     maybe
;;         %% foo(G),
;;         {ok,A} ?= a(G),
;;         %% bar(G),
;;         {ok,B} ?= b(G),
;;     else
;;         error ->
;;             {got,error};
;;         wrong ->
;;             2, {got,wrong}
;;     end.

(defun foo
  (['gunnar] `#(bah gunnar gurun))
  ([g] `#(bah ,g ,g)))

(defun bar (g)
  `#(blah ,g))

(defun a
  (['good] `#(ok ,42))
  (['bada] 'error)
  (['errora] (error #(a error)))
  (['throwa] (throw #(a throw)))
  ([i] `#(ok ,i)))                      ;Just pass on everything else

(defun b
  (['good] `#(ok 99))
  (['badb] 'wrong)
  (['errorb] (error #(b error)))
  (['throwb] (throw #(b throw)))
  ([i] `#(ok ,i)))                      ;Just pass on everything else
