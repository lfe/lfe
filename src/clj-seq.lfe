;;;; Various macros and functions that relate to sequences
;;;;
(defmodule clj-seq
  (export all))

(include-lib "clj/include/predicates.lfe")

;; List sequence wrapper functions
;;
;; Usage:
;;
;; > (seq 10)
;; (1 2 3 4 5 6 7 8 9 10)
;;
(defun seq (end)
  (lists:seq 1 end))

(defun seq (start end)
  (lists:seq start end))

(defun seq (start end step)
  (lists:seq start end step))

;; Infinite series functions
;;
;; The following are identical:
;; > (take 21 (range))
;; (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)
;; > (take 21 (next #'+/2 1 1))
;; (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)
;; > (take 21 (next (lambda (x y) (+ x y)) 1 1))
;; (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21)
;;
;; More usage:
;;
;; > (take 10 (next (lambda (x y) (* 3 (+ x y))) 1 1))
;; (1 6 21 66 201 606 1821 5466 16401 49206)
;; > (take 17 (next (lambda (x _) (* 2 x)) 1 1))
;; (1 2 4 8 16 32 64 128 256 512 1024 2048 4096 8192 16384 32768 65536)
;; > (take 7 (next (lambda (x _) (math:pow (+ x 1) 2)) 1 1))
;; (1 4.0 25.0 676.0 458329.0 210066388900.0 4.4127887745906175e22)
;;
(defun next (func)
  (next func 1 1))

(defun next (func start)
  (next start 1))

(defun next (func start step)
  (lambda ()
    (cons start (next func
                      (funcall func start step)
                      step))))

;; Range functions
;;
;; Usage:
;;
;; > (range)
;; #Fun<lfe_eval.23.86468545>
;; > (funcall (range))
;; (1 . #Fun<lfe_eval.23.86468545>)
;; > (funcall (range 100))
;; (100 . #Fun<lfe_eval.23.86468545>)
;;
;; Some more:
;;
;; > (funcall (range))
;; (1 . #Fun<lfe_eval.23.86468545>)
;; > (funcall (cdr (funcall (range))))
;; (2 . #Fun<lfe_eval.23.86468545>)
;; > (funcall (cdr (funcall (cdr (funcall (range))))))
;; (3 . #Fun<lfe_eval.23.86468545>)
;; > (funcall (cdr (funcall (cdr (funcall (cdr (funcall (range))))))))
;; (4 . #Fun<lfe_eval.23.86468545>)
;;
(defun range ()
  (range 1 1))

(defun range (start)
  (range start 1))

(defun range (start step)
  (next #'+/2 start step))

;; Drop function
;;
;; Usage:
;;
;; > (drop 5 '(1 2 3 4 5 6 7 8 9 10 11 12))
;; (6 7 8 9 10 11 12)
;; > (drop 'all '(1 2 3 4 5 6 7 8 9 10 11 12))
;; ()
;;
(defun drop
  ((_ '())
   '())
  (('all data) (when (is_list data))
    '())
  ((x data) (when (is_list data))
    (lists:nthtail x data)))

;; Take functions
;;
;; Usage:
;;
;; > (take 4 (range))
;; (1 2 3 4)
;; > (take 5 '(1 2 3 4 5 6 7 8 9 10 11 12))
;; (1 2 3 4 5)
;; > (take 'all '(1 2 3 4 5 6 7 8 9 10 11 12))
;; (1 2 3 4 5 6 7 8 9 10 11 12)
;;
(defun take
  (('all data) (when (is_list data))
    data)
  ((x data) (when (is_list data))
    (lists:sublist data x))
  ((x func) (when (is_function func) (is_integer x) (>= x 0))
    (take x '() (funcall func))))

(defun take
  ((0 acc _)
    (lists:reverse acc))
  ((x acc (cons item func))
    (take (- x 1) (cons item acc) (funcall func))))

;; Partitioning functions
;;
;; Usage:
;;
;; > (split-at 3 '(1 2 3 4 5 6 7 8 9 10 11 12))
;; '((1 2 3) (4 5 6 7 8 9 10 11 12))
;;
;; > (split-by 2 '(1 2 3 4 5 6 7 8 9 10 11 12))
;; ((1 2) (3 4) (5 6) (7 8) "\t\n" "\v\f")
;; > (split-by 5 '(1 2 3 4 5 6 7 8 9 10 11 12))
;; ((1 2 3 4 5) (6 7 8 9 10) "\v\f")
;; > (split-by 7 '(1 2 3 4 5 6 7 8 9 10 11 12))
;; ((1 2 3 4 5 6 7) "\b\t\n\v\f")
;; > (split-by 11 '(1 2 3 4 5 6 7 8 9 10 11 12))
;; ((1 2 3 4 5 6 7 8 9 10 11) "\f")
;; > (split-by 12 '(1 2 3 4 5 6 7 8 9 10 11 12))
;; ((1 2 3 4 5 6 7 8 9 10 11 12))
;;
;; > (split-into 5 '(1 2 3 4 5 6 7 8 9 10 11 12))
;;
(defun split-at (x data)
  (list (lists:sublist data x) (lists:nthtail x data)))

(defun split-by
  ((0 data)
    data)
  ((_ '())
   '())
  ((x data) (when (> x (length data)))
   (split-by (length data) data))
  ((x data)
   (cons (lists:sublist data x)
         (split-by x (lists:nthtail x data)))))

;; XXX finish implementation for this
;;(defun split-into
;;  ((_ '()) '())

;; Interleave
;;
;; Usage:
;;
;; > (set l1 '(a b c d e f g))
;; (a b c d e f g)
;; > (set l2 '(1 2 3 4 5 6 7))
;; (1 2 3 4 5 6 7)
;; > (interleave l1 l2)
;; (a 1 b 2 c 3 d 4 e 5 f 6 g 7)
(defun interleave (list-1 list-2)
  (lists:flatten
    (lists:map
      #'tuple_to_list/1
      (lists:zip list-1 list-2))))

;; Get-In
;;
;; This macro is inspired by the Clojure function 'get-in'. Unlike the
;; Clojure function, however, the LFE version handles both lists as well
;; as proplists, dicts, orddicts, and maps.
;;
;; List-based usage:
;;
;; Given the following data structure assigned to the variable 'data':
;;
;; '((1)
;;   (1 2 3)
;;   (1 2 (3 4 (5 6 (7 8 9)))))
;;
;; > (include-lib "clj/include/core.lfe")
;; loaded-core
;;
;; > (get-in data 1 1)
;; 1
;; > (get-in data 2 3)
;; 3
;; > (get-in data 3 3 3 3 3)
;; 9
;; > (get-in data 4)
;; undefined
;; > (get-in data 4 3 3 3)
;; undefined
;;
;; Key-value-based usage:
;;
;; Given the following data structure assigned to the variable 'data':
;;
;; '(#(key-1 val-1)
;;   #(key-2 val-2)
;;   #(key-3 (#(key-4 val-4)
;;            #(key-5 val-5)
;;            #(key-6 (#(key-7 val-7)
;;                     #(key-8 val-8))))))
;;
;; > (include-lib "clj/include/core.lfe")
;; loaded-core
;; > (get-in data 'key-1)
;; val-1
;; > (get-in data 'key-3 'key-5)
;; val-5
;; > (get-in data 'key-3 'key-6 'key-8)
;; val-8
;; > (get-in data 'key-19)
;; undefined
;; > (get-in data 'key-3 'key-6 'key-89)
;; undefined
;; > (get-in data 'key-3 'key-6 'key-89 'key-100)
;; undefined
(defun get-in (data keys)
  "This function is not intended to be used directly (though one certainly may)
  but rather to be used via the macro defined in include/seq.lfe."
  ;; XXX We'll take the cheap way out right now and assume (uh-oh ...) that
  ;; any error here will be keys or indices not found, and thus return
  ;; undefined. Might be better to only do this for function_clause errors ...
  (try
    (cond ((clj-p:proplist? data) (get-in-proplist data keys))
          ((clj-p:dict? data) (get-in-dict data keys))
          ((clj-p:list? data) (get-in-list data keys))
          ((clj-p:map? data) (get-in-map data keys)))
    (catch (_
      'undefined))))

(defun get-in-list (data indices)
  (lists:foldl #'lists:nth/2 data indices))

(defun get-in-proplist (data keys)
  (lists:foldl #'proplists:get_value/2 data keys))

(defun get-in-dict (data keys)
  (get-in-kv #'dict:fetch/2 data keys))

(defun get-in-map (data keys)
  (get-in-kv #'maps:get/2 data keys))

(defun get-in-kv
  ((func data (cons key keys))
    (let ((value (funcall func key data)))
      (if (orelse (clj-p:proplist? value)
                  (clj-p:dict? value)
                  (clj-p:map? value))
          (get-in value keys)
          value))))

;; Reduce
;;
;; This macro simplifies the usage of lists:foldl when using it as a reduce
;; operation.
;;
;; Before:
;; (lists:foldl (lambda (n acc) (+ n acc)) 0 '(1 2 3))
;;
;; After:
;; (reduce #'+/2 '(1 2 3))
;; or:
;; (reduce (fun + 2) '(1 2 3))
(defun reduce
 ((func `(,head . ,tail))
  (lists:foldl func head tail)))

;; An alias for lists:foldl to allow for use of the same name for reduce/2 and
;; reduce/3.
(defun reduce (func acc data)
  (lists:foldl func acc data))

;; Repeat
;;
;; Alias for lists:duplicate/2, but can also take a function as an argument.
;; In the first form, returns a list of n items, where each item is constructed
;; by calling f.
;; In the second form, simply repeats n times the item given as argument x.
;;
;; Inspired by Clojure's repeatedly and repeat.
;;
;; Another way to write this would be to use list comprehensions:
;;   (lc ((<- _ (seq n))) x))
;;
;; but constructing the seq is more costly than using recursion to
;; directly construct the list.
(defun repeat
  ((n f) (when (is_function f) (is_integer n) (>= n 0))
    (fletrec ((repeat-fun
                 ((0 acc)
                   acc)
                 ((n acc)
                   (repeat-fun (- n 1) (cons (funcall f) acc)))))
      (repeat-fun n '())))
  ((n x)
    (lists:duplicate n x)))
