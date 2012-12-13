(defmodule test_hash_quote
  (export (run_all 0))
  (export (a 0) (b 0) (c 0) (d 0) (e 0) (f 0) (g 0) (h 0) (i 0) (j 0) (k 0))
  (export (foo:bar42 1) (foo/baz42 1)))

(defun run_all ()
  (: lists map
    (lambda (f) (: io format '"~p~n" (list (funcall f))))
    (list (fun a 0) (fun b 0) (fun c 0) (fun d 0) (fun e 0) (fun f 0) (fun g 0)
      (fun h 0) (fun i 0) (fun j 0) (fun k 0))))

(defun a ()
  (let (((1 2 3) (: lists sort #'</2 '(2 1 3))))
    'ok))

(defun b ()
  (let ((42.0 (funcall #'//2 420 10)))
    'ok))

(defun c ()
  (let (((-1 2) (: lists map #'-/1 '(1 -2))))
    'ok))

(defun d ()
  (let (((1 2) (: lists map #'abs/1 '(-1 2))))
    'ok))

(defun e ()
  (let ((("foobar") (: lists map #'string:to_lower/1 '("fooBar"))))
    'ok))

(defun f ()
  (let (((tuple 'EXIT (tuple 'undef _))
         (catch (: lists map #'foo:bar42/1 '(1 2)))))
    'ok))

(defun g ()
  (let (((42 42) (: lists map #'test_hash_quote:foo:bar42/1 '(1 2))))
    'ok))

(defun h ()
  (let (((42 42) (: lists map #'foo/baz42/1 '(1 2))))
    'ok))

(defun i ()
  (let (((42 42) (: lists map #'test_hash_quote:foo/baz42/1 '(1 2))))
    'ok))

(defun j ()
  (let ((('false 'true 'false) (: lists zipwith #'=:=/2 '(1 2 3) '(3 2 1))))
    'ok))

(defun k ()
  (let ((('true 'false 'true) (: lists zipwith #'=/=/2 '(1 2 3) '(3 2 1))))
    'ok))

;;;;

(defun foo:bar42 (_)
  42)

(defun foo/baz42 (_)
  42)
