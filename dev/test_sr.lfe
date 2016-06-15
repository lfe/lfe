;;; File    : test_sr.lfe
;;; Author  : Robert Virding
;;; Purpose : Test syntax rules.

(defmodule test_sr
  (export (a 1) (b 1) (c 1)))

(defsyntax allan
  (() '(0))
  ((a) '(1 a))
  ((a b) '(2 a b))
  ((((a b) ...) e ...) '(2 (lambda (a ...) e ...) (b ...))))

(defsyntax dodo
  ([((a b c) ...) (t v) e ...]
   [fletrec ((-sune- (a ...)
                     (if t v
                         (begin e ... (-sune- c ...)))))
     (-sune- b ...)]))

(defmacro aaa
  (() '(0))
  ((a) '(1 a))
  ((a b) '(2 a b)))

(defun a (x)
  (aaa x))

(defsyntax bbb
  ([a b ...] '(a b ...)))

(defun b (x)
  (bbb x y z))

(defsyntax ccc
  ([(tuple (a b) ...)] (cons (list a ...) (tuple b ...))))

;; This will not compile, but we are interested in expansion.
(defun c (x)
  (ccc #((m m1) (n n1) (o o1)))         ;Should we take tuple literals?
  (ccc (tuple (m m1) (n n1) (o o1))))

(defun e-1 (m)
  (dodo ((i 1 (+ 1 i))
         (acc 1 (* acc i)))
        ((> i m) acc)
        '(i)))

(defun e-2 (m)
  (do ((l m (cdr l))
       (acc 0 (+ acc 1)))
      ((== l ()) acc)
    (list l acc)))
