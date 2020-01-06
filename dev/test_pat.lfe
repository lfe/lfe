;;; File    : test_pat.lfe
;;; Author  : Robert Virding
;;; Purpose : Test patterns.

(defmodule test_pat
  (export (a 2) (b 2) (c 1) (c 2) (d 2) (f 2) (g 2)))

(defsyntax make-foo
  (fs (tuple 'sune . fs)))

(defsyntax pat-foo
  ((f1) (tuple 'sune f1))
  ((f1 f2) (tuple 'sune f1 f2))
  ((f1 f2 f3) (tuple 'sune f1 f2 f3)))

;; Testing aliases (that match)
(defun a (x y)
  (case (list x y)
    ((list 'a 4) (tuple 1.5 'a 4))
    ((= (list 'a _) (cons p1 ps)) (tuple 2 'a p1 ps))
    ((= (tuple 'a _) (tuple p1 p2)) (tuple 2 'a p1 p2))
    ((list _ _) (tuple 3 'anything))))

;; Testing aliases (that don't match)
;; (define (a-1 x y)
;;   (case (list x y)
;;     ((list 'a 4) (tuple 1 'a 4))
;;     ((= (list 'a _) (tuple p1 ps)) (tuple 2 'a p1 ps))
;;     ((= #(a _) (tuple p1 p2)) (tuple 2 'a p1 p2))
;;     ((= #(a _) (tuple p1 p2 p3)) (tuple 2 'a p1 p2 p3))
;;     ((list _ _) (tuple 3 'anything))))

(defun b
  (('a 4) (tuple 1 'a 4))
  (('a _) (tuple 2 'a 'anything))
  ((_ _) (tuple 3 'anything)))

(defun c
  (("1234") 'string)
  ((_) 'other))

;; Macro expansion in patterns.
(defun c (x y)
  (let ((foo (make-foo x y)))
    (case foo
      ((pat-foo m n) (tuple foo m n)))))

(defun d
  (((pat-foo m n) x) (tuple m n x)))

;; Testing patterns and guards in let.
(defun e (x y)
  (let (((cons p ps) (when (is_number p)) (b x y))
    (q (when (is_atom q)) (c x y)))
    (d p q)))

;; Test equivalence of old and new patterns, compare generated code.

(defun f (x y)
  (case (cons x y)
    (('a m) (tuple 1 'old 'a m))
    ((list 'a m) (tuple 1 'new 'a m))
    ((m . n) (tuple 2 'old m n))
    ((cons m n) (tuple 2 'new m n))
    ;((m n . o) (tuple 3 'old m n o))
    ((cons m (cons n o))                ;(list* m n o)
     (tuple 3 'new m n o))
    ;; cons/list become "reserved words" like tuple/binary.
    ((m cons n o) (tuple 4 'old m n o)) ;(cons m (cons n o))
    ((m list n o) (tuple 5 'old m n o)) ;(cons m (list n o))
    ))

;; Test aliases, old/old, old/new and new/new.

(defun g (x y)
  (case (cons x y)
    ;; Check old to old.
    ((= ('a m1) ('a m2)) (tuple 1 'old/old 'a m1))
    ((= (m1 . n1) (n2 . m2)) (tuple 2 'old/old m1 n1))
    ((= (m1 n1 . o1) (m2 n2 . o2)) (tuple 3 'old/old m1 n1 o1))
    ;; Check new to new.
    ((= (list 'a m1) (list 'a m2)) (tuple 1 'new/new 'a m1))
    ((= (cons m1 n1) (cons m2 n2)) (tuple 2 'new/new m1 n2))
    ((= (cons m1 (cons n1 o1)) (list* m2 n2 o2)) (tuple 3 'new/new m1 n1 o1))
    ;; New/old
    ((= (list 'a m1) ('a m2)) (tuple 1 'new/old 'a m1))
    ((= (cons m1 n1) (m2 . n2)) (tuple 2 'new/old m1 n1))
    ((= (cons m1 (cons n1 o1)) (m2 n2 . o2)) (tuple 3 'new/old m1 n1 o1))
    ;; cons/list become "reserved words" like tuple/binary.
    ((= (m1 cons n1 o1) (list* m2 n2 o2)) (tuple 4 'new/old m1 n1 o1))
    ((= (m1 list n1 o1) (cons m2 (list n2 o2))) (tuple 5 'new/old m1 n1 o1))
    ))
