;;; File    : test_pat.lfe
;;; Author  : Robert Virding
;;; Purpose : Test patterns.

(defmodule test_pat
  (export (a 2) (b 2) (c 2) (d 2)))

(defsyntax make-foo
  (fs (tuple 'sune . fs)))

(defsyntax pat-foo
  ((f1) (tuple 'sune f1))
  ((f1 f2) (tuple 'sune f1 f2))
  ((f1 f2 f3) (tuple 'sune f1 f2 f3)))

;; Testing aliases (that match)
(defun a (x y)
  (case (list x y)
    (('a 4) (tuple 1 'a 4))
    ((= ('a _) (p1 . ps)) (tuple 2 'a p1 ps))
    ((= (tuple 'a _) (tuple p1 p2)) (tuple 2 'a p1 p2))
    ((_ _) (tuple 3 'anything))))

;; Testing aliases (that don't match)
;; (define (a-1 x y)
;;   (case (list x y)
;;     (('a 4) (tuple 1 'a 4))
;;     ((= ('a _) (tuple p1 ps)) (tuple 2 'a p1 ps))
;;     ((= #(a _) (tuple p1 p2)) (tuple 2 'a p1 p2))
;;     ((= #(a _) (tuple p1 p2 p3)) (tuple 2 'a p1 p2 p3))
;;     ((_ _) (tuple 3 'anything))))

(defun b
  (('a 4) (tuple 1 'a 4))
  (('a _) (tuple 2 'a 'anything))
  ((_ _) (tuple 3 'anything)))

;; Macro expansion in patterns.
(defun c (x y)
  (let ((foo (make-foo x y)))
    (case foo
      ((pat-foo m n) (tuple foo m n)))))

(defun d
  (((pat-foo m n) x) (tuple m n x)))

;; Testing patterns and guards in let.
(defun e (x y)
  (let (((p . ps) (when (is_number p)) (b x y))
	(q (when (is_atom q)) (c x y)))
    (d p q)))
