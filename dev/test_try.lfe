;; File    : test_try.lfe
;; Author  : Robert Virding
;; Purpose : Test cases for catch and try.

(defmodule test_try
  (export (a 2) (b 1) (b 2) (c 2) (d 2) (e 2) (f 2) (g 2) (h 2))
  (import (from lists (reverse 1) (reverse 2))
          (from ordsets (is_element 2))))

(defun a (x y) (catch (+ x y) (b y)))

(defun a-1 (x) (list 'x x))

;; Testing just catch.
(defun b (x y)
  (try
      (progn
        (yyy x y)
        (zzz y))
    (catch
      ;; Pattern MUST be tuple of 3 elements here!
      ;; (tuple TYPE VALUE IGNORE-THIS)
      ((tuple 'error n o) (tuple 'this-is-error n))
      ((tuple 'throw n o) (tuple 'this-is-throw n))
      ((tuple _ n o) (tuple 'this-is-default n)))))

;; Testing using case and catch.
(defun c (x y)
  (try (progn
         (yyy x y)
         (zzz y))
    (case
        ('sune #(value sune))
      ('bert #(value bert)))
    (catch
      ;; Pattern MUST be tuple of 3 elements here!
      ;; (tuple TYPE VALUE IGNORE-THIS)
      ((tuple 'error n o) (tuple 'this-is-error n))
      ((tuple 'throw n o) (tuple 'this-is-throw n))
      ((tuple _ n o) (tuple 'this-is-default n)))))

(defun d (x y)
  (try (progn
         (yyy x y)
         (zzz y))
    (after )))

;; Testing just after.
(defun e (x y)
  (try
    (progn
      (yyy x y)
      (zzz y))
    (after (yyy 'this-is-after (list x y))
      'this-is-after)))

;; Testing using case and after.
(defun f (x y)
  (try
      (progn
        (yyy x y)
        (zzz y))
    (case
        ('sune #(value sune))
      ('bert #(value bert)))
    (after (yyy 'this-is-after (list x y))
      'this-is-after)))

;; Testing using catch and after.
(defun g (x y)
  (try
      (progn
        (yyy x y)
        (zzz y))
    (catch
      ;; Pattern MUST be tuple of 3 elements here!
      ;; (tuple TYPE VALUE IGNORE-THIS)
      ((tuple 'error n o) (tuple 'this-is-error n))
      ((tuple 'throw n o) (tuple 'this-is-throw n))
      ((tuple _ n o) (tuple 'this-is-default n)))
    (after (yyy 'this-is-after (list x y))
      'this-is-after)))

;; Testing using all case, catch and after.
(defun h (x y)
  (try
      (progn
        (yyy x y)
        (zzz y))
    (case
        ('sune #(value sune))
      ('bert #(value bert)))
    (catch
      ;; Pattern MUST be tuple of 3 elements here!
      ;; (tuple TYPE VALUE IGNORE-THIS)
      ((tuple 'error n o) (tuple 'this-is-error n))
      ((tuple 'throw n o) (tuple 'this-is-throw n))
      ((tuple _ n o) (tuple 'this-is-default n)))
    (after (yyy 'this-is-after (list x y))
      'this-is-after)))

(defun b (x) '"a string")

(defun yyy (x y)
  (: io fwrite '"(yyy ~w ~w)\n" (list x y)))

(defun zzz (x)
  (: foo bar x))
