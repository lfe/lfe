(defmodule shadow-test
  (export (test-1 1)
          (test-2 1)
          (test-3 1)
	  (test-4 1)
	  (test-5 1)
	  (test-6 1)
	  (test-7 1))
  (export (get 1) (car 1) (call 3))
  (import (from bert (get 0)))
  )

;; Shadowing erlang BIF get/0 with import and local function.

(defun get (x) `#(get ,x))              ;This should shadow BIF get/1

(defun test-1 (x) (get))                ;This should call bert:get/0

(defun test-2 (x) (get x))              ;This should call local get/1

(defun test-3 (x) (put x 42))           ;This should call BIF put/2

;; Shadowing core form car/1 with local function car/1.

(defun car (x) (tuple 'car x))		;This might shadow Core car/1

(defun test-4 (x) (car x))		;This should call core form car/1

(defun test-5 (x) (shadow-test:car x))	;This should call exported car/1

;; Shadow core form call with local function call/3.

(defun call (x y z)			;This might shadow core call.
  (tuple 'call x y z))

(defun test-6 (x)			;This should call core form call
  (call 'sune 'calle x))

(defun test-7 (x)			;This should call exported call/3
  (shadow-test:call 'sune 'calle x))
