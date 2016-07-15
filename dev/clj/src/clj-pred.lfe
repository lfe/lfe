(defmodule clj-p
  (export all))

(defun string? (data)
  (io_lib:printable_list data))

(defun unicode? (data)
  (io_lib:printable_unicode_list data))

(defun list? (data)
  (and (is_list data) (not (string? data))))

(defun tuple? (data)
  (is_tuple data))

(defun atom? (data)
  (is_atom data))

(defun binary? (data)
  (is_binary data))

(defun bitstring? (data)
  (is_bitstring data))

(defun bool? (data)
  (is_boolean data))

(defun float? (data)
  (is_float data))

(defun function? (data)
  (is_function data))

(defun function? (data arity)
  (is_function data arity))

(defun func? (data)
  (is_function data))

(defun func? (data arity)
  (is_function data arity))

(defun integer? (data)
  (is_integer data))

(defun int? (data)
  (is_integer data))

(defun number? (data)
  (is_number data))

(defun record? (data record-tag)
  (is_record data record-tag))

(defun record? (data record-tag size)
  (is_record data record-tag size))

(defun reference? (data)
  (is_reference data))

(defun map? (data)
  (if (erl_internal:bif 'is_map 1)
      (call 'erlang 'is_map data)
      'false))

(defun set? (x)
  (or (sets:is_set x)
      (ordsets:is_set x)))

(defun dict?
  ((data) (when (=:= 'dict (element 1 data)))
    'true)
  ((_)
    'false))

(defun proplist?
  ((data) (when (is_list data))
    (if (lists:all #'proplist-kv?/1 data)
      'true
      'false))
  ((_)
    'false))

(defun proplist-kv?
  ((`#(,key ,_)) (when (is_atom key))
    'true)
  ((bool-key) (when (is_atom bool-key))
    'true)
  ((_)
    'false))

(defun undefined? (x)
  (=:= x 'undefined))

(defun undef? (x)
  (=:= x 'undefined))

(defun nil? (x)
  (or (=:= x 'nil)
      (=:= x '())))

(defun true? (x)
  (=:= x 'true))

(defun false? (x)
  (=:= x 'false))

(defun odd? (x)
  (=:= 1 (rem x 2)))

(defun even? (x)
  (=:= 0 (rem x 2)))

(defun zero? (x)
  (=:= 0 x))

(defun pos? (x)
  (> x 0))

(defun neg? (x)
  (< x 0))

(defun identical? (x y)
  (=:= x y))

(defun empty? (x)
  (=:= x '()))

(defun every? (pred x)
  (lists:all pred x))

(defun all? (pred x)
  (lists:all pred x))

(defun any? (pred x)
  (lists:any pred x))

(defun not-any? (pred x)
  (not (lists:any pred x)))

(defun element?
  ((element x) (when (is_list x))
    (any? (lambda (y) (identical? element y)) x))
  ((element x)
    (cond
      ((sets:is_set x)
        (sets:is_element element x))
      ((ordsets:is_set x)
        (ordsets:is_element element x))
      ('true 'false))))