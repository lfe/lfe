(defmodule cl
  (export all))

;;; Lists

(defun car
  (('())
   '())
  ((xs)
   (lists:nth 1 xs)))

(defun first (xs)
  (cl:car xs))

(defun cdr
  (('())
   '())
  ((xs)
   (lists:nthtail 1 xs)))

(defun rest (xs)
  (cl:cdr xs))

(defun ncons (x)
  (cons x '()))

(defun xcons (x y)
  (cons y x))

(defun nth
  ((n xs) (when (or (< n 0) (>= n (length xs))))
   '())
  ((n xs)
   (lists:nth (+ n 1) xs)))

(defun elt
  ((n xs) (when (is_list xs))
   (nth n xs))
  ((n xs) (when (is_tuple xs))
   (element (+ n 1) xs)))

(defun aref (array i j)
  (elt j (elt i array)))

(defun getf (plist key)
  (case (lists:keyfind key 1 plist)
   ('false 'undefined)
   (`#(,k ,v) v)))

(defun position (x xs)
  (position x 0 xs))

(defun position
  ((x _ '()) 'undefined)
  ((x pos `(,head . ,tail))
   (if (== x head)
       pos
       (position x (+ pos 1) tail))))

(defun butlast (xs)
  (lists:droplast xs))

(defun nthcdr (n xs)
  (lists:nthtail (+ n 1) xs))

(defun every (pred xs)
  (lists:all pred xs))

(defun some (pred xs)
  (lists:any pred xs))

(defun notevery (pred xs)
  (not (lists:all pred xs)))

(defun notany (pred xs)
  (not (lists:any pred xs)))

(defun adjoin (a xs)
  (case (lists:member a xs)
    ('true xs)
    ('false (cons a xs))))

(defun mapcar (func xs)
  (lists:map func xs))

;; XXX maplist flattens everything; the results need to remain grouped by
;; sublist to maintain parity with CL usage/results.

;;(defun maplist (func xs)
;;  (maplist func (mapcar func xs) xs))

;;(defun maplist
;;  ((_ acc '())
;;   acc)
;;  ((func acc `(,_ . ,xs))
;;   (maplist func (++ acc (mapcar func xs)) xs)))

(defun remove-duplicates (xs)
  (lists:usort xs))

(defun remove-if-not (func xs)
  (lists:filter func xs))

(defun reduce (func xs)
  (lists:foldl func '() xs))

(defun reduce
  ((func xs 'initial-value x)
   (lists:foldl func x xs))
  ((func xs 'from-end 'true)
   (lists:foldr func '() xs)))

(defun reduce
  ((func xs 'from-end 'true 'initial-value x)
   (lists:foldr func x xs))
  ((func xs 'initial-value x 'from-end 'true)
   (lists:foldr func x xs)))

;;; Types

(defun type-of
  ((x) (when (is_boolean x))
   'boolean)
  ((x) (when (is_atom x))
   'atom)
  ((x) (when (is_tuple x))
   'tuple)
  ((x) (when (is_integer x))
   'integer)
  ((x) (when (is_float x))
   'float)
  ((x) (when (is_list x))
   (cond ((io_lib:printable_latin1_list x) 'string)
         ((io_lib:printable_unicode_list x) 'unicode)
         ((?= `(,a . ,b) (when (not (is_list b))) x) 'cons)
         ('true 'list)))
  ((x) (when (is_map x))
   'map)
  ((x) (when (is_function x))
   'function)
  ((x) (when (is_binary x))
   'binary)
  ((x) (when (is_bitstring x))
   'bitstring)
  ((x) (when (is_pid x))
   'pid)
  ((x) (when (is_port x))
   'port)
  ((x) (when (is_reference x))
   'reference))

(defun coerce
  ((x 'vector) (when (is_list x))
   (list_to_tuple x))
  ((x 'tuple) (when (is_list x))
   (list_to_tuple x))
  ((x 'atom) (when (is_list x))
   (list_to_atom x))
  ((x 'list) (when (is_atom x))
   (atom_to_list x))
  ((x 'list) (when (is_tuple x))
   (tuple_to_list x))
  ((x 'list) (when (is_binary x))
   (binary_to_list x))
  ((x 'list) (when (is_bitstring x))
   (bitstring_to_list x))
  ((x 'character) (when (is_atom x))
   (car (atom_to_list x)))
  ((x 'character) (when (is_list x))
   (car x))
  ((x 'integer) (when (is_float x))
   (trunc x))
  ((x 'float) (when (is_integer x))
   (list_to_float (integer_to_list x)))
  ((x 'float) (when (is_list x))
   (list_to_float x))
  ((x 'float) (when (is_atom x))
   (list_to_float (atom_to_list x)))
  ((x 't)
   x))

;;; System

(defun posix-argv ()
  (init:get_arguments))
