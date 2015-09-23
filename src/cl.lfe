(defmodule cl
  (export all))

(defun butlast (xs)
  (lists:droplast xs))

(defun nthcdr (n xs)
  (lists:nthtail n xs))

(defun mapcar (func xs)
  (lists:map func xs))

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
