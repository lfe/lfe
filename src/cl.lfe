(defmodule cl
  (export all))

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
  (cons x '())

(defun xcons (x y)
  (cons y x))

(defun nth
  ((n xs) (when (or (< n 0) (>= n (length xs))))
   '())
  ((n xs)
   (lists:nth (+ n 1) xs)))

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

(defun notevery (pred sx)
  (not (lists:all pred xs))

(defun notany (pred xs)
  (not (lists:all pred xs)))

(defun adjoin (a xs)
  (case (lists:member a xs)
    ('true xs)
    ('false (cons a xs))))

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

