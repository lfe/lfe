(defmodule test_map
  (export all))

(defun literal ()
  #m(a 1 b (2 3 4) c #(a b)))

(defun make (x y z)
  (tuple (map 'a x 'b y 'c (tuple 'ok (: test_map foo z)))
	 (map 'b y 'c (tuple 'ok (: test_map foo z)) 'a x)
	 (map #b(1 2 3) x)
	 (map #b((1 (size 16)) ("åäö" utf-8)) y)))

;; (defun get (map)
;;   (get-map map 'a))

;; (defun set (map v1 v2)
;;   (set-map map 'a v1 'b (: test_map foo v2)))

;; (defun update (map v1 v2)
;;   (update-map map 'a v1 'b (: test_map foo v2)))

;; (defun guard
;;   ([map x] (when (== (map 'a x) map)) 1)
;;   ([map x] (when (== (set-map map 'a 1) x)) 2)
;;   ([map x] (when (== (update-map map 'a 1) x)) 3))

(defun get (map)
  (mref 'a map))

(defun set (map v1 v2)
  (mset 'a v1 'b (: test_map foo v2) map))

(defun update (map v1 v2)
  (mupd 'a v1 'b (: test_map foo v2) map))

(defun guard
  ([map x] (when (== (map 'a x) map)) 1)
  ([map x] (when (== (mset 'a 1 map) x)) 2)
  ([map x] (when (== (mupd 'a 1 map) x)) 3))

(defun match
  ([(map 'a x)] x)
  ([(map 'a 1 'b y)] y)
  ([(map 'c (tuple x y))] (tuple x y))
  ([(map #(d e) z)] z))

(defun foo (x)
  (list x x))
