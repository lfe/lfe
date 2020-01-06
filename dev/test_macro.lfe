(progn
(defmodule test_macro
  (export (a 1) (ae 2) (af 2) (t1 2) (d1 2) (d2 2)))
)

(defmacro let@
  ((cons (cons vb vbs) b) `(let (,vb) (let@ ,vbs . ,b)))
  ((cons () b) `(begin . ,b)))

(defsyntax let&
  ([(vb . vbs) . b] [let (vb) (let& vbs . b)])
  ([() . b] [begin . b]))

(defmacro and-also
  ((e) e)
  ((e . es)
   `(case ,e
      ('true (and-also . ,es))
      ('false 'false)))
  (() `'true))

;; Top-level progns are legal and can be used when generating many
;; forms from one form. E.g. records.
(progn
  (defsyntax c-ond
    ([('else . b)] (begin . b))
    ([(('?= p e) . b) . c] (case e (p . b) (_ (c-ond . c))))
    ([(('?= p g e) . b) . c] (case e (p g . b) (_ (c-ond . c))))
    ([(t . b) . c] (if t (begin . b) (c-ond . c)))
    ([] 'false))

  (defun aa (x y)
    (let& ((o (e x))
       (p (e-1 y)))
      (tuple o p)))
  )

(defun ab (x y)
  (let@ ((o (e x))
         (p (e-1 y)))
        (tuple o p)))

(defun ac (x y)
  (and-also (e x) (e-1 y) (aa x y))
  (and-also))

(defun ad (x y)
  (andalso (e x) (e-1 y) (aa x y))
  (andalso))

(defun ae (x y)
  (c-ond ((p-1 x) (e-1 y) (list x y))
     ((?= (cons p ps) (e-1 x))          ;Match (p . ps) or fail
      (list p ps))
     ((p-2 x) (e y) (tuple x y))
     (else (e y))))

(defun af (x y)
  (cond ((p-1 x) (e-1 y) (list x y))
        ((?= (p . ps) (e-1 x))          ;Match (p . ps) or fail
         (list p ps))
        ((p-2 x) (e y) (tuple x y))
        (else (e y))))

(defun e (x) (list 'e x))

(defun e-1 (x) (list 'e-1 x))

(defun p-1 (x) (is_integer x))

(defun p-2 (x) (is_atom x))

(defrecord person name age)

(defun a (age)
  (let ((p (make-person name '"Sune" age 100)))
    (list p
      (set-person-age p age)
      (set-person p name '"Kurt" age 53))))

(defun b (x y)
  (e x)
  (let-syntax ((e (syntax-rules ((x) (e-1 x))))) ;Old style still valid
    (e x)
    (fletrec ((e (a) (aa a a)))
      (e x))))

(define-syntax tmac                     ;Old style still valid
  (macro
    ((e) `(tuple 'ok ,e))
    ((e . es) `(tuple 'ok (tuple ,e . ,es)))))

(defun t1 (x y)
  (list (tmac x) (tmac x 1 y 2)))

(define-syntax d-o                      ;Old style still valid
  (syntax-rules
    ([((v i c) ...) (t r) b ...]
     (fletrec ((f (v ...)
          (if t
            r
            (progn b ... (f c ...)))))
       (f i ...)))))

(defun d1 (x y)
  (do ((n x (+ n 1))
       (m y (- m 1))
       (c 0 (+ c 1)))
      ((> n m) c)))

(defun d2 (x y)
  (d-o ((n x (+ n 1))
    (m y (- m 1))
    (c 0 (+ c 1)))
       ((> n m) c)))

;; Some macro calls which will generate errors!
(defun xx1 (x y)
  (let@ 1 2 x))

(progn
  (progn
    (defun xx2 (x y)
      (let& 1 x))
    )
  (defun xx3 (x y)
    (let* 1 2 x))
  )
