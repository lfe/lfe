(begin
(define-module test_macro
  (export (a 1) (ae 2) (af 2) (t1 2) (d1 2) (d2 2)))
)

(define-syntax let@
  (macro
    (((vb . vbs) . b) `(let (,vb) (let@ ,vbs . ,b)))
    ((() . b) `(begin . ,b))))

(define-syntax let&
  (syntax-rules
    ([(vb . vbs) . b] [let (vb) (let& vbs . b)])
    ([() . b] [begin . b])))

(define-syntax and-also
  (macro
    ((e) e)
    ((e . es)
      `(case ,e
         ('true (and-also . ,es))
	 ('false 'false)))
    (() `'true)))

;; Top-level begins are legal and can be used when generating many
;; forms from one form. E.g. records.
(begin
  (define-syntax c-ond
    (syntax-rules
      ([('else . b)] (begin . b))
      ([(('match p e) . b) . c] (case e (p . b) (_ (cond . c))))
      ([(('match p g e) . b) . c] (case e (p g . b) (_ (cond . c))))
      ([(t . b) . c] (if t (begin . b) (cond . c)))
      ([] 'false)))
  
  (define (aa x y)
    (let& ((o (e x))
	   (p (e-1 y)))
	  (tuple o p)))
  )

(define (ab x y)
  (let@ ((o (e x))
	 (p (e-1 y)))
	(tuple o p)))

(define (ac x y)
  (and-also (e x) (e-1 y) (aa x y))
  (and-also))

(define (ad x y)
  (andalso (e x) (e-1 y) (aa x y))
  (andalso))

(define (ae x y)
  (c-ond ((p-1 x) (e-1 y) (list x y))
	 ((match (p . ps) (e-1 x))	;Match (p . ps) or fail
	  (list p ps))
	 ((p-2 x) (e y) (tuple x y))
	 (else (e y))))

(define (af x y)
  (cond ((p-1 x) (e-1 y) (list x y))
	((match (p . ps) (e-1 x))	;Match (p . ps) or fail
	 (list p ps))
	((p-2 x) (e y) (tuple x y))
	(else (e y))))

(define (e x) (list 'e x))

(define (e-1 x) (list 'e-1 x))

(define (p-1 x) (is_integer x))

(define (p-2 x) (is_atom x))

(define-record person name age)

(define (a age)
  (let ((p (make-person '"Sune" 100)))
    (set-person-age p age)))

(define (b x y)
  (e x)
  (let-syntax ((e (syntax-rules ((x) (e-1 x)))))
    (e x)
    (fletrec ((e (lambda (a) (aa a a))))
      (e x))))

(define-syntax tmac
  (macro
    ((e) `(tuple 'ok ,e))
    ((e . es) `(tuple 'ok (tuple ,e . ,es)))))

(define (t1 x y)
  (list (tmac x) (tmac x 1 y 2)))

(define-syntax d-o
  (syntax-rules
    ([((v i c) ...) (t r) b ...]
     (fletrec ((f (lambda (v ...)
		    (if t
		      r
		      (begin b ... (f c ...))))))
       (f i ...)))))

(define (d1 x y)
  (do ((n x (+ n 1))
       (m y (- m 1))
       (c 0 (+ c 1)))
      ((> n m) c)))


(define (d2 x y)
  (d-o ((n x (+ n 1))
	(m y (- m 1))
	(c 0 (+ c 1)))
       ((> n m) c)))

