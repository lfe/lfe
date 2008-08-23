(define-module test_sr
  (export (a 1) (b 1) (c 1)))

(define-record point x y)

(define-syntax allan
  (syntax-rules
    (() '(0))
    ((a) '(1 a))
    ((a b) '(2 a b))
    ((((a b) ...) e ...) '(2 (lambda (a ...) e ...) (b ...)))))

(define-syntax dodo
  (syntax-rules
    ([((a b c) ...) (t v) e ...]
     [letrec ((-sune-
	       (lambda (a ...)
		 (if t v
		     (begin e ... (-sune- c ...))))))
       (-sune- b ...)])))

(define-syntax aaa
  (macro
   (() '(0))
   ((a) '(1 a))
   ((a b) '(2 a b))))

(define (a x)
  (aaa x))

(define-syntax bbb
  (syntax-rules
    ([a b ...] '(a b ...))))

(define (b x)
  (bbb x y z))

(define-syntax ccc
  (syntax-rules
    ([(tuple (a b) ...)] (cons (list a ...) (tuple b ...)))))

(define (c x)
  (ccc #((m m1) (n n1) (o o1)))
  (ccc (tuple (m m1) (n n1) (o o1))))

;; (define (d r)
;;   (case r
;;     ((match-point x y) (list x y))))

;; (define (e-1 m)
;;   (dodo ((i 1 (+ 1 i))
;; 	 (acc 1 (* acc i)))
;; 	((> i m) acc)
;; 	'(i)))

;; (define (e-2 m)
;;   (do ((l m (cdr l))
;;        (acc 0 (+ acc 1)))
;;       ((== l ()) acc)
;;     (list l acc)))
