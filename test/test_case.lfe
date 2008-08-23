(define-module test_case
  (export (a 2) (a-1 2) (b 2) (d-1 2) (d-2 2))
  ;; We can have any attributes
  (compile export_all)
  (compiler lfe_comp))

(define (a x y)
  (if (> (length x) y) 'yes 'no))

(define a-1
  (lambda (x y)
     (if (let ((z (length x))) (> z y)) 'yes 'no)))

(define (b x y)
  (if (andalso (is_integer x) (> (+ x 1) y)) 'yes 'no))

(define (c x y)
  (case (bbb x)
    ((tuple 'ok z) (when (> z 5)) (bbb z))
    ((tuple 'ok z) (bbb z))))

;; This case is not optimised by the core compiler.
(define (d-1 x y)
  (case (list x y)
    (('a 1) (tuple 'yes 1))
    (('a 2) (tuple 'yes 2))
    (('b _) 'no)))

;; This case is optimised by the core compiler.
(define (d-2 x y)
  (case (tuple x y)
    ((tuple 'a 1) (tuple 'yes 1))
    ((tuple 'a 2) (tuple 'yes 2))
    ((tuple 'b _) 'no)))

(define (bbb x)
  (tuple 'ok x))
