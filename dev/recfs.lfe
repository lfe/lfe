;; (define (f n)
;;   (if (== n 0) 1 (* n (g (- n 1)))))
;; (define (g n)
;;   (if (== n 0) 1 (* n (h (- n 1)))))
;; (define (h n)
;;   (if (== n 0) 1 (* n (f (- n 1)))))

#(f 1 (lambda (n) (call 'io 'fwrite '"f: ~p\n" (list n)) (if (== n 0) 1 (* n (f (- n 1))))))

;; #(f 1 (lambda (n) (if (== n 0) 1 (* n (g (- n 1))))))
;; #(g 1 (lambda (n) (if (== n 0) 1 (* n (h (- n 1))))))
;; #(h 1 (lambda (n) (if (== n 0) 1 (* n (f (- n 1))))))

;; #(f 1 (y12 (lambda (f) (lambda (n) (if (== n 0) 1 (* n (f (- n 1)))))))

;; (define zipper
;;   (match-lambda
;;     (('f n) (if (== n 0) 1 (* n (g (- n 1)))))
;;     (('g n) (if (== n 0) 1 (* n (h (- n 1)))))
;;     (('h n) (if (== n 0) 1 (* n (f (- n 1)))))))
