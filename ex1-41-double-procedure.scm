; Ex. 1.41 Double procedure

(define (double f)
    (lambda (x) (f (f x))))
    
(define (inc x) (+ x 1))