; Ex. 1.32 Accumulate procedure that generates an ITERATIVE process

(define (accumulate combiner null-value term a next b)
    (define (iter a result)
            (if (> a b)
                result
                (iter (next a) (combiner result (term a)))))
    (iter a null-value))
    
(define (term x) x)
    
(define (next x) (+ x 1))
                    
(define (sum-series a b)    
    (accumulate + 0 term a next b))
    
(define (product-series a b)
    (accumulate * 1 term a next b))