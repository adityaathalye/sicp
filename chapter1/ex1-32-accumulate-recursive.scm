; Ex. 1.32 Accumulate procedure that generates a recursive process

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) 
                (accumulate 
                    combiner null-value term (next a) next b))))
                    
(define (term x) x)
    
(define (next x) (+ x 1))
                    
(define (sum-series a b)    
    (accumulate + 0 term a next b))
    
(define (product-series a b)
    (accumulate * 1 term a next b))