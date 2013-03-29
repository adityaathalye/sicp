(define (product term a next b)
    (define (product-iter a result)
        (if (> a b)
            1
            (next a))
        (* result a))
    (product-iter a 1))
        
; Factorial n is 1.(1+1).(2+1)...((n-1) + 1)
; So, the k-th term is ((k-1)+ 1)

(define (n! n)
    (define (identify x) x)
    (define a 1)
    (define (next x) (1+ x))
    
    (cond ((= n 0) 1)
          ((= n 1) 1)
          (else (product identify a next n))))
          
          