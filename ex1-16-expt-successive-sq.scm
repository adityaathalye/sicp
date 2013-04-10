; Ex. 1-16 Iterative exponentiation 
    ; Process that grows by theta(log n) steps in constant space.

(define (successive-sq b n)
    (if (= n 0) 
        1
        (successive-iter 1 b n)))
    
(define (successive-iter a b n)
    ; Display statement only to visually verify that the process 
    ; grows by theta(log n) steps
    (display successive-iter)
    (display " ")
    (cond ((= n 1) a)
          ((even? n) (successive-iter (* a (square b)) b (/ n 2)))
          (else (successive-iter (* a b) b (- n 1)))))
          
(define (even? n)
    (= (remainder n 2) 0))
    
(define (square n) (* n n))