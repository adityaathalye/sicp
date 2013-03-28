; Ex. 1.29 Simpsons Rule for numerical integration

; DID NOT PEEK!!!!!! MY LOGIC WORKED THE FIRST TIME!!!!!!!!
; SO GLAD I did NOT peek :)))

(define (simpsons-rule f a b n)
    ; n must always be even
    (define n-even
        (if (even? n) n (+ n 1)))

    ; h is the smallest increment
    (define h 
        (/ (- b a) n-even))

    ; Apply the correct multiplier to each yk to create 
    ; a single summable term
    (define (term x)
        (* (cond ((= (/ x h) n-even) 1.0)
                 ((even? (/ x h)) 2.0)
                 (else 4.0))
           (f (+ a x))))
           
    ; The transform that 'sum' uses to increment each new yk term
    (define (next m) 
        (+ m h))

    ; Invoke the computation using the 'sum' procedure. Note that 
    ; the 0-th term is added as the logic starts computing from
    ; the 1-st term.
    (if (<= n 0) 
        "Cannot compute"
        (* (/ h 3.0) (+ (f a) (sum term h next (- b a))))))

; Sum of series limited by x and y
(define (sum term x next y)
    (if (> x y)
        0
        (+ (term x)
           (sum term (next x) next y))))
    
(define (even? x)
    (= (remainder x 2) 0))