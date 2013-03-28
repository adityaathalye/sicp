; Ex. 1.29 Simpsons Rule for numerical integration

; DID NOT PEEK!!!!!! MY LOGIC WORKED THE FIRST TIME!!!!!!!!
; SO GLAD I did NOT peek :)))

(define (simpsons-rule f a b n)

    (define (next m) (+ m h))

    (define h 
        (/ (- b a) (evenify n)))

    (define (term x) 
        (cond ((= (/ x h) (evenify n))
                (* 1.0 (f (+ a x))))
              ((and (even? (/ x h)) (<= (/ x h) (evenify n)))
                (* 2.0 (f (+ a x))))
              (else
                (* 4.0 (f (+ a x))))))
                
    (if (<= n 0) 
        "Cannot compute"
        (* (/ h 3.0) (+ (f a) (sum term h next (- b a))))))

    
(define (sum term x next y)
    (if (> x y)
        0
        (+ (term x)
           (sum term (next x) next y))))
    
    
(define (evenify x)
    (if (even? x) x (+ x 1)))

    
(define (even? x)
    (= (remainder x 2) 0))
    
