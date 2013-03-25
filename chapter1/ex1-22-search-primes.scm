; Ex. 1.22 Search for Primes

; This searches for prime numbers from a lower bound to an upper 
; bound that is limited by the number of primes to be found.
(define (search-primes from how-many)
    (cond ((= how-many 0) 
        (newline)
        (display "Done"))
        ((even? from)
            (search-primes (+ from 1) how-many))
        ((prime? from) ; This feels redundant...
            (timed-prime-test from)
            (search-primes (+ from 2) (- how-many 1)))
        (else 
            (search-primes (+ from 2) how-many))))

; In the procedure below, applying "runtime" returns an value 
; that specifies the amount of time the system has been running.
(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime))) 
    
(define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))))
        
(define (report-prime elapsed-time)
    (display "***")
    (display (* elapsed-time 1000.0)))

(define (prime? n)
    (= n (smallest-divisor n)))
    
(define (smallest-divisor n)
    (find-divisor n 2))
    
(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
    (= (remainder b a) 0))

(define (square x)
    (* x x))

(define (even? n)
    (= (remainder n 2) 0))