; Ex. 1.23 Search for Primes with modified test-divisor procedure

; This searches for prime numbers from a lower bound to an upper 
; bound that is limited by the number of primes to be found.
(define (search-primes from how-many)
    (cond ((= how-many 0) 
        (newline)
        (display "Done"))
        ((even? from)
            (search-primes (+ from 1) how-many))
        ((fast-prime? from 10) ; This feels redundant...
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
    (if (fast-prime? n 10)
        (report-prime (- (runtime) start-time))))
        
(define (report-prime elapsed-time)
    (display "***")
    (display (* elapsed-time 1000.0)))

; (define (prime? n)
    ; (= n (smallest-divisor n)))

(define (fast-prime? n times)
    (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
    (cond ((= exp 0) 1)
        ((even? exp)
            (remainder (square (expmod base (/ exp 2) m)) m))
        (else
            (remainder (* base (expmod base (- exp 1) m)) m))))
    
(define (smallest-divisor n)
    (find-divisor n 2))
    
; Modified this procedure to use "next" as per the question
(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
    (= (remainder b a) 0))

(define (square n)
    (* n n))

; Added next as per the question. It can be more generalised, but 
; not doing that over here as it gives not extra computational 
; benefit for the purpose of this question.
(define (next n)
    (if (= n 2)
        3
        (+ n 2)))
        
(define (even? n)
    (= (remainder n 2) 0))