; Ex. 1.33 Filtered accumulate procedure

; This definition of filtered-acccumulate works but it cannot be 
; used as a black-box abstraction. It forces the user to learn 
; how filtered-accumulate internally applies 'filter', in order
; to design a custom filter for the problem at hand.
(define (filtered-accumulate filter combiner
                             null-value term a next b)
        (define (iter a result)
            (cond ((> a b) result)
                  ((filter a) 
                   (iter (next a) (combiner (term a) result)))
                  (else (iter (next a) result))))
        (iter a null-value))
        
; Ex. 1.33 Part A:
; Compute sum of squares of PRIMES between a and b using a known
; filter for checking primes

(define (square x) (* x x))

(define (next x) (+ x 1))

(define (prime? x) (fast-prime? x 100))

(define (sum-square-primes a b)
    (filtered-accumulate prime? + 0 square a next b))


; Ex. 1.33 Part B:    
; Product of all positive integers less than n that are relatively
; prime to n (i.e. all integers 0 < i < n such that GCD(i,n) = 1)

; (define (identify x) x)

; (define (rel-prime? x y)
;         ((gcd x y) = 1))

; (define (multiply-relative-primes n)
    ; (filtered-accumulate rel-prime? * 1 identify a next b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test Data and Results 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FOR PART A: Sum of Squares of Primes in a range
; ; (sum-square-primes 1 2)
; ; ;Value: 5

; ; (sum-square-primes 2 3)
; ; ;Value: 13

; ; (sum-square-primes 3 4)
; ; ;Value: 9

; ; (sum-square-primes 4 5)
; ; ;Value: 25

; ; (sum-square-primes 5 10)
; ; ;Value: 74

; ; (sum-square-primes 1 10)
; ; ;Value: 88

; ; (sum-square-primes 1 7)
; ; ;Value: 88





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The fast-prime? procedure, below, is copied from Ex. 1.28, and
; uses the Miller-Rabin test to check for primality.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fast-prime? n times)
    (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))
        
        
(define (miller-rabin-test n)
    (define (try-it a)
        ; n is not prime if a^(n-1) mod n equals 1 mod n
        (= (expmod a (- n 1) n) 1)) ; false if n is not prime
    (if (or (= n 1) (= n 2))
        true
        (try-it (+ 2 (random (- n 2)))))) ; any a where 0 < a < n

        
(define (expmod base exp m)
    (cond ((= exp 0) 1)
        ((even? exp)
            (square-check (expmod base (/ exp 2) m) m))
        (else
            (remainder (* base (expmod base (- exp 1) m)) m))))

            
(define (square-check x m)
    (if (and (not (or (= x 1) (= x (- m 1))))
             (= (remainder (* x x) m) 1))
        0
        (remainder (* x x) m)))

        
(define (even? n)
    (= (remainder n 2) 0))

