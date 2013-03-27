; Ex. 1.28 Miller-Rabin test for prime numbers, that cannot be 
; fooled by numbers like Carmichael numbers. This relies on a 
; modified form of Fermat's little theorem which states that:
; if n is prime
; and 0 < a < n
; then a^(n-1) modulo n is congruent to 1 modulo n

(define (fast-prime? n times)
    (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

; Note how the argument passed to try-it has been changed. This is
; required to ensure that not only is the value non-zero, but also
; that it is at most equal to (- n 1).
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

; (define (square n)
    ; (* n n))

; By redefining squaer as follows, we get a square-check operator 
; that, when used in the expmod procedure will "wait" for a value 
; to be generated. However, this will increase the number of
; deferred computations in the expmod procedure.
(define (square-check x m)
    (if (and (not (or (= x 1) (= x (- m 1))))
             (= (remainder (* x x) m) 1))
        0
        (remainder (* x x) m)))

(define (even? n)
    (= (remainder n 2) 0))
    
    
; Sample output:

; (fast-prime? 1 100)
; ;Value: #t

; (fast-prime? 2 100)
; ;Value: #t

; (fast-prime? 3 100)
; ;Value: #t

; (fast-prime? 4 100)
; ;Value: #f

; (fast-prime? 5 100)
; ;Value: #t

; (fast-prime? 6 100)
; ;Value: #f

; (fast-prime? 7 100)
; ;Value: #t

; (fast-prime? 8 100)
; ;Value: #f

; (fast-prime? 9 100)
; ;Value: #f

; (fast-prime? 10 100)
; ;Value: #f


; ; Carmichaels Numbers:

; (fast-prime? 561 100)
; ;Value: #f

; (fast-prime? 1105 100)
; ;Value: #f

; (fast-prime? 1729 100)
; ;Value: #f

; (fast-prime? 2465 100)
; ;Value: #f

; (fast-prime? 2821 100)
; ;Value: #f

; (fast-prime? 6601 100)
; ;Value: #f