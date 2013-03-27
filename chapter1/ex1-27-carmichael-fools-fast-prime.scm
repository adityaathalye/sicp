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

(define (square n)
    (* n n))

(define (even? n)
    (= (remainder n 2) 0))
    

; Sample output computed using the above procedure definition shows
; that Carmaichel numbers do fool the fast-primes test!

; (fast-prime? 561 100)
; ;Value: #t


; (fast-prime? 1105 100)
; ;Value: #t


; (fast-prime? 1729 100)
; ;Value: #t


; (fast-prime? 2465 100)
; ;Value: #t


; (fast-prime? 2821 100)
; ;Value: #t


; (fast-prime? 6601 100)
; ;Value: #t

