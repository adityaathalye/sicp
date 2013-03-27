; Ex. 1.28 Miller-Rabin test for prime numbers, that cannot be 
; fooled by numbers like Carmichael numbers. This relies on a 
; modified form of Fermat's little theorem which states that:
; if n is prime
; and a > 0 and a < n
; then a^(n-1) modulo n is congruent to 1 modulo n

(define (fast-prime? n times)
    (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a (- n 1) n) a)) ; need to get a^(n - 1)
    (try-it (random (- n 1)))) ; any number a < n will do

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (cond ((or (= (expmod base (/ exp 2) m) 1)
                     (= (expmod base (/ exp 2) m) (- m 1)))
                  (remainder (square (expmod base (/ exp 2) m)) m))
                ((= (square (expmod base exp m)) 1)
                  0)
                (else
                  (remainder (square (expmod base (/ exp 2) m)) m)))
    (else
      (remainder (* base (expmod base (- exp 1) m)) m)))))
            
(define (square n)
    (* n n))

(define (even? n)
    (= (remainder n 2) 0))