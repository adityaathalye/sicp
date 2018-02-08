;; Ex 2.40 and 2.41
;; Note: Utils and helpers like `prime?' and `enumerate-interval' at the bottom


;; EX 2.40 Write `unique-pairs' and use it to simplify `prime-sum-pairs'

(define (unique-pairs i)
  (map (lambda (j) (list i j))
       (enumerate-interval 1 i)))

(flatmap unique-pairs
         (enumerate-interval 1 5))


(define (prime-sum-pairs n)
  (map make-prime-sum
       (filter prime-sum?
               (flatmap
                unique-pairs
                (enumerate-interval 1 n)))))

(prime-sum-pairs 5)


;; Ex 2.41
;; Write a procedure to find all ordered triples of distinct positive integers
;; i , j , and k less than or equal to a given integer n that sum to
;; a given integer s.

(define (filter-triples n s triples)
  (define (ordered-and-bounded? triple)
    (> (+ n 1)
       (caddr triple) ; k
       (cadr triple)  ; j
       (car triple)   ; i
       0))

  (define (totals-to-s? triple)
    (= s (fold-left + 0 triple)))

  (define (legal-triple? triple)
    (and (ordered-and-bounded? triple)
         (totals-to-s? triple)))

  (filter legal-triple? triples))


(define triples
  (list (list 1 2 7) (list 1 3 6) (list 1 4 5) (list 2 3 5)
        ;; The following must not appear in the result
        (list 1 7 2) (list 1 6 3) (list 5 4 1) (list 3 2 5)
        (list 1 9 0)
        (list 2 2 6)
        (list 9 9 9)))

(filter-triples 9 10 triples)
(filter-triples 5 10 triples)
(filter-triples 4 10 triples)



;; =================== utils and helper fns for this file ===================

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(filter prime-sum?
        (flatmap unique-pairs
                 (enumerate-interval 1 5)))


(define (make-prime-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (enumerate-interval from to)
  (define (ei r from to)
    (if (> from to)
        r
        (ei (cons to r) from (- to 1))))
  (if (< from to)
      (ei '() from to)
      (ei '() to from)))

(enumerate-interval 1 10)


(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))


(flatmap list '(1 2 3 4 5))


(define (prime? n)
  (fast-prime? n 100))

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
