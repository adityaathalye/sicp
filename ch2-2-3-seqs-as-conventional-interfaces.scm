;; Chapter 2.2.3 "Sequences as conventional interfaces"


;; ====================== prime? utils for this file ================================================

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


;; ========================== end utils for this file ==========================


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


(define (gen-pairs i)
  (map (lambda (j) (list i j))
       (enumerate-interval 1 i)))

(flatmap gen-pairs
         (enumerate-interval 1 5))

(filter (lambda (pair)
          (or (= 1 (car pair)) (= 1 (cadr pair))))
        (flatmap gen-pairs
                 (enumerate-interval 1 5)))


(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(filter prime-sum?
        (flatmap gen-pairs
                 (enumerate-interval 1 5)))


(define (make-prime-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
  (map make-prime-sum
       (filter prime-sum?
               (flatmap
                gen-pairs
                (enumerate-interval 1 n)))))

(prime-sum-pairs 5)

(define (permutations s)
  (if (null? s)
      (list ())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))


(define (remove item s)
  (filter (lambda (x) (not (= x item)))
          s))

(permutations (enumerate-interval 1 3))
