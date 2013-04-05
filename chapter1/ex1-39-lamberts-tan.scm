; Ex. 1.39. Compute tan x using Lambert's continued fraction formula
; assuming x is in radians.

(define (tan-cf-recurse x k)
    (cont-frac-recurse (lambda (i) (- (* 2 i) 1.0)) ; d
                       (lambda (i) (if (= i 1) x (- (square x)))) ; n
                       k))

                       
(define (tan-cf-iter x k)
    (cont-frac-iter (lambda (i) (- (* 2 i) 1.0)) ; d
                    (lambda (i) (if (= i 1) x (- (square x)))) ; n
                    k))
                       
                       
(define (cont-frac-recurse d n k)
  (define (recurse i)
    (/ (n i) 
       (+ (d i) 
          (if (= i k)
              0
              (recurse (+ i 1))))))
  (recurse 1))

  
(define (cont-frac-iter d n k)
    (define (iterate result count)
      (if (= count 1)
          (/ (n 1) result)
          (iterate (+ (d (- count 1)) (/ (n count) result))
                   (- count 1))))
    (iterate (d k) k))