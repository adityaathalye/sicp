; Ex. 1.38 Euler's Expansion
; With cont-frac (recursive) as reference,
; Ni = 1 always
; Di = 1,2,1,1,4,1,1,6,1,1,8,1,1....
; For reference: e = 2.71828183

(define (eulers-expansion cont-frac times)
  (+ 2.0
     (cont-frac (lambda (i)
                   (if (= (remainder (+ i 1) 3) 0)
                       (* (/ (+ i 1) 3) 2)
                       1)) ; d
                (lambda (i) 1) ; n
                times)))

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