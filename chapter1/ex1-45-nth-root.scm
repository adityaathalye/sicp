; Ex. 1.45 N-th root

; The experiments I ran, to discover how many average damping 
; operations need to be applied as n grows, showed that the 
; former number is proportion to the natural logarithm of n.
; I've added a margin of safety--multiply log(n) by 2--that I hope 
; will provide coverage for the full range of real numbers my PC 
; can process.

(define (nth-root fx n)
  (fixed-point 
   ((repeated average-damp (* (round (log n)) 2))
    (lambda (y) (/ fx (expt y (- n 1)))))
   1.0))

(define (average-damp f)
    (lambda (x) (average x (f x))))

; Procedure to repeatedly apply a function

(define (repeated f n)
    (define (recurse result count)
        (if (= count n)
            result
            (recurse (compose f result) (+ count 1))))
    (recurse f 1))

(define (compose f g)
    (lambda (x) (f (g x))))

(define (average x y)
    (/ (+ x y) 2))

    
; Fixed-point function

(define tolerance 0.00001)


(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next))))
    (try first-guess))


; From Ex. 1-16 Iterative exponentiation process that grows by 
; theta(log n) steps in constant space.

(define (successive-sq b n)
    (if (= n 0) 
        1
        (successive-iter 1 b n)))

(define (successive-iter a b n)
    (cond ((= n 1) a)
          ((even? n) (successive-iter (* a (square b)) b (/ n 2)))
          (else (successive-iter (* a b) b (- n 1)))))
          
(define (even? n)
    (= (remainder n 2) 0))
    
(define (square n) (* n n))