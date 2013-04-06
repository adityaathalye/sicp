; Ch. 1.3.4 Abstractions and first-class procedures

; A general method that finds the fixed point of some 
; transformation of the function.

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
  
(define (sqrt x)
    (fixed-point-of-transform (lambda (y) (/ x y))
                              average-damp
                              1.0))
                              
(define (cube-root x)
    (fixed-point-of-transform (lambda (y) (/ x (square y)))
                              average-damp
                              1.0))

                              
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Prerequisite code

; Average damping can be expressed as:
(define (average-damp f)
    (lambda (x) (average x (f x))))

    
; tolerance intended for fixed-point procedure
(define tolerance 0.00001)

; procedure to compute fixed-point f(x) = x
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
          (display "Guess: ")
          (display guess)
          (newline)
          (display "Next: ")
          (display next)
          (newline)
          (if (close-enough? guess next)
              next
              (try next))))
    (try first-guess))

    
(define (average x y) (/ (+ x y) 2))
