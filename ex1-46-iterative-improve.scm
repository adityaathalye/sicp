; Ex. 1.46 General-purpose iterative improvement procedure

(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (display guess)
    (newline)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve) 
         (improve guess)))))

; Another way to generate the iteration
; (define (iterative-improve good-enough? improve)
  ; (lambda (guess)
    ; (define (iterator guess)
        ; (display guess)
        ; (newline)
        ; (if (good-enough? guess)
            ; guess
            ; (iterator (improve guess))))
    ; (iterator guess)))
        
(define (square-root x)
  ((iterative-improve 
      ; good-enough? 
      (lambda (y) (< (abs (- y (average y (/ x y)))) 0.000000001))
      ;(lambda (y) (< (abs (- (square y) x)) 0.0000001)) 
        ; does not converge in all cases.
      ; improve
      (lambda (y) (average y (/ x y))))
    ; 1.0 is the initial guess, specific to this procedure
    1.0))

(define (average x y) (/ (+ x y) 2))

(define (square a) (* a a))

