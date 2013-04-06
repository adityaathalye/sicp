; Ch. 1.3.4 Procedures as returned values

; Average damping can be expressed as:
(define (average-damp f)
    (lambda (x) (average x (f x))))
    

; Square root using average-damp
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

; Cube-root using average-damp
(define (cube-root x)
    (fixed-point (average-damp (lambda (y) (/ x (square y))))
                 1.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Newton's method re-cast as: If x |--> g(x) is a 
; differentiable function, then the solution of the equation 
; g(x) = 0 is a fixed point of the function x |--> f(x) where
; f(x) = x - g(x)/Dg(x) and Dg(x) is the derivative of g 
; evaluated at x.

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
        dx)))
; Where dx is defined as
(define dx 0.00001)

; Newton's transform for f(x) = x - g(x)/Dg(x)
(define (newtons-transform g)
    (lambda (x) 
      (- x (/ (g x) ((deriv g) x)))))

; Newton's method implemented using newtons-transform
(define (newtons-method g guess)
  (fixed-point (newtons-transform g) guess))

  
; Square-root using newtons-method relies on finding a zero of the
; function y |--> y^2 - x starting with an intial guess of 1.
(define (sqrt x)
    (newtons-method (lambda (y) (- (square y) x))
                    1.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Prerequisite code

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