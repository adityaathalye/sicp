; ----------------------------------------------------------
; FINDING SQUARE ROOTS
; ----------------------------------------------------------
; Mathematically, we can define WHAT IS a square root:
;   _/x = the y such that y>=0 and y^2 = x

; BUT Programmatically, we need a way - i.e. HOW TO arrive
; at a square-root, with reasonable accuracy.

; Newton's method of successive approximation is 1 such way.
  ; Initially GUESS that some y is the square root of x
  ; Now find a BETTER GUESS closer to the square root by
  ; averaging guess y with x/y till Good Enough accuracy 
  ; is achieved.
; ----------------------------------------------------------


; The basic strategy, expressed as a Procedure:

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
		 x)))

; New guess: Avg guess with quotient of (radicand x)/(guess)

(define (improve guess x)
  (average guess (/ x guess)))

; where

(define (average x y)
  (/ (+ x y) 2))

; and good enough accuracy is (let's say) 0.0001
  ; [Note: this will fail for sqrt of very small numbers]

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001))


(define (sqrt x)
  (sqrt-iter 1.0 x))











