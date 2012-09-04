; Procedure for Newton's Approximation of cube roots
; [Done with block structure nesting and lexical scoping [Ch 1.1.8]]
	; If y is an approximation of cube root x
		; Then a better approximation is [x/y^2 + 2y]/3
		
(define (cube-root x)
	(define (good-enough? guess)
		(< (abs (- guess (improve guess))) 0.0001))
	(define (improve guess)
		(/ (+ (/ x (square guess)) (* 2 guess)) 3))
	(define (cube-root-iter guess)
		(if (good-enough? guess)
		guess
		(cube-root-iter (improve guess))))
	(cube-root-iter 1.0))