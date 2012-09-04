; A block-structured and lexically scoped version of finding square roots.
(define (average x y)
	(/ (+ x y) 2))

(define (sqrt x)
	(define (good-enough? guess)
		(< (abs (- (square guess) x)) 0.0001))
	(define (improve guess)
		(average guess (/ x guess)))
	(define (sqrt-iter guess)
		(if (good-enough? guess)
			guess
			(sqrt-iter (improve guess))))
	(sqrt-iter 1.0))
	
