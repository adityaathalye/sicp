; Ex 1.7 Better square roots accuracy
; Use alternate strategy to implement <good-enough?>
	; Watch how guess changes from one iteration to next and stop when change is a very small fraction of the guess
; A block-structured and lexically scoped version of finding square roots.

(define (average x y)
	(/ (+ x y) 2))

(define (sqrt x)
	 (define (good-enough? guess)
		(< (/ (abs (- guess (improve guess))) guess) 0.0000000000000000001))  ; Examine the difference between this guess and the next best guess
	(define (improve guess)
		(average guess (/ x guess)))
	(define (sqrt-iter guess)
		(if (good-enough? guess)
			guess
			(sqrt-iter (improve guess))))
	(sqrt-iter 1.0))
	
