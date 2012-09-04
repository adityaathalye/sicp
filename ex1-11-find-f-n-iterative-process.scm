; Ex 1.11 Solve function f(n) using Recursive and Iterative PROCESSES
; Such that if n < 3, then f(n) = n
; But if n >= 3, then f(n) = f(n-1) + 2f(n-2) + 3f(n-3)

; ITERATIVE PROCESS version

(define (f n)
	(define (f-iter a b c n)
		(if (< n 3)
			a
			(f-iter (+ a (* 2 b) (* 3 c)) a b (- n 1))))
			; This should grow linearly
	(if (< n 3)
		n
		(f-iter 2 1 0 n)))
		; This initializes the iteration from the degenerate end
		; so, the process should accumulate the value
		; as it iterates to the nth value