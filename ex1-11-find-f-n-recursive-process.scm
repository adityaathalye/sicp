; Ex 1.11 Solve function f(n) using Recursive and Iterative PROCESSES
; Such that if n < 3, then f(n) = n
; But if n >= 3, then f(n) = f(n-1) + 2f(n-2) + 3f(n-3)

; RECURSIVE PROCESS version

(define (f n)
	(if (< n 3)
		n
		(+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))
		; Will expand into a RECURSIVE TREE with growth order n^2