; Ex 1.12 Pascal's triangle
	; A RECURSIVE PROCESS to find coefficients

(load-option 'format)

(define (the-digit x n)
	(if (OR (= n 0) (= x 0) (= x n))
		1
		(+ (the-digit (- x 1) (- n 1)) (the-digit x (- n 1)))))
		; This generates a Tree-Recursive  process

; Print all digits of the entire nth row
; i.e. all binomial coefficients for (x + y)^n
(define (pascal-row n)
	(define (walk-row count)
		(display (the-digit count n))
		(display " ")
		(if (<= count 0) 1 (walk-row (- count 1))))
	(walk-row n))

; Construct a complete pascal triangle
; Print all rows for coefficients from n = 0 to n
(define (pascal-triangle n)
	(define (walk-down count)
		(pascal-row count)
		(format #t "~%")
		(if (>= count n) 1 (walk-down (+ count 1))))
	(walk-down 0))