; Chapter 1.2.4. Exponentiation - Linear Recursive PROCESS
; Given that b^n = b.b^(n - 1) and b^0 = 1
; Directly translated into procedure

(load-option 'format) ; Invokes library to format text

(define (expt b n)
	(display "b is ")
	(display b)
	(display " count n is ")
	(display n)
	(display " expt is ")
	(display (expt))
	(newline); new line procedure for scheme
	(if (= n 0)
		1
		(* b (expt b (- n 1)))))
