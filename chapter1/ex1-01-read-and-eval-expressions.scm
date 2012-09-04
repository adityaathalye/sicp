; Exercise 1.1
; ------
; What will the following expressions evaluate to, in the REPL?
; ------

10
; 10

(+ 5 3 4)
; 12

(- 9 1)
; 8

(/ 6 2)
; 3

(+ (* 2 4) (- 4 6))
; (+ 8 (- 2)) --> 6

(define a 3)
; a
; This creates a procedure which is an object with name a 
; and then gives it the value 3.
; REPL in MIT-Scheme, prints the name of the procedure, 
; and NOT the value 3, as I had mistakenly assumed.

(define b (+ a 1))
; b 
; BUT ONLY if a is associated with some value.
; otherwise it generates an "unbound variable" error for a.
	; For e.g. If a is first defined, then REPL is restarted, 
	; and then we try to evaluate the procedure b.
	; This happens because the interpreter's "memory" 
	; (i.e. the global environment) gets wiped clean.

; ------
; Following evaluations assume that the interpreter knows 
; a = 3 and b = 4 as defined above.
; ------

(+ a b (* a b))
; --> (+ 3 4 (* 3 4)) --> 19


(= a b)
; Evaluates to #f i.e. false


(if (and (> b a) (< b (* a b)))
	b
	a)
; --> (if (and #t #t) a b) --> a --> 3


(cond ((= a 4) 6)
	((= b 4) (+ 6 7 a))
	(else 25))
; --> (cond (#f 6) (#t (+ 6 7 3)) (else 25))
; --> (+ 6 7 3) 
; --> 16


(+ 2 (if (> b a) b a))
; --> (+ 2 (if #t 4 3)) 
; --> (+ 2 4) 
; --> 6


(* (cond ((> a b) a)
		((< a b) b)
		(else -1))
	(+ a 1))
; --> (* (cond (#f 3) (#t 4) (else -1)) (+ 3 1))
; --> (* 4 4)
; --> 16
; But SUPPOSE  both conditions were false, the interpreter would
; have to move on to evaluate (else -1), which will generate an
; error, saying unbound variable -1. 
; Because even though the minus sign is a primitive operator, 
; Lisp treats contiguous non-whitespace characters as one symbol. 
; So whereas (- 1) evaluates to negative of 1, the sequence -1 
; in the above procedure is a symbol with no value assigned.
