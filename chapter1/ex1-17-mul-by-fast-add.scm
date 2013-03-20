; -*- Scheme -*-
; Ex. 1.17 "Fast-multiplication"
    ; This procedure to multiply, using only addition, grows by 
    ; theta(log n) steps and in theta(log n) space owing to 
    ; deferred operations.

(define (* a b)
  ; I displaying object * ONLY as a quick-n-dirty way to visually 
  ; verify order of growth.
  (display *)
  (display " ")
  (cond ((= b 0) 0)
	((halve b) (double (* a (halve b))))
	(else (+ a (double (* a (halve (- b 1))))))))


    ; (halve) as a black-box abstraction
    ; Strictly speaking, one can argue that division and remainder 
    ; are illegal primitives based on the wording of the question. 
    ; However, since I use halve as a black-box abstraction, 
    ; I take the liberty to assume that its internal definition 
    ; can be easily substituted by one of the well-understood 
    ; division algorithm that operate using only primitive addition
    ; and subtraction.
(define (halve x)
  (if (= (remainder x 2) 0)
      (/ x 2)
      #f))


; double in terms of primitive addition
(define (double x) (+ x x))