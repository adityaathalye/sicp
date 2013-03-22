; Ex 1.18 Iterative process to multiply using addition
    ; The process to multiply, using only addition, grows by 
    ; theta(log n) steps, in constant space.
        
(define(* a b)
  (if (= b 0) 
    0 
    (mult-iter 0 a b)))

(define (mult-iter mult-sum a b)
    ; I display object '*' ONLY as a quick-n-dirty way to visually 
    ; verify order of growth.
    (display (+ mult-sum a))
    (display " ")
    (cond ((= b 1) (+ mult-sum a))
        ((halve b) (mult-iter mult-sum (double a) (halve b)))
        (else (mult-iter (+ mult-sum a) a (- b 1)))))
        
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