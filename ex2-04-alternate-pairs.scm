; Ex. 2.04 Alternative procedural representation of pairs

(define (cons x y)
    (lambda (m) (m x y)))
    
(define (car z)
    (z (lambda (p q) p)))
    
(define (cdr z)
    (z (lambda (p q) q)))
    
; Evaluation by substitution 
; (car (cons 1 2))
; ((cons 1 2) (lambda (p q) p))
; (((lambda (m) (m x y)) 1 2) (lambda (p q) p))
; ((lambda (m) (m 1 2)) (lambda (p q) p))
; ((lambda (p q) p) 1 2)
; 1