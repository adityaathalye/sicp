; Ex. 2.5 Define cons, car, cdr in terms of 2^a * 3^b

(define (cons a b)
    (* (expt 2 a) (expt 3 b)))
    
(define (car x)
  (define (iter result)
     (if (= (modulo result 3) 0)
         (iter (/ result 3))
         (/ (log result) (log 2))))
  (iter x))
  
(define (cdr x)
  (define (iter result)
    (if (= (modulo result 2) 0)
        (iter (/ result 2))
        (/ (log result) (log 3))))
  (iter x))
  
; Test results:
; (car (cons 7 13))
; ;Value: 7.

; (cdr (cons 7 13))
; ;Value: 12.999999999999998
