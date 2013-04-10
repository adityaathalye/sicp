; Ex. 2.2 Represent a line segment in a plane

(define (make-segment start-seg end-seg)
    (cons start-seg end-seg))
    
(define (start-segment p)
    (car p))

(define (end-segment p)
    (cdr p))

(define (make-point x y)
    (cons x y))
    
(define (x-point p)
    (car p))
    
(define (y-point p)
    (cdr p))
    
(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ", ")
    (display (y-point p))
    (display ")"))
    
; Find the midpoit of a line segment
(define (midpoint-segment s)
    (let ((mid-x (/ (+ (x-point (start-segment s)) 
                       (x-point (end-segment s))) 
                    2.0))
          (mid-y (/ (+ (y-point (start-segment s))
                       (y-point (end-segment s)))
                    2.0)))
      (cons mid-x mid-y)))