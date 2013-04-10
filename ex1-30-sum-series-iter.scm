; Ex. 1.30 Sum of series by iterative process

(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))



; A procedure to calculate a sum of cubes in a given range:

(define (cube n) (* n n n))

(define (inc n) (+ n 1))


(define (sum-cubes a b)
    (sum cube a inc b))
    

(define (identity n) n)

(define (sum-integers a b)
    (sum identity a inc b))
    

(define (pi-sum a b)
    (define (pi-term x)
        (/ 1.0 (* x (+ x 2))))
    (define (pi-next x)
        (+ x 4))
    (sum pi-term a pi-next b))
    
    
; Definite integral of a function f between limits a and b:

(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))