; SICP Ex. 1.32 
; Express a general notion called 'accumulate' that combines a 
; collection of terms, using some general accumulation function:

; (accumulate combiner null-value term a next b)

; WHERE:
; 'combiner' procedure (of two arguments) specifies how the 
    ; current term is to be combined with the accumulation of the 
    ; preceding terms.
; 'null-value' specifies what base value to use when 
    ; the terms run out. 
; 'a', 'b' range-limit the terms of the series to be accumulated.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HOW TO ACCUMULATE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) 
                (accumulate 
                    combiner null-value term (next a) next b))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ACCUMULATE SUM OF SERIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum-of-series a b)    
    (accumulate + 0 identify a increment b))

; The above procedure returns the sum of series from a to b
; using the idea of accumulation.

; WHERE:
; The 'identify' procedure expresses "return my argument as it is":
(define (identify x) x)

; And the 'increment' procedure expresses "increment by one":
(define (increment x) (+ x 1))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ACCUMULATE PRODUCT OF SERIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (product-of-series a b)
    (accumulate * 1 identify a increment b))
; See how the product of series uses EXACTLY the same procedures
    ; as arguments, as are used by the sum-of-series procedure.
; The difference being that the way to combine is to multiply, 
    ; which is represented by the built-in symbol for primitive 
    ; multiplication. 
; And it is not hard to see why the null-value for the product of 
    ; series is 1 (same reason why it is zero for sum of series).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; n-FACTORIAL    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The product-of-series expression can be easily reduced to n!
(define (factorial n)
    (accumulate * 1 identify 1 increment n))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUM OF SQUAREs OF INTEGERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Now, to generate the sum of squares of integers from a-to-b, 
; simply:

(define (square x) (* x x))

(define (sum-of-squares a b)
    (accumulate + 0 square a increment b))


; Sum the cube of series
(define (cube x) (* x x x))

(define (sum-of-cubes a b)
    (accumulate + 0 cube a increment b))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NUMERICAL INTEGRATION of a function f using the rule:
; integral f over lim a-to-b = [f(a + dx/2) + 
;                               f(a + dx + dx/2) + 
;                               f(a + 2dx + dx/2) + ... ]*dx
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (integral f a b dx)
    
    (define (add-dx x) (+ x dx))
    
    (if (<= dx 0) 
        "Cannot compute" ; If condition evaluates to true
        (* (accumulate + 0 f (+ a (/ dx 2.0)) add-dx b)
           dx)))
            

; And so on... You get the picture :)

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TEST IT!
; Test the script at the online interpreter available here:
    ; http://www.biwascheme.org/repos/repl.html

; HOW TO:
; 1. Copy-paste ALL CODE in this file INCLUDING the commented-out
    ; test code you see right after this how-to.
; 2. Uncomment the test procedure calls ONE AT A TIME and click 
    ; the 'eval' button at the online interpreter. (Of course,
    ; also play with the arguments and/or verify the computation.)
; 3. Note that design of all the series procedures work only for
    ; a range of positive integers such that a <= i <= b.
; 4. WARNING: The process generated by the accumulate procedure
    ; is very bad--I reckon it grows non-linearly with b.

; (sum-of-series 1 10)
    ; ;Expected Value: 55


; (product-of-series 1 10)
    ; ;Expected Value: 3628800


; (factorial 10)
    ; ;Expected Value: 3628800


; (sum-of-squares 1 6)
    ; ;Expected Value: 91


; (sum-of-cubes 1 4)
    ; ;Expected Value: 100


; (integral cube 0 1 0.001)
    ; ; Expected Value: 0.249999875000001
    ; ; Gives you the integral of cube between 0 and 1, with 'dx' 
    ; ; proportional to 1/1000. (Its exact value is 1/4.)
    
    