; Ch. 2.1.1 Introduction to data abstraction: 
; Example: Arithmetic operations for rational numbers

; Using "wishful thinking", the powerful strategy of synthesis, we
; define a skeleton of procedures that will help us construct 
; a rational number and select its numerator and denominator...

; (define (make-rat n d)
    ; ; Should create a rational number
    ; )
    
; (define (numer x) 
    ; ; should return the numerator of the rational number x
    ; ; notice how numer takes x as an atomic value, without caring for
    ; ; the internal representation of x. (Which in this case happens
    ; ; to be a compound data entity created using the primitive 
    ; ; `cons' constructor.
    ; )
    
; (define (denom x)
    ; ; should return the denominator of the rational number x
    ; ; denom, like the numer procedure accepts x as an atomic entity
    ; )

    
; We use the above wishful thinking to translate the rules of 
; rational number arithmetic into procedures. Rules like:
; n1/d1 + n2/d2 = (n1d2 + n2d1)/(d1d2)
; n1/d1 * n2/d2 = n1n2/d1d2
; n1/d1 = n2/d2 IFF n1d2 = n2d1

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
              
(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
              
(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (numer x) (numer y))))

(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
              
(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))
            
(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x))
    ; Also return the original pair
    x)


; We can create a compound data structure called a pair to 
; represent rational numbers as parts of a pair. The pair can be
; constructed using the `cons' primitive, which takes two arguments
; and returns a compound data object that contains the two 
; arguments as parts.
; AND given a pair object, we can extract the elements using the 
; primitive procedures `car' and `cdr'.

; `cons', `car', and `cdr' allow us to fill in the skeleton of 
; wishful thinking like this:

(define (make-rat n d) 
    (let ((g (gcd n d)))
       (cons (/ n g) (/ d g))))

       
(define (numer x) (car x))

(define (denom x) (cdr x))