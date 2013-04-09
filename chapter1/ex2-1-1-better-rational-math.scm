; Ex. 2.1 A better implementation of rational arithmetic

; Construct rational number as a pair object. 
; ; This improves upon the make-rat procedure of Section 2.1.2.
(define (make-rat n d) 
    (let ((norm-n (abs (/ n (gcd n d))))
          (norm-d (abs (/ d (gcd n d))))
          (same-sign? (or (and (< n 0) (< d 0)) 
                          (and (> n 0) (> d 0)))))
      (cond ((= n 0) 
             (cons 0 1))
            ((= d 0) 
             (error "The denominator must be non-zero! " d))
            (same-sign?
             (cons norm-n norm-d))
            (else
             (cons (- norm-n) norm-d)))))


; Extract the numerator and/or denominator of a rational number, 
; assuming it is represented as a pair object.
(define (numer x) (car x))

(define (denom x) (cdr x))


; Perform rational number arithmetic operations
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

       
; Display a pair object using the conventional rational number 
; notation.
(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (denom x)))