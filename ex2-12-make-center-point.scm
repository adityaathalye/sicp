;;; Ex. 2.12 Real engineers use % tolerances.
;;; Define a constructor make-center-percent.


;;
;; Constructor functions taken from Ex. 2.7 and before
;; 
(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))


;;
;; Constructors Alyssa wrote after Ex. 2.11
;; 
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i))
     2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i))
     2))


;;
;; Solution to Ex. 2.12
;; 
(define (make-center-percent c p)
  (make-interval (- c (/ (* c p) 100.0))
		 (+ c (/ (* c p) 100.0))))

(define (percent i)
  (* (/ (width i) (center i))
     100))


;;
;; Test for Ex. 2.12
;;
(define r (make-interval 1 1.1))
r
(center r)
(percent r)
(make-center-percent (center r) (percent r))

