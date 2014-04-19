;; Extended Exercise Interval arithmetic; contd...
;; Ex. 2.8 sub-interval

;; Rp = (/ 1 (+ (/ 1 R1) (/ 1 R2)))


;; 
;; Given information, a.k.a what Alyssa did
;;
(define (add-interval x y) 
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
		  (/ 1.0 (lower-bound y)))))


(define (make-interval a b) (cons a b))

;;
;; Ex. 2.7 Define lower-bound and upper-bound
;;
(define (lower-bound i) (car i))

(define (upper-bound i) (cdr i))


;;
;; Ex. 2.8 sub-interval
;; 

;; Subtracting intervals means expressing as a pair, the difference
;; of lower bounds and the difference of upper bounds, with the following
;; things in mind:
;;;; The delta of two UPPER bounds could sometimes be _smaller_ than
;;;; the delta of two lower bounds.
;;;; Deltas could be negative too.
(define (sub-interval x y)
  (let ((d1 (- (lower-bound x) (lower-bound y)))
	(d2 (- (upper-bound x) (upper-bound y))))
    (make-interval (min d1 d2)
		   (max d1 d2))))


;;
;; Test results
;; 

r1
;Value 25: (.5 . 1.)

r2
;Value 27: (.8 . 1.1)

(sub-interval r1 r2)
;Value 28: (-.30000000000000004 . -.10000000000000009)

(sub-interval r2 r1)
;Value 29: (.10000000000000009 . .30000000000000004)


