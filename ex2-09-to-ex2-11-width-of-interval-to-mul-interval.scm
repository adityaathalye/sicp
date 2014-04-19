;; Extended Exercise Interval arithmetic; contd...
;; Ex. 2.9 Width of intervals

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
;; Ex. 2.9 width of interval
;; 

;; The width of an interval will always be a positive number.
;; (Assuming, of course, resistances are intervals of real numbers.)
(define (width-of-interval-ex2-9 x)
  (/ (abs (- (upper-bound x)
	     (lower-bound x)))
     2))


;; 
;; Ex. 2.10 Modify width-of-interval to makes use of Ben Bitdiddle's
;; expert remarks. (Spake he; "It's not clear what it means to divide
;; by an interval that spans zero.")
;; 

(define (width-of-interval x)
  (let ((up-x (upper-bound x))
	(lo-x (lower-bound x)))
    (if (= up-x lo-x)
	(error "The interval for this resistor spans zero: " x)
	(/ (abs (- up-x lo-x))
	   2))))


(display "****** 
Testing Ex. 2.10 
******")


(define r1 (make-interval 0.5 1.0))
(display r1)
(width-of-interval r1)
;Value: .25

(define r2 (make-interval 0.8 1.1))
(display r2)
(width-of-interval r2)
;Value: .15000000000000002

(define r3 (make-interval 2.0 2.0))
(display r3)
(display "Call to width-of-interval with r3 is commented out, 
as it will cause an error and prevent 
the other tests from running. 
Uncomment it to see what happens.")
; (width-of-interval r3) ; uncomment this and evaluate it to see the error:

;The interval for this resistor spans zero:  (2. . 2.)
;To continue, call RESTART with an option number:
; (RESTART 3) => Return to read-eval-print level 3.
; (RESTART 2) => Return to read-eval-print level 2.
; (RESTART 1) => Return to read-eval-print level 1.
;Start debugger? (y or n): n


(= (width-of-interval (add-interval r1 r2))
   (width-of-interval (add-interval r2 r1)))

(= (width-of-interval (sub-interval r1 r2))
   (width-of-interval (sub-interval r2 r1)))

(define r4 (make-interval 1.3 1.4))
(define r5 (make-interval -0.9 -0.8))
(mul-interval r4 r5)
(mul-interval r5 r4)
(width-of-interval (mul-interval r4 r5))
(width-of-interval (mul-interval r5 r4))


(= (width-of-interval (div-interval r1 r2))
   (width-of-interval (div-interval r2 r1)))





;; Ex. 2.11 Break mul-interval into nine case by testing
;; for the signs of the endpoints of the intervals.

(define (mul-interval-new x y)
  (let ((lox (lower-bound x))
	(upx (upper-bound x))
	(loy (lower-bound y))
	(upy (upper-bound y))
	(-? (lambda (r) (negative? r)))
	(+? (lambda (r) (positive? r))))
    (cond ((or (< upx lox) (< upy loy)) "HAH!")
	  ((and (+? lox) (+? upx) (+? loy) (+? upy)) (make-interval (* lox loy)
								    (* upx upy)))	  
	  ((and (-? lox) (-? upx) (-? loy) (-? upy)) (make-interval (* upx upy)
								    (* lox loy)))
	  ((and (-? lox) (+? upx) (+? loy) (+? upy)) (make-interval (* lox upy)
								    (* upx upy)))
	  ((and (+? lox) (+? upx) (-? loy) (+? upy)) (make-interval (* upx loy)
								    (* upx upy)))
	  ((and (-? lox) (-? upx) (+? loy) (+? upy)) (make-interval (* lox upy)
								    (* upx loy)))
	  ((and (+? lox) (+? upx) (-? loy) (-? upy)) (make-interval (* upx loy)
								    (* lox upy)))
	  ((and (-? lox) (-? upx) (-? loy) (+? upy)) (make-interval (* lox upy)
								    (* upx loy)))
	  ((and (-? lox) (+? upx) (-? loy) (-? upy)) (make-interval (* upx loy)
								    (* loy upy)))
	  ((and (-? lox) (+? upx) (-? loy) (+? upy)) (make-interval (min (* lox upy) (* upx loy))
								    (max (* lox loy) (* upx upy)))))))


(display "*****
Testing Ex. 2.11
*****")

(define res-pairs (list (cons (make-interval 1 2) (make-interval 1 2))
			(cons (make-interval -2 -1) (make-interval -2 -1))
			(cons (make-interval -1 2) (make-interval 1 2))
			(cons (make-interval 1 2) (make-interval -1 2))
			(cons (make-interval -2 -1) (make-interval 1 2))
			(cons (make-interval 1 2) (make-interval -2 -1))
			(cons (make-interval -2 -1) (make-interval -1 2))
			(cons (make-interval -2 1) (make-interval -2 -1))
			(cons (make-interval -2 1) (make-interval -2 1))
			; Illegal intervals follow:
			(cons (make-interval -1 -2) (make-interval -2 1))
			(cons (make-interval -2 1) (make-interval 2 -1))
			(cons (make-interval 2 -1) (make-interval 2 -1))
			(cons (make-interval -1 -2) (make-interval -1 -2))))

(map (lambda (rpair) (mul-interval-new (car rpair) (cdr rpair))) res-pairs)





