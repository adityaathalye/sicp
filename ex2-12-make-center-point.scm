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
			(cons (make-interval -1 -2) (make-interval -1 -2))
			(cons (make-interval 2 1) (make-interval 3 2))))

(map (lambda (rpair) (mul-interval-new (car rpair) (cdr rpair))) res-pairs)



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



;;
;; Ex. 2.13
;; Assuming:
;; x and y are both positive intervals,
;; defined in center-percent form.
;;
;; Given that:
;; The % tolerances of x and y are small
;;

;; Suppose:
;; dx is the width of interval x (computed using the % tolerance value)
;; dy is the width of interval y (computed using the % tolerance value)
;; cx is the center of interval x
;; cy is the center of interval y
;;
;; We can say:
;; interval x => cx - dx to cx + dx
;; interval y => cy - dy to cy + dy
;;
;; Therefore:
;; interval x.y 
;; = ((cx - dx) to (cx + dx)).((cy - dy) to (cy + dy))
;; = (cx - dx).(cy - dy) to (cx + dx).(cy + dy)
;; = (cx.cy - cx.dy - cy.dx + dx.dy) to (cx.cy + cx.dy + cy.dx + dx.dy)
;;     Since dx and dy are small, the value of dx.dy will be negligible, 
;;     and can be dropped for purpose of approximation.
;; = (cx.cy - cx.dy - cy.dx) to (cx.cy + cx.dy + cy.dx)
;; = (cx.cy - (cx.dy + cy.dx)) to (cx.cy + (cx.dy + cy.dx))
;; = (cx.cy - cx.cy(dy/cy + dx/cx)) to (cx.cy + cx.cy(dy/cy + dx/cx))
;; = cx.cy.(1 - (dy/cy + dx/cx)) to cx.cy.(1 + (dy/cy + dx/cx))
;;
;; Now, suppose % tolerance of x is pctx, and that of y is pcty, then
;; pctx = 100.dx/cx and pcty = 100.dy/cy
;;
;; Therefore, interval x.y can be rewritten as:
;; = cx.cy.(1 - (pcty/100 + pctx/100)) to cx.cy.(1 + (pcty/100 + pctx/100))
;; = cx.cy.(1 - (pctx + pcty)/100) to cx.cy.(1 + (pctx + pcty)/100)
;; Hence, the % tolerance of interval x.y is simply (pctx + pcty), 
;; and the center of interval x.y is cx.cy.

(define (mul-interval-small-pct-tolerance x y)
  (let ((cx (car x))
        (pctx (cdr x))
        (cy (car y))
        (pcty (cdr y)))
    (cons (* cx cy) (+ pctx pcty))))


(percent (mul-interval (make-center-percent 42 2)
                       (make-center-percent 21 1)))




;; Ex. 2.14

;; If the two methods are actually equivalent, the difference of
;; values computed by either method would be near-identical. 
;; However, such is not the case.
;; For e.g., given r1 = 42 +or- 2% and r2 = 21 +or- 1%, the 
;; difference computes to:
;; (-0.4528858016115631 . 0.4808935815504345)
(let ((r1 (make-center-percent 42 2))
      (r2 (make-center-percent 21 1))
      (one (make-interval 1 1))) ;; 'one' insight: Hat tip: Bill the lizard
  (let ((r1xr2_by_r1+r2 
         ;; Method 1 to compute Parallel resistors
         (div-interval (mul-interval r1 r2)
                       (add-interval r1 r2)))
        (inverse_of_sumof_inverse_r1_and_inverse_r2 
         ;; Method 2 to compute Parallel resistors
         (div-interval one
                       (add-interval (div-interval one r1) 
                                     (div-interval one r2)))))
    (list (cons "Parallel_1 = r1xr2_by_r1+r2 = " r1xr2_by_r1+r2)
          (cons "Parallel_2 = inverse_of_sumof_inverse_r1_and_inverse_r2 = "
                inverse_of_sumof_inverse_r1_and_inverse_r2)
          (cons "Parallel_1 - Parallel_2 = "
           (sub-interval r1xr2_by_r1+r2 
                         inverse_of_sumof_inverse_r1_and_inverse_r2)))))


;; Ex. 2.15
;; The best answer lies in demonstrating how error creation, 
;; propogation, and amplification differs for algebraically
;; equivalent functions, when we try to apply them to uncertain 
;; intervals.
;;
;; The closest analogy I have is: If a function mutates state
;; then it's very difficult to write an equivalent function
;; that does _exactly_ the same thing _every_ time. By contrast, 
;; if a function is pure, then it's far more possible to 
;; drop in another pure function that implements the exact same API,
;; but has a radically different internal logic.
;; 
;; We can think of uncertain intervals as analogous to uncertainty
;; introduced by state-changing behaviour in functions.
;;


;; Ex. 2.16
;; This is a hard problem. And, that's Hal and Jerry talking...
;; So... I'll circle back to it later. Maybe.
;;
