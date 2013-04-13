; Ex. 2.3 Construct a Rectangle

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ; ; Procedures that compute a rectangle's properties: perimeter, 
; area, diagonal) should only need to know its length and width.

; ; ; These shouldn't be forced to validate the correctness of the 
; rectangle object. So, the rectangle constructor must ensure it 
; constructs a valid rectangle. And, a guaranteed rectangle will 
; expose length and width (at least), in a standard way, to 
; enable computations of the rectangle's properties.

; ; ; [Using Wishful Thinking (pg 84) and 
; ; ; Abstraction Barrier (pg 87, ln -5)]

; ------[Programs that use guaranteed rectangles]----------------

;        Quadrilaterals guaranteed to be rectangles

; ------[Programs that use arbitrary quadrilaterals]-------------

;        Quadrilaterals as sequences of arbitrary coordinates

; ------[Programs that use arbitrary coordinates]----------------

;        Coordinates as pairs of two arbitrary real numbers

; ------[cons, car, cdr]-----------------------------------------
;        However pairs are implemented


(define (rect-property property rectangle)
    (property (car rectangle) (cdr rectangle)))

(define (perimeter a b)
    (* 2 (+ a b)))
    
(define (area a b)
    (* a b))

(define (diagonal a b)
    (sqrt (+ (square a) (square b))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Represent rectangles using various means


; Represent a rectangle simple-mindedly
(define (rect-dimensions side-one side-two)
    (cons side-one side-two))


; Represent a rectangle using vertices, assuming convexity /
; adjacency of coordinates will be guaranteed by the user
(define (rect-coordinates a b c d)
  (let ((edge (lambda (p q) (seg-len (mk-seg p q)))))
    (cons (edge a b) (edge b c))))


; Guarantee a rectangle and represent it in terms of adjacent edges
; ; ; Guaranteeing correctness of a rectangle involves:
; 1. Guaranteeing convexity by enforcing adjacency of coordinates
; 2. Checking if diagonals are of equal length
; 3. Ensuring the rectangle object is always composed of lengths
; of adjacent sides of the rectangle.
; ; ; This is proving to be rather cumbersome as I can only 
; construct pairs and access pair elements, at this moment.

; ; ; For the purpose of this exercise, I blindly trust that the  
; user will read this and supply a guaranteed rectangle, in one of 
; two ways: 
; ; ; as lengths of two adjacent edges or 
; ; ; as a set of adjacent coordinates.

; (define (rect-guaranteed a b c d)
    ; ; A crude way to do it:
    ; (let ((quad (quadrilateral a b c d))
          ; (rect-seg (quadri-seg (quadrilateral a b c d))))
      ; ; Return a pair of adjacent sides of the quadrilateral 
      ; ; if it is a rectangle.
      ; (if (equal-diagonals? quad)
        ; (cons (car rect-seg) (car (cdr rect-seg)))
        ; (error "Not a rectangle." quad))))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ; A convex quadrilateral with diagonals of equal length is a 
; ; rectangle.

; (define (quadrilateral a b c d)
    ; ; Take four coordinates as arguments and place them in a 
    ; ; sequence that represents an arbitrary quadrilateral.
    ; ; Depending on the sequence of the coordinates, this could
    ; ; be convex or concave.
    ; ;)


; (define (convexify! quadrilateral)
    ; ; Some way to return a quadrilateral in a convex form.
    ; ; This implies that the coordinates of the input 
    ; ; quadrilateral be rearranged into an output quadrilateral
    ; ; that is a sequence of **adjacent** coordinates.
    ; )

    
; (define (quadri-seg quadrilateral)
    ; ; Compute all the six line segments that can be generated 
    ; ; using four coordinates, and return an orderly sequence 
    ; ; of the same [edges ab, bc, cd, da, and diagonals ac and bd]

    ; ; This will internally make use of convexify!
    ; )

    
; (define (equal-diagonals? quadrilateral)
    ; ; Some way to check if diagonals of the quadrilateral are
    ; ; of equal length.

    ; ; This can make use of quadri-segments.
    ; (car (cdr (cdr (cdr (cdr quadri-segments quadrilateral)))))
    ; (cdr (cdr (cdr (cdr (cdr quadri-segments quadrilateral)))))
    ; )
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Perform computations on line segments

; Compute the midpoint of a line segment
(define (seg-mid s)
    (let ((mid-x (/ (+ (x (start-segment s)) 
                       (x (end-segment s))) 
                    2.0))
          (mid-y (/ (+ (y (start-segment s))
                       (y (end-segment s)))
                    2.0)))
      (cons mid-x mid-y)))


; Compute the length of a line segment
(define (seg-len s)
    (let ((x-distance (- (x (start-seg s)) 
                         (x (end-seg s))))
          (y-distance (- (y (start-seg s)) 
                         (y (end-seg s)))))
      (sqrt (+ (square x-distance) (square y-distance)))))      
      
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Represent a line segment in a plane

(define (mk-seg a b)
    (cons a b))
    
(define (start-seg p)
    (car p))

(define (end-seg p)
    (cdr p))

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Represent a point in a plane

(define (p x y)
    (cons x y))
    
(define (x p)
    (car p))
    
(define (y p)
    (cdr p))

(define (same-p? p1 p2)
    (and (= (car p1) (car p2)) (= (cdr p1) (cdr p2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Test data and results

; (define a (p 0 0))
; ;Value: a

; (define b (p 10 0))
; ;Value: b

; (define c (p 10 5))
; ;Value: c

; (define d (p 0 5))
; ;Value: d

; (define rectangle-a
  ; (rect-coordinates a b c d))
; ;Value: rectangle-a

; (define rectangle-b
  ; (cons 5 10))
; ;Value: rectangle-b


; (rect-property perimeter rectangle-a)
; ;Value: 30

; (rect-property area rectangle-a)
; ;Value: 50

; (rect-property diagonal rectangle-a)
; ;Value: 11.180339887498949


; (rect-property perimeter rectangle-b)
; ;Value: 30

; (rect-property area rectangle-b)
; ;Value: 50

; (rect-property diagonal rectangle-b)
; ;Value: 11.180339887498949

