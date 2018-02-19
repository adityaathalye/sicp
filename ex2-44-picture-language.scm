;; Picture Language
;; - Represent data objects as procedures
;; - Design operations that obey the closure property (just like cons)
;; - There is only one kind of element, a `painter'
;;   - A painter draws an image that is shifted and scaled to fit
;;     within a designated parallelogram-shaped frame. The actual
;;     shape of the image depends on the shape of the frame.
;;   - We will use a primitive painter called `wave'
;; - We combine images with operations like `beside',
;;   - `beside'  takes two painters and produces a compound painter
;;     that draws the first painter's image in the left half
;;     and the second painter's image in the right half of the frame.
;;   - other painters include `below', `flip-vert', `flip-horiz'
;;   - all these produce new painters which are closed under our
;;     language's means of combination

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;; ===========================================
;; Ex. 2.44 `up-split'
;; - Similar to `right-split', except it switches the roles of
;;   `below' and `beside'


(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        ;; interchange `below' and `beside' w.r.t. `right-split'
        (below painter (beside smaller smaller)))))


;; Higher order operations


;; Hof to generalize over flipped-pairs and square-limit

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (bl painter))))
      (below bottom top))))


(define (flipped-pairs2 painter)
  ((square-of-four identity-procedure flip-vert
                   identity-procedure flip-vert)
   painter))


(define (square-limit2 painter n)
  ((square-of-four flip-horiz identity-procedure
                   rotate180  flip-vert)
   (corner-split painter n)))


;; ===========================================
;; Ex. 2.45 `split' higher-order procedure
(quote
 ;; `beside' and `below' are unbound symbols, so we get an error
 ;; if we actually evaluate these definitions:
 (define right-split (split beside below))
 (define up-split (split below beside)))


(define (split fa fb)
  (define (split-hlp painter n)
    (let ((smaller (split-hlp painter (- n 1))))
      (fa painter (fb smaller smaller)))))



;; Frames
;;
;; A "frame" is:
;; - Defined by 3 vectors; an origin vector, and two edge vectors
;; - Constructed using some procedure `make-frame'
;; - And has selectors `origin-frame', `edge1-frame', `edge2-frame'
;;
;;        .-------.
;;        ^       |
;;   edge | frame |
;;        '------>'
;;       /  edge
;;      /
;;     / origin vector
;;    /
;;   * (0, 0)
;;
;; - We will use coordinates in the unit square (0 <= x,y <= 1)
;;   to specify images.
;; - A "frame coordinate map" will be used to shift and scale images
;;   to fit the frame.
;; - This map transforms the unit square into the frame by mapping
;;   vector    :  v = (x,y) to the
;;   vector sum:  Origin(Frame) + x.Edge1(Frame) + y.Edge2(Frame)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


;; ===========================================
;; Ex. 2.46 Data abstraction for vectors
;; - make-vect, xcor-vect, ycor-vect
;; - assuming x,y represents a vector from origin
;; - further implement add-vect, sub-vect, and scale-vect

(define make-vect cons)

(define xcor-vect car)

(define ycor-vect cdr)

(quote
 (let ((v (make-vect 1 2)))
   (list (xcor-vect v) (ycor-vect v) v)))


(define (arith-vect op)
  (lambda (v1 v2)
    (make-vect (op (xcor-vect v1) (xcor-vect v2))
               (op (ycor-vect v1) (ycor-vect v2)))))

(define add-vect (arith-vect +))

(define sub-vect (arith-vect -))

(define (scale-vect scalar v)
  (make-vect (* scalar (xcor-vect v))
             (* scalar (ycor-vect v))))

(quote (add-vect (make-vect 1 2) (make-vect 3 4)))


;; ===========================================
;; Ex. 2.47 Selectors for frames
;; - I prefer the implementation using `list', over `cons'

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)

(define edge1-frame cadr)

(define edge2-frame caddr)

(quote
 (let ((frm (make-frame (make-vect 0 0)
                        (make-vect 1 2)
                        (make-vect 3 4))))
   (list (origin-frame frm)
         (edge1-frame frm)
         (edge2-frame frm))))


(quote
 ;; Using `cons' instead of `list' makes things awkward:
 (let ((frm (make-frame2 (make-vect 0 0)
                         (make-vect 1 2)
                         (make-vect 3 4))))
   (define (make-frame2 origin edge1 edge2)
     (cons origin (cons edge1 edge2)))

   (define edge2-frame2 cddr)

   (list (origin-frame frm)
         (edge1-frame frm)
         (edge2-frame2 frm))))



;; Painters
;; - If p is a painter, and f is a frame, then we produce p's image
;;   in f, by calling p with f as an argument.

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))


;; ===========================================
;; Ex. 2.48 A directed line segment in a plane is a pair of vectors
;; where each vector runs from the origin to a point on the plane
;; "Use your vector representation from Exercise 2.46 to define
;; a representation for segments with a constructor make-segment"
(define make-segment cons)

(define start-segment car)

(define end-segment cdr)


;; ===========================================
;; Ex. 2.49 Use segments->painter to define the following
;; primitive painters:

;; - Painter to draw the outline of a designated frame
;; - painter to draw diagonals inside a frame
;; - painter to draw a diamond shape connecting mid-points
;;   of the sides of the frame
;; - the wave painter

;; - Notes:
;;   - make-frame requires origin, edge1, edge2,
;;   - make-segment expects two vectors, each from origin to point


;; Some fundamental vectors for convenience
(define zero-origin (make-vect 0 0))
(define unit-x (make-vect 1 0))
(define unit-y (make-vect 0 1))
(define unit-xy (make-vect 1 1))


(define (vecs->segments-path vecs)
  ;; Convenience utility to turn an ordered list of vectors
  ;; into a list of contiguous segments that when drawn will
  ;; "trace" a path from the first vector to the last vector.
  (define (path-iter pxs vxs)
    (if (null? (cdr vxs))
        pxs
        (path-iter
         (append pxs (list (make-segment (car vxs) (cadr vxs))))
         (cdr vxs))))
  (path-iter '() vecs))


(define frame-border
  ;; The frame outline painter should transform a unit square
  ;; by the provided frame.
  (let* ((s (lambda (v) (scale-vect 0.99 v)))
         (unit-x (s unit-x))
         (unit-y (s unit-y))
         (unit-xy (s unit-xy)))
    (segments->painter
     (vecs->segments-path
      (list zero-origin
            unit-x
            unit-xy
            unit-y
            zero-origin)))))


(define diagonals
  ;; A unit x
  (segments->painter
   (list (make-segment zero-origin
                       unit-xy)
         (make-segment unit-x
                       unit-y))))


(define diamond
  (let* ((mid-x (scale-vect 0.5 unit-x))
         (mid-y (scale-vect 0.5 unit-y))
         (top-xy (add-vect mid-x unit-y))
         (right-xy (add-vect unit-x mid-y)))
    (segments->painter
     (vecs->segments-path
      (list mid-x
            mid-y
            top-xy
            right-xy
            mid-x)))))


(define wave
  ;; Oddly human figurine as traced on a crude 10x10 square grid.
  ;; Ref: "ex2-44-picture-language.rkt" for a working example.
  (let* ((l-hand-top (make-vect 0.0 0.7))
         (l-hand-bot (make-vect 0.0 0.6))
         (l-elbow-top (make-vect 0.2 0.6))
         (l-elbow-bot (make-vect 0.2 0.5))
         (l-shoulder-top (make-vect 0.3 0.7))
         (l-shoulder-bot (make-vect 0.3 0.6))
         (l-neck (make-vect 0.4 0.7))
         (l-waist (make-vect 0.4 0.4))
         (l-toe-out (make-vect 0.3 0.0))
         (l-toe-in (make-vect 0.4 0.0))
         (mid (make-vect 0.5 0.3))
         (r-toe-in (make-vect 0.6 0.0))
         (r-toe-out (make-vect 0.7 0.0))
         (r-waist (make-segment 0.6 0.4))
         (r-shoulder-bot (make-vect 0.7 0.6))
         (r-hand-bot (make-vect 1.0 0.3))
         (r-shoulder-top (make-vect 0.7 0.7))
         (r-hand-top (make-vect 1.0 0.4))
         (r-neck (make-vect 0.6 0.7))
         (l-temple (make-vect 0.35 0.85))
         (r-temple (make-vect 0.65 0.85))
         (l-crown (make-vect 0.4 1.0))
         (r-crown (make-vect 0.6 1.0)))
    (segments->painter
     (fold-right
      append '()
      (list
       (vecs->segments-path (list l-hand-top
                                  l-elbow-top
                                  l-shoulder-top
                                  l-neck
                                  l-temple
                                  l-crown))
       (vecs->segments-path (list l-hand-bot
                                  l-elbow-bot
                                  l-shoulder-bot
                                  l-waist
                                  l-toe-out))
       (vecs->segments-path (list l-toe-in
                                  mid
                                  r-toe-in))
       (vecs->segments-path (list r-toe-out
                                  r-waist
                                  r-shoulder-bot
                                  r-hand-bot))
       (vecs->segments-path (list r-hand-top
                                  r-shoulder-top
                                  r-neck
                                  r-temple
                                  r-crown)))))))


;; Transforming and Combining Painters
;; - Given a painter and information on how to transform a frame,
;;   produce a new transformed painter. The transformed painter,
;;   when called on a frame, transforms the frame and calls the
;;   original painter on the transformed frame.
;; - The arguments to transform-painter are points (represented
;;   as vectors) that specify the corners of the new frame.

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let* ((m (frame-coord-map frame))
           (new-origin (m origin)))
      (painter (make-frame
                new-origin
                (sub-vect (m corner1) new-origin)
                (sub-vect (m corner2) new-origin))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2


(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))


(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))


;; ===========================================
;; Ex. 2.50
;; - flip-horiz
;; - rotate180
;; - rotate270

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))


(define (rotate180 painter)
  (flip-vert (flip-horiz painter)))


(define (rotate270 painter)
  (rotate90 (rotate180 painter)))


;; ===========================================
;; Ex. 2.51 below painter
;; - As a procedure like beside
;; - As a procedure using beside and rotation

(define (below painter1 painter2)
  (let* ((split-point (make-vect 0.0 0.5))
         (paint-upper
          (transform-painter painter1
                             split-point
                             (make-vect 1.0 0.5)
                             (make-vect 0.0 1.0)))
         (paint-lower
          (transform-painter painter2
                             (make-vect 0.0 0.0)
                             (make-vect 1.0 0.0)
                             split-point)))
    (lambda (frame)
      (paint-upper frame)
      (paint-lower frame))))


(define (below-alt painter1 painter2)
  (rotate270
   (beside (rotate90 painter1)
           (rotate90 painter2))))
