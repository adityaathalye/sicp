#lang sicp
(#%require sicp-pict)
;; Install DrRacket and read the following documentation to
;; install and use SICP:
;; http://docs.racket-lang.org/sicp-manual/index.html


;; Some aliases for compatibility between my code and
;; the sicp-pict module
(define scale-vect vector-scale)
(define add-vect vector-add)
(define sub-vect vector-sub)
(define identity-procedure identity)

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
          (bottom (beside (bl painter) (br painter))))
      (below top bottom))))


(define (flipped-pairs2 painter)
  ((square-of-four identity-procedure flip-vert
                   identity-procedure flip-vert)
   painter))


(define (square-limit2 painter n)
  ((square-of-four rotate180  flip-vert
                   flip-horiz identity-procedure)
   (corner-split painter n)))


;; ===========================================
;; Ex. 2.45 `split' higher-order procedure
;; `beside' and `below' are unbound symbols, so we get an error
;; if we actually evaluate these definitions:
;;     (define right-split (split beside below))
;;     (define up-split (split below beside)))


(define (split op-a op-b)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split op-a op-b) painter (- n 1))))
          (op-a painter (op-b smaller smaller))))))



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
  (define (path-iter pxs vxs)
    (if (null? (cdr vxs))
        pxs
        (path-iter (append pxs
                           (list (make-segment (car vxs) (cadr vxs))))
                   (cdr vxs))))
  (path-iter (list) vecs))


(define frame-border
  ;; The frame outline painter should transform a unit square
  ;; by the provided frame.
  (let ((s (lambda (v) (scale-vect 0.99 v))))
    (let ((unit-x (s unit-x))
          (unit-y (s unit-y))
          (unit-xy (s unit-xy)))
      (segments->painter
       (vecs->segments-path
        (list zero-origin
              unit-x
              unit-xy
              unit-y
              zero-origin))))))

; Debug it
; 
; ;Works
; (vecs->segments-path
;         (list zero-origin
;               unit-x
;               unit-xy
;               unit-y
;               zero-origin))
; 
; ;True
; (list? (vecs->segments-path
;         (list zero-origin
;               unit-x
;               unit-xy
;               unit-y
;               zero-origin)))
; 
; ;Works
; (segments->painter (list))
; 
; ;Also works, and list by construction
; (list
;  (make-segment (make-vect 0 0)
;                (make-vect 0.99 0))
;  (make-segment (make-vect 0.99 0)
;                (make-vect 0.99 0.99))
;  (make-segment (make-vect 0.99 0.99)
;                (make-vect 0 0.99))
;  (make-segment (make-vect 0 0.99)
;                (make-vect 0 0)))
; 
; ;BOOM! Whyyyy????
; (segments->painter
;  (list
;   (make-segment (make-vect 0 0)
;                 (make-vect 0.99 0))
;   (make-segment (make-vect 0.99 0)
;                 (make-vect 0.99 0.99))
;   (make-segment (make-vect 0.99 0.99)
;                 (make-vect 0 0.99))
;   (make-segment (make-vect 0 0.99)
;                 (make-vect 0 0))))


;; Try:
;; (paint frame-border)


(define diagonals
  ;; A unit x
  (segments->painter (list (make-segment zero-origin
                                         unit-xy)
                           (make-segment unit-x
                                         unit-y))))


(define diamond
  (let ((mid-x (scale-vect 0.5 unit-x))
        (mid-y (scale-vect 0.5 unit-y)))
    (let ((top-xy (add-vect mid-x unit-y))
          (right-xy (add-vect unit-x mid-y)))
      (segments->painter
       (vecs->segments-path
        (list mid-x
              mid-y
              top-xy
              right-xy
              mid-x))))))

;; Try
;(paint diamond)


(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))


(define wave
  ;; Figure as traced on a crude 10x10 square grid.
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
         (r-waist (make-vect 0.6 0.4))
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

;; Try:
;(paint wave)



(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))


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

;; Try:
;(paint (beside (below wave wave)
;               (below-alt wave wave)))


;; =============================================
;; Ex. 2.52 Make changes to square limit of wave

(define (overlay painter1 painter2)
  (lambda (frame)
    (painter1 frame)
    (painter2 frame)))

(define wave+smile
  (overlay wave
           (segments->painter
            (vecs->segments-path
             (list (make-vect 0.435 0.815)
                   (make-vect 0.45 0.8)
                   (make-vect 0.46 0.79)
                   (make-vect 0.54 0.79)
                   (make-vect 0.55 0.8)
                   (make-vect 0.565 0.815))))))

;(paint wave+smile)


(define (corner-split2 painter n)
  (define (commute f x)
    (lambda (y) (f y x)))
  (if (= n 0)
      painter
      ((square-of-four identity-procedure
                       (commute right-split n)
                       (commute up-split n)
                       (commute corner-split2 (- n 1)))
       painter)))

;; Try
; (paint (corner-split wave 4))
; (paint (corner-split2 wave 4))


; '(define (square-limit2 painter n)
;   ((square-of-four flip-horiz identity-procedure
;                    rotate180 flip-vert)
;       (flip-vert (corner-split painter n))))
;


(define (square-limit3 painter n)
  (let ((outward-looking-p (flip-horiz painter)))
    ((square-of-four flip-horiz identity-procedure
                     rotate180 flip-vert)
     (flip-vert (corner-split outward-looking-p n)))))

;; Try
;; (paint (square-limit2 wave+smile 4))
;; (paint (square-limit3 wave+smile 4))


; ((paint frame-border))
; 
; (paint wave)
; 
; (paint wave+smile)
; 
; (paint (square-limit2 wave+smile 4))



; ;; Other weird and wonderful square-limits...
; (paint (square-limit2 (shrink-to-upper-right wave+smile)
;                       4))
; 
; (paint (square-limit2 (overlay frame-border
;                                (shrink-to-upper-right wave+smile))
;                       4))
; 
; (paint (square-limit2 (squash-inwards wave)
;                       4))
; 
; (paint (square-limit2 (overlay diamond
;                                (squash-inwards wave))
;                       4))
; 


