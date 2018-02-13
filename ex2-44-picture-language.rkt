#lang sicp
(#%require sicp-pict)
;; Install DrRacket and read the following documentation to
;; install and use SICP:
;; http://docs.racket-lang.org/sicp-manual/index.html


(define scale-vect vector-scale)
(define add-vect vector-add)
(define sub-vect vector-sub)

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
  (path-iter '() vecs))


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

;; Try:
;(paint frame-border)


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

;; Try:
;(paint wave)



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


(paint (beside (below wave wave)
                (below-alt wave wave)))