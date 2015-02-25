;; Ex. 2.7 Extended Exercise Interval arithmetic

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
;; Results
;;

(define R1 (make-interval 0.5 1.0))
;Value: r1

R1
;Value 14: (.5 . 1.)

(define R2 (make-interval 0.7 1.2))
;Value: r2

(lower-bound R1)
;Value: .5

(upper-bound r2)
;Value: 1.2

(add-interval r1 r2)
;Value 16: (1.2 . 2.2)

(mul-interval r1 r2)
;Value 17: (.35 . 1.2)

(div-interval r1 r2)
;Value 21: (.4166666666666667 . 1.4285714285714286)
