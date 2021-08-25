;; -*- geiser-scheme-implementation: mit -*-

;; Sets as un-ordered lists (from ex 2.59 and 2.60)
;; O(n)
(define (element-of-set-unordered-list? x unordered-list)
  (cond ((null? unordered-list) false)
        ((equal? x (car unordered-list)) true)
        (else (element-of-set? x (cdr unordered-list)))))


;; Sets as ordered lists
;; - Number objects only
;; - compared using < and >

;; Re-do element of set (complexity is still O(n))
(define (element-of-set? x ordered-list)
  (cond ((null? ordered-list) false)
        ((< x (car ordered-list)) false)
        ((> x (car ordered-list)) (element-of-set? x
                                                   (cdr ordered-list)))
        (else true)))

;; (element-of-set? 5 '())
;; (element-of-set? 2 '(3))
;; (element-of-set? 5 '(1 2 3 4 5))
;; (element-of-set? 6 '(1 2 3 4 5))

;; Intersection set
;; - Sets as ordered lists should intersect in O(n) time instead of
;;   O(n^2) when represented as unordered lists
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((= (car set1) (car set2)) (cons (car set1)
                                         (intersection-set (cdr set1) (cdr set2))))
        ((> (car set1) (car set2)) (intersection-set set1
                                                     (cdr set2)))
        (else (intersection-set (cdr set1)
                                set2))))

;; (intersection-set '() '(1))
;; (intersection-set '(1 2 3) '(2 3 4))
;; (intersection-set '(2 3 4) '(1 2 3))

;; Ex. 2.61 Implement adjoin-set to grow at half the rate
;; of the implementation for unordered lists

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((> x (car set)) (cons (car set)
                               (adjoin-set x (cdr set))))
        (else (cons x set))))

;; (adjoin-set 2 '())      ; into an empty set
;; (adjoin-set 2 '(3 4 5)) ; to the beginning
;; (adjoin-set 5 '(2 3 4)) ; at the end
;; (adjoin-set 4 '(2 3 5)) ; in between


;; Ex. 2.62 union-set
;; - An O(n) implementation for sets as ordered lists

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (cons (car set1)
                                         (union-set (cdr set1) (cdr set2))))
        ((> (car set1) (car set2)) (cons (car set2)
                                         (union-set set1 (cdr set2))))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

;; (union-set '(1 2 3) '(3 4 5)) ; must union at 3
;; (union-set '(3 4 5) '(1 2 3)) ; must union at 3 _and_ sort correctly
