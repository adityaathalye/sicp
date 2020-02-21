;; -*- geiser-scheme-implementation: mit-scheme -*-

;; ch 2.3.3 Example: Representing Sets

;; Sets as un-ordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; (element-of-set? 1  '(1 2 3 4))
;; (element-of-set? 42 '(1 2 3 4))
;; (element-of-set? 4 '(1 2 3 3 4 3 3 4))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

;; (adjoin-set 42 (adjoin-set 42 '(1 2 3 4)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


;; (intersection-set '(1 2 3 4) '(3 4 5 6))
;; (intersection-set '(1 2 3 4) '(5 6 7 8))
;; (intersection-set '(2 4) '(1 2 3 4))


;; Ex. 2.59 Implement union-set operation for the unordered-list
;; representation of sets.


(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1)
                         (cons (car set1) set2)))))

;; (union-set '() '())
;; (union-set '() '(1 2 3 4))
;; (union-set '(1 2 3 4) '())
;; (union-set '(1 2 3 4) '(1 2 3 4))
;; (union-set '(1 2 3 4) '(4 3 2 1))
;; (union-set '(1 2 3 4) '(5))
;; (union-set '(1 2 3 4) '(5 4 3 2 1))
;; (union-set '(1 2 3 4) '(5 6 7 8 9))
;; (union-set '(1 2 1 2 3 3 4 3 3 3) '(5 6 7 8 9))
;; (union-set '(5 6 7 8 9) '(1 2 1 2 3 3 4 3 3 3))

;; Ex. 2.60 Allow sets with duplicates, and redesign procedures
;; element-of-set?, adjoin-set, union-set, and intersection-set
;; for sets having duplicates.

(define element-of-set?-v2
  element-of-set?)

;; (element-of-set?-v2 3 '(1 2 1 2 3 3 4 3 3 3))
;; (element-of-set?-v2 9 '(1 2 1 2 3 3 4 3 3 3))


(define (adjoin-set-v2 x set)
  (cons x set))

;; (adjoin-set-v2 3 '(1 2 1 2 3 3 4 3 3 3))
;; (adjoin-set-v2 9 '(1 2 1 2 3 3 4 3 3 3))


(define intersection-set-v2 intersection-set)

;; (intersection-set-v2 '(1 2 2 3 4) '(2 2 3 4 5 5 6 7))
;; (intersection-set-v2 '(1 2 2 3 3) '(2 2 1 1 3 3 3))
;; (intersection-set-v2 '(1 2 2 3 3 3) '(2 2 1 1 3 3))
;; (intersection-set-v2 '(3 2 2 3 3) '(2 1 1 3 3))


(define union-set-v2 append)

;; (union-set-v2 '(1 2 2 3 3 4 4 9) '(1 2 2 3 4 5 5))
;; (union-set-v2 '() '(1 2 2 3 4 5 5))
;; (union-set-v2 '(1 2 2 3 4 5 5) '())
