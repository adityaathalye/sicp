;; -*- geiser-scheme-implementation: mit -*-

(define (make-tree entry left right)
  (list entry left right))

(define (tree? tree)
  (and (list? tree)
       (= 3 (length tree))))

(define (leaf? tree)
  (not (tree? tree)))

(define (entry tree)
  (if (tree? tree)
      (car tree)
      tree))

(define (left-branch tree)
  (if (tree? tree)
      (cadr tree)
      '()))

(define (right-branch tree)
  (if (tree? tree)
      (caddr tree)
      '()))

;; Ex 2.65, Θ(n) implementations of union-set and intersection-set for
;; sets implemented as (balanced) bi-nary trees.

;; Intersection set

(define (intersection-set-v1 set1 set2)
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
