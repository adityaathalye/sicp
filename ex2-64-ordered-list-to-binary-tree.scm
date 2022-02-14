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

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define tree->list-counter 0)

(define (inc-tree->list-counter!)
  (set! tree->list-counter (+ tree->list-counter
                              1)))

(define (partial-tree elts n)
  (inc-tree->list-counter!)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(define odds (list 1 3 5 7 9 11))


;;  (partial-tree odds 2)

;; Answers:

;; list->tree returns a balanced binary tree as expected
;;
;; (list->tree odds)
;; (5
;;  (1
;;   ()
;;   (3 () ()))
;;  (9
;;   (7 () ())
;;   (11 () ())))

;; which can be drawn as:
;;          ___ 5 ___
;;         /         \
;;        1           9
;;         \         / \
;;          3       7   11
;;

;; Ans. 2.64.a
;;
;; partial-tree essentially CDRs down the given list. It maintains the intermediate
;; state of the tree-construction as a pair of accumulated items, and non-accumulated
;; items. It doesn't know anything about sort order of the incoming list. Nor does
;; it try to re-balance the tree in any way. It makes a balanced tree by construction,
;; by simply generates a left-right tree-recursion based on the length of the list
;; of elements at each partition. This way leaf nodes are trees with non-empty CAR,
;; and empty CADR and CADDR.

;; Ans. 2.64.b
;;
;; tree->list grows as O(n) because the tree-recursion in partial-tree cdrs down
;; the given list, thus touching each element only.
