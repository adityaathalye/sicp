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

;; An arbitrary tree, not necessarily ordered
(define example-tree
  (make-tree 6
             (make-tree 3 2 (make-tree 4 -2 4))
             5))

;; Assuming a set is represented as a balanced, ordered, binary tree,
;; element-of-set? will evaluate in O(log(n)) steps.
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        (else (element-of-set? x (right-branch set)))))

;; Adjoin set

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        (else (make-tree (entry set)
                         (left-branch set)
                         (adjoin-set x (right-branch set))))))

;;
;; AVL Tree Rebalancing
;; A self-binary tree named after its inventors Adelson-Velsky and Landis
;;
;; Ref: GeeksForGeeks explanation found here:
;; https://www.geeksforgeeks.org/avl-tree-set-1-insertion/
;;
;; T1, T2 and T3 are subtrees of the tree
;; rooted with y (on the left side) or x (on
;;                                        the right side)
;;     y                               x
;;    / \     Right Rotation          /  \
;;   x   T3   - - - - - - - >        T1   y
;;  / \       < - - - - - - -            / \
;; T1  T2     Left Rotation            T2  T3
;; Keys in both of the above trees follow the
;; following order
;; keys(T1) < key(x) < keys(T2) < key(y) < keys(T3)
;; So BST property is not violated anywhere.

(define (right-rotate tree)
  (make-tree (entry (left-branch tree))
             (left-branch (left-branch tree))
             (make-tree (entry tree)
                        (right-branch (left-branch tree))
                        (right-branch tree))))

(define (left-rotate tree)
  (make-tree (entry (right-branch tree))
             (make-tree (entry tree)
                        (left-branch tree)
                        (left-branch (right-branch tree)))
             (right-branch (right-branch tree))))

(define (rebalance-LL tree)
  (right-rotate tree))

(define (rebalance-RR tree)
  (left-rotate tree))

(define (rebalance-LR tree)
  (right-rotate (make-tree (entry tree)
                           (left-rotate (left-branch tree))
                           (right-branch tree))))

(define (rebalance-RL tree)
  (left-rotate (make-tree (entry tree)
                          (left-branch tree)
                          (right-rotate (right-branch tree)))))

(define ll-unbalanced-tree
  (make-tree 12
             (make-tree 8 5 (list))
             (list)))

;; (rebalance-ll ll-unbalanced-tree)

(define rr-unbalanced-tree
  (make-tree 5
             (list)
             (make-tree 12 8 (list))))

;; (rebalance-rr rr-unbalanced-tree)

;; Ex. 2.63 Procedures that convert binary trees to lists.
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; Answer:
;; The two solutions walk binary trees in the same way. However, the solution
;; with append would grow at about n.log(n) as append traverses one of the lists
;; at each tree-halving step. The copy-to-list solution actually a simple tree
;; walk that iteratively conses up to the final list. It should grow at the order
; of O(n) because the function walks each item once.
;;
;; Note: In case of set representation of fig. 2.16, for the same set of
;; members, we cannot expect equality to hold across the set representations,
;; after we flatten them. This is because sets guarantee only unique membership,
;; not order of members. The order of items in the flattened list is the order
;; in which we walk the tree representation.


;; Example Trees

;; Fig. 2.16.1 Tree representations of set #{1, 3, 5, 7, 9, 11}
(define ex-tree-fig-2.16-1

  (make-tree 7
             (make-tree 3 1 5)
             (make-tree 9 '() 11)))

(define ex-tree-fig-2.16-2
  (make-tree 3
             1
             (make-tree 7
                        5
                        (make-tree 9 '() 11))))

(define ex-tree-fig-2.16-3
  (make-tree 5
             (make-tree 3 1 '())
             (make-tree 9 7 11)))

(define big-tree
  (make-tree 42
             (make-tree 42 example-tree example-tree)
             (make-tree 42 example-tree example-tree)))

(define bigger-tree
  (make-tree 42
             (make-tree 42 big-tree big-tree)
             (make-tree 42 big-tree big-tree)))

(define bigger-bigger-tree
  (make-tree 42
             (make-tree 42 bigger-tree bigger-tree)
             (make-tree 42 bigger-tree bigger-tree)))

(define even-bigger-tree
  (make-tree 42
             (make-tree 42 bigger-bigger-tree bigger-bigger-tree)
             (make-tree 42 bigger-bigger-tree bigger-bigger-tree)))

(define ok-thats-a-big-enough-tree
  (make-tree 42
             (make-tree 42 even-bigger-tree even-bigger-tree)
             (make-tree 42 even-bigger-tree even-bigger-tree)))

(define i-told-you-to-stop-tree
  (make-tree 42
             (make-tree 42 ok-thats-a-big-enough-tree ok-thats-a-big-enough-tree)
             (make-tree 42 ok-thats-a-big-enough-tree ok-thats-a-big-enough-tree)))

;; (map (lambda (tree)
;;        (equal? (tree->list-1 tree)
;;                (tree->list-2 tree)))
;;      (list example-tree
;;            ex-tree-fig-2.16-1
;;            ex-tree-fig-2.16-2
;;            ex-tree-fig-2.16-3
;;            ll-unbalanced-tree
;;            rr-unbalanced-tree
;;            big-tree
;;            i-told-you-to-stop-tree))


;; To count steps, we can modify the function definitions:

(define tree->list-counter 0)

(define (inc-tree->list-counter!)
  (set! tree->list-counter (+ tree->list-counter
                              1)))

(define (tree->list-1-with-count! tree)
  (inc-tree->list-counter!)

  (define (accumulate op initial sequence)
    (inc-tree->list-counter!)

    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

  (define (my-append seq1 seq2)
    (accumulate cons seq2 seq1))

  (if (null? tree)
      (list)
      (my-append (tree->list-1-with-count! (left-branch tree))
                 (cons (entry tree)
                       (tree->list-1-with-count! (right-branch tree))))))

(define (tree->list-2-with-count! tree)
  (define (copy-to-list tree result-list)
    (inc-tree->list-counter!)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
