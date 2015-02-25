;; Ex. 2.17 last-pair

(define (last-pair xs)
  (if (null? xs)
      xs
      (if (null? (cdr xs))
          xs
          (last-pair (cdr xs)))))


;; Ex. 2.18 reverse

(define (my-reverse xs)
  (define (my-reverse-hlp xs rs)
    (if (null? xs)
        rs
        (my-reverse-hlp (cdr xs) 
                        (cons (car xs) rs))))
  ;; `nil` doesn't seem to work, but an empty list does.
  (my-reverse-hlp xs (list)))


;; Ex. 2.19 refactor `count-change` into `cc`, then write
;; first-denomination, except-first-denomination, and no-more?
(define us-coins (list 50 25 10 5 1))
(define us-coins-alt-1 (list 1 5 10 25 50))
(define us-coins-alt-2 (list 50 10 5 25 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values))
                     coin-values)))))

(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

;; (= (cc 100 us-coins)
;;    (cc 100 us-coins-alt-1)
;;    (cc 100 us-coins-alt-2))
;; #t
;; We are computing all possible ways of counting change. 
;; The cc algorithm generates a recursive process,
;; which will grow unevaluated branches as long as amount > 0
;; and there are coin values left to cover.
;; Every coin value is tested at least once in the computation.



;; Ex. 2.20 same-parity to define procedures with 
;; arbitrary number of arguments

(define (same-parity x . xs)
  (let ((same-parity? (if (odd? x) odd? even?)))
    (define (same-parity-hlp ys)
      (cond ((null? ys) ys)
            ((same-parity? (car ys)) 
             (cons (car ys)
                   (same-parity-hlp (cdr ys))))
            (else (same-parity-hlp (cdr ys)))))
    (same-parity-hlp (cons x xs))))


;; Ex. 2.21 square-list alternatives

(define (square-list-1 items)
  (if (null? items)
      (list)
      (cons (* (car items)(car items))
            (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x))
       items))


;; Ex. 2.22 Louis Reasoner's (failed) attempts explained

;; Attempt 1
;; This iterative computation causes the first item to be
;; consed first, the second item second, and so on.
;; Since consing adds an item to the head of the list,
;; and since the last item will be the last to consed on
;; the resulting list will be reversed.

;; Attempt 2
;; This will cause a _list_ to be consed to the car position
;; of a pair. The procedure will generate a nested pair of pairs,
;; like this: (((() . 1). 2) . 3)



;; Ex. 2.23 for-each
;; This can be a simple wrapper over map, because the purpose of
;; for-each is to use a function to perform side-effects.
;; Functions that use for-each can simply ignore the return 
;; sequence of for-each.
(define (my-for-each f xs)
  (map f xs))

;; An alternative approach without using map
(define (my-for-each-2 f xs)
  (if (null? xs)
      #t
      (let ((_ (f (car xs)))) ;; Do side effect, ignore return value
        (my-for-each-2 f (cdr xs)))))



;; Ex. 2.24 
;; > (list 1 (list 2 (list 3 4)))
;; (1 (2 (3 4)))
;;
;; In box-pointer notation:
;;
;;                               (2 (3 4))       (3 4)
;; (1 (2 (3 4))) --> [*|*] ------> [*|*] ------> [*|*] --> [*|/]
;;                    |             |             |         |
;;                    v             v             v         v
;;                   [*|/]         [*|/]         [*|/]      4
;;                    |             |             |
;;                    v             v             v
;;                    1             2             3


;; Ex. 2.25 Our life cdr be betr
(car 
 (cdr 
  (car 
   (cdr 
    (cdr '(1 3 (5 7) 9))))))

(car (cdr 
      (car (cdr 
            (car (cdr 
                  (car (cdr 
                        (car (cdr 
                              (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))


;; 2.26 listy
(define some-xs (list 1 2 3))
(define some-ys (list 4 5 6))

(equal? (append some-xs some-ys)
        '(1 2 3 4 5 6))

(equal? (cons some-xs some-ys)
        '((1 2 3) 4 5 6))

(equal? (list some-xs some-ys)
        '((1 2 3) (4 5 6)))


;; 2.27 deep-reverse
(define (my-reverse xs)
  (define (my-reverse-hlp xs rs)
    (if (null? xs)
        rs
        (my-reverse-hlp (cdr xs) 
                        (cons (car xs) rs))))
  ;; `nil` doesn't seem to work, but an empty list does.
  (my-reverse-hlp xs (list)))


(define (deep-reverse xs)
  (define (deep-reverse-hlp xs rs)
    (cond ((null? xs) rs)
          ((not (pair? xs)) xs) ;; Additional base case
          (else 
           (deep-reverse-hlp (cdr xs)
                             (cons (deep-reverse (car xs)) rs)))))
  (deep-reverse-hlp xs (list)))



;; Ex.2.28 fringe
(define (fringe xs) ; After lots of trial and error
  (define (fringe-hlp xs rs)
    (cond ((null? xs) rs)
          ((not (pair? xs)) (cons xs rs))
          (else 
           (fringe-hlp (cdr xs)
                       (fringe-hlp (car xs) rs)))))
  (reverse (fringe-hlp xs (list))))

;; After looking at solutions on SchemeWiki
;; http://community.schemewiki.org/?sicp-ex-2.28
(define (fringe2 xs)
  (define (fringe-hlp xs rs)
    (cond ((null? xs) rs)
          ((not (pair? xs)) (cons xs rs))
          (else 
           (fringe-hlp (car xs) ; car instead of cdr, w.r.t `fringe`, above
                       (fringe-hlp (cdr xs) rs))))) ; cdr instead of car
  (fringe-hlp xs (list)))


;; After looking at solutions on SchemeWiki
;; http://community.schemewiki.org/?sicp-ex-2.28
(define (fringe3 xs)
  (define (fringe-hlp xs rs)
    (cond ((null? xs) rs)
          ((not (pair? xs)) (cons xs rs))
          (else 
           (fringe-hlp (cdr xs)
                       ;; append is bad. Adds O(n) time at each step.
                       (append rs (fringe3 (car xs)))))))
  (fringe-hlp xs (list)))


;; Ex. 2.29 binary mobile

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch binary-mobile)
  (car binary-mobile))

(define (right-branch binary-mobile)
  (cadr binary-mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight binary-mobile)
  (+ (branch-structure (left-branch binary-mobile))
     (branch-structure (right-branch binary-mobile))))

(define (balanced-binary? mobile)
  (let ((lb (left-branch binary-mobile))
        (rb (right-branch binary-mobile)))
    (= (* (branch-length lb) (branch-structure lb))
       (* (branch-length rb) (branch-structure rb)))))

;; If the constructors use cons instead of list,
;; then we must refactor only those selectors that
;; use cadr. We replace all those cadrs with cdr.


;; Ex. 2.30 square-tree

(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) ((lambda (x) (* x x)) tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree-map x)
             (* x x)))
       tree))


;; Ex. 2.30 tree-map
(define (tree-map f tree)
  (map (lambda (x)
         (if (pair? x)
             (tree-map f x)
             (f x)))
       tree))


;; Ex. 2.31 Square-tree with tree-map
(define (square x) (* x x))

(define (square-tree-with-tree-map tree)
  (tree-map square tree))

(equal? (square-tree-with-tree-map
         (list 1
               (list 2 (list 3 4) 5)
               (list 6 7)))
        '(1 (4 (9 16) 25) (36 49)))


;; Ex. 2.32 Subsets
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) 
                            (cons (car s) x)) 
                          rest)))))

(subsets '(1 2 3))

;(append (subsets '(2 3))
;        (map ??? (subsets '(2 3))))
;
;(append (append (subsets '(3))
;                (map ??? (subsets '(3))))
;        (map ??? (append (subsets '(3))
;                         (map ??? (subsets '(3))))))
;
;(subsets '(3))
;(append (subsets '())
;        (map ??? (subsets '())))
;
;
;(append (append (append (subsets '())
;                        (map ??? (subsets '())))
;                (map ??? (append (subsets '())
;                                 (map ??? (subsets '())))))
;        (map ??? (append (append (subsets '())
;                                 (map ??? (subsets '())))
;                         (map ??? (append (subsets '())
;                                          (map ??? (subsets '())))))))
;
;
; Intuitively, '???' has to do something about the car of s, 
; which we would otherwise lose at each step. 
;
;
;(append (append (append '()
;                        (map ??? '()))
;                (map ??? (append '()
;                                 (map ??? '()))))
;        (map ??? (append (append '()
;                                 (map ??? '()))
;                         (map ??? (append '()
;                                          (map ??? '()))))))
;
;(append (append (map ??? '())
;                (map ??? (map ??? '())))
;        (map ??? (append (map ??? '())
;                         (map ??? (map ??? '())))))
;
;
;
