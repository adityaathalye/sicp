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

;; Ex. 2.33 map
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) 
              '()
              sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))


;; Ex. 2.34 Horner's rule
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* higher-terms x)))
              0
              coefficient-sequence))

(equal? (+ 1 (* 3 2) (* 5 2 2 2) (* 2 2 2 2 2))
        (horner-eval 2 (list 1 3 0 5 0 1)))


;; Ex. 2.35 count-leaves-mapreduce
(define (count-leaves t)
  (accumulate + 0 (map (lambda (_) 1)
                       (fringe2 t))))

(= (count-leaves '(1 (4 (9 16) 25) (36 49)))
   7)


;; Ex. 2.36 accumulate-n
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      ;; Accumulate op over a sequence of the first item of
      ;; each sequence and cons the result of that to the
      ;; result of accumulate-n over a sequence of the rest of
      ;; each sequence.
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))



; DIGRESSION: Solution to 'Sequs Horriblis', 4clojure #112
; https://www.4clojure.com/problem/solutions/112
; a.k.a.
; In which one rids oneself of an old deamon (and salvages 
; the tattered wind-blown remains of one's ego.)

; Solution to the 'Sequs Horriblis' problem from 4clojure.


; QUESTION: 
; Create a function which takes an integer and a nested collection
; of integers as arguments.
; Analyze the elements of the input collection and return a sequence 
; which maintains the nested structure, and which includes all elements 
; starting from the head whose sum is less than or equal to the input integer.

; And one slays an old enemy thusly:

; By nature processing a tree is a recursive problem.
(define (horriblis n xs)
  ; Base case 1. Can't process an empty list any further.
  (if (null? xs) 
      (list)
      ; When xs is not null, pair? must be true. 
      ; So we must inspect the head and the tail, and proceed
      ; based on our findings.
      (let ((hd (car xs))
            (tl (cdr xs)))
        ; Base case 2: We must stop processing right away, if the head
        ; is a number, and its value is greater than our checksum.
        (cond ((and (number? hd) (> hd n))
               (list)) 
              ; Recursive case 1: If the head 'hd' is a number and we have
              ; not reached or exceeded checksum yet, we must hold on
              ; to the head and process the tail 'tl'. 
              ; Also we must retain the nested list structure. Therefore, 
              ; 'hd' must be consed at the original head position, and 
              ; the result of processing 'tl' must appear at the tail 
              ; position.
              ; Further, we must account for the value of 'hd' that we
              ; just consumed. We must deduct it from the current checksum
              ; and feed the new checksum to the recursive to processs 'tl'.
              ((number? hd)
               (cons hd 
                     (horriblis (- n hd) tl)))
              ; Recursive case 2: 'hd' is not a number. In this case, we must
              ; place the result of recursively processing 'hd', at the head
              ; position, to retain structure. In other words wrap the result, 
              ; of processing 'hd', in a list. 
              (else (list (horriblis n hd)))))))


(define (horriblis2 n xs)
  (cond ((< n 0) (list))
        ((null? xs) (list))
        (else (let ((fst (car xs))
                    (rst (cdr xs)))
                (if (number? fst)
                    (if (> fst n)
                        (list)
                        (cons fst 
                              (horriblis2 (- n fst) rst)))
                    (list (horriblis2 n fst)))))))


(define (horrib n xs)
  (cond ((< n 0) (list))
        ((null? xs) (list))
        (else (let ((fst (car xs))
                    (rst (cdr xs)))
                (cond ((and (number? fst) (pair? rst) (number? (car rst)))
                       (cons fst 
                             (horrib (- n fst (car rst)) rst)))
                      ((number? fst)
                       (cons fst 
                             (horrib (- n fst) rst)))
                      (else (list (horrib n fst))))))))

"--------------------------------"
;#f
(horriblis 10 '(1 2 (3 (4 5) 6) 7))
(equal?  (horriblis 10 '(1 2 (3 (4 5) 6) 7))
         '(1 2 (3 (4))))


(horriblis 30 '(1 2 (3 (4 (5 (6 (7 8)) 9)) 10) 11))
(equal?  (horriblis 30 '(1 2 (3 (4 (5 (6 (7 8)) 9)) 10) 11))
         '(1 2 (3 (4 (5 (6 (7)))))))


(horriblis 9 '(0 1 2 3 4 5 6 7 8))
(equal?  (horriblis 9 '(0 1 2 3 4 5 6 7 8))
         '(0 1 2 3))


(horriblis 1 '(((((1))))))
(equal?  (horriblis 1 '(((((1))))))
         '(((((1))))))


(horriblis 0 '(1 2 (3 (4 5) 6) 7))
(equal?  (horriblis 0 '(1 2 (3 (4 5) 6) 7))
         '())


(horriblis 0 '(0 0 (0 (0))))
(equal?  (horriblis 0 '(0 0 (0 (0))))
         '(0 0 (0 (0))))


(horriblis 1 '(-10 (1 (2 3 (4 5 (6 7 (8)))))))
(equal?  (horriblis 1 '(-10 (1 (2 3 (4 5 (6 7 (8)))))))
         '(-10 (1 (2 3 (4)))))








