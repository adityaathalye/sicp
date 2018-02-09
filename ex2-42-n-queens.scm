;; Ex. 2.42 N queens puzzle followed by 2.43 Louis Reasoner's bug


;; Ex. 2.42 Given algorithm:
(define (queens board-size)
  (define (queens-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row
                                    k ; col
                                    rest-of-queens))
                 ;; rows: 1 2 3 4 5 6 7 8
                 (enumerate-interval 1 board-size)))
          ;; cols: 1 2 3 4 5 6 7 8
          (queens-cols (- k 1))))))
  ;; board-size: 8
  (queens-cols board-size))


;; Check:
(quote
 (let ((board-sizes (enumerate-interval 1 10))
       (num-solutions (lambda (board) (length (queens board)))))
   (equal? (map num-solutions board-sizes)
           '(1 0 0 2 10 4 40 92 352 724))))


;; A solution:

(define empty-board
  ;; Empty set of positions
  '())


(define (position row col)
  (cons row col))


(define (row position)
  (car position))


(define (col position)
  (cdr position))


(define (adjoin-position new-row k rest-of-queens)
  (cons (position new-row k)
        rest-of-queens))


(define (safe? _ positions)
  ;; Argument `k' is irrelevant if we choose to cons the kth queen,
  ;; a.k.a. "new" queen, to the head of the set of "other" positions.
  (let ((new-queen     (car positions))
        (new-queen-row (caar positions))
        (new-queen-col (cdar positions))
        (other-queens  (cdr positions)))
    (define (same-row? pos)
      (= (row pos) new-queen-row))
    (define (same-col? pos)
      (= (col pos) new-queen-col))
    (define (same-diagonal? pos)
      ;; Intuition: On any x,y grid:
      ;; delta x,y for 1 diagonal move is always 1,1 (regardless of
      ;; direction). So delta x,y for n diagonal moves is always n,n.
      (= (abs (- (row pos) new-queen-row))
         (abs (- (col pos) new-queen-col))))
    (define (safe-pos? pos)
      (not (or (same-col? pos)
               (same-row? pos)
               (same-diagonal? pos))))
    ;; Is every other queen safe w.r.t. the new queen?
    (fold-left (lambda (bool pos)
                 (and bool (safe-pos? pos)))
               true
               other-queens)))


(define (dummy-safe? k positions)
  ;; Use this to complete queens, while solving the
  ;; rest of the problem.
  true)


(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))


(define (enumerate-interval from to)
  (define (ei r from to)
    (if (> from to)
        r
        (ei (cons to r) from (- to 1))))
  (if (< from to)
      (ei '() from to)
      (ei '() to from)))


;; Ex. 2.43 Louis reasoner's bug:
;;
;; The original solution space itself grows exponentially, if we
;; _don't_ filter out unsafe positions in every recursive call.
;;
;; (expt n n)
;;
;; By filtering out unsafe positions, we reduce the rate of growth.
;;
;; Louis's algorithm is "slow" because putting (queens-cols (- k 1))
;; inside makes for still worse exponential growth.
;;
;; For every k, we will go over each item of the board k times.
;;
;; If board size is 5, and k decays as k - 1, then we will
;; cover each of the 5 cols as:
;;
;;               | < --- replicated work ----------------->|
;; (* (expt 5 5) (expt 5 4) (expt 5 3) (expt 5 2) (expt 5 1))
;;
;; Value: 9765625
;;
;; If the original algorithm takes time T(n), for board size n,
;; then Louis's buggy one will take (* T(n) T(- n 1) T(- n 2) ...).




;; =====================================================
;; Riffing off Bill the lizard's solution:
;; =====================================================
;;
;; - I looked at his solution after hitting a wall on diagonal logic,
;;   and being quite unsure what the authors meant by `adjoin'.
;;
;; - cf. http://www.billthelizard.com/2011/06/sicp-242-243-n-queens-problem.html


(define (queens-bill board-size)
  (define (queens-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (bills-safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (bills-adjoin-position new-row
                                          k ; col
                                          rest-of-queens))
                 ;; rows: 1 2 3 4 5 6 7 8
                 (enumerate-interval 1 board-size)))
          ;; cols: 1 2 3 4 5 6 7 8
          (queens-cols (- k 1))))))
  ;; board-size: 8
  (queens-cols board-size))


;; Check:
(quote
 (let ((num-solutions '(1 0 0 2 10 4 40 92 352 724))
       (queens-proc (lambda (queen-proc)
                      (lambda (board)
                        (length (queen-proc board)))))
       (board-sizes (enumerate-interval 1 10)))

   (list (equal? num-solutions
                 ;; bill's variant
                 (map (queens-proc queens-bill) board-sizes))

         (equal? num-solutions
                 ;; my variant
                 (map (queens-proc queens) board-sizes)))))


(define (bills-adjoin-position new-row k rest-of-queens)
  (append rest-of-queens
          (list (position new-row k))))


(define (nth lst n)
  (cond
   ((or (null? lst)
        (> n (length lst))) (list))
   ((= 0 n) (car lst))
   (else (nth (cdr lst) (- n 1)))))


(define (bills-safe? k positions)
  ;; Variant of bill the lizard's procedure Are the other positions
  ;; safe, if we place the kth queen?
  (define kth-queen
    (nth positions (- k 1)))
  (define other-queens
    (filter (lambda (pos)
              (not (equal? kth-queen pos)))
            positions))
  (define (good-position? pos)
    (let ((krow (row kth-queen))
          (kcol (col kth-queen)))
      (not (or (= krow (row pos))
               (= (abs (- krow (row pos)))
                  (abs (- kcol (col pos))))))))
  (equal? other-queens
          (filter good-position? other-queens)))


;; ============================================================
;; Scratch work
;; ============================================================

;; Queens on a 4x4 board:
;;
;; RC 1 2 3 4
;; 1  * q * *
;; 2  * * * q
;; 3  q * * *
;; 4  * * q *

;; ((1 . 2) (2 . 4) (3 . 1) (4 . 3)) ; safe

(safe?       1 '((1 . 2) (2 . 4) (3 . 1) (4 . 3)))

(bills-safe? 1 '((1 . 2) (2 . 4) (3 . 1) (4 . 3)))
