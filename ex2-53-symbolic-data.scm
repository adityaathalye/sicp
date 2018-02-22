;; Ch 2.3 Symbolic Data
(define (memq item x)
  (cond ((null? x ) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))


;; Ex. 2.53 Predicted evaluation results:

;; (list 'a 'b 'c) --> (a b c)

;; (list (list 'george)) --> ((george))

;; (cdr '((x1 x2) (y1 y2))) --> ((y1 y2))

;; (cadr '((x1 x2) (y1 y2))) --> (y1 y2)

;; (pair? (car '(a short list))) --> false

;; (memq 'red '((red shoes) (blue socks))) --> false

;; (memq 'red '(red shoes blue socks)) --> (red shoes blue socks)


;; Ex. 2.54 implement equal?
;;
;; We can define equal? recursively in terms of the basic eq?
;; equality of symbols by saying that a and b are equal? if they
;; are both symbols and the symbols are eq?, or if they are both
;; lists such that (car a) is equal? to (car b) and (cdr a) is
;; equal? to (cdr b).

(define (myequal? xs ys)
  (define (myeq? xs ys r)
    (cond
     ((or (null? xs) (null? ys))
      r)
     ((and (pair? (car xs)) (pair? (car ys)))
      (myeq? (cdr xs) (cdr ys) (myeq? (car xs) (car ys) r)))
     (else
      (myeq? (cdr xs) (cdr ys) (eq? (car xs) (car ys))))))
  (cond
   ((not (and (pair? xs) (pair? ys)))
    (eq? xs ys))
   ((not (length=? xs ys))
    false)
   (else
    (myeq? xs ys true))))

;; Test:
;;
;; (let* ((l1 (list 1 2 (list 3 4)))
;;        (l2 (list "a" "b" 'c 'd))
;;        (l3 '((1 2) ('a 'b) ("c" 'd) 42))
;;        (all-lists (list l1 l2 l3)))
;;   (list 'myequal? (map myequal? all-lists all-lists)
;;         'equal?   (map equal?   all-lists all-lists)))


;; Ex. 2.55 Eva Lu Ator types to the interpreter the expression
;;          (car ''abracadabra)
;; To her surprise, the interpreter prints back quote. Explain.

;; 'object is equivalent to (quote object), and
;; ''object is equivalent to (quote (quote object))
;; The interpreter evaluates ''object to a quoted list of symbols
;; where quote itself is the first symbol
