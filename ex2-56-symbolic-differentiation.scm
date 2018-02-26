;; Chapter 2.3.2 Symbolic Differentiation example


;; Filling in the required selectors, constructors, and predicates:

;; (variable? e)
(define (variable? x)
  (symbol? x))

;; (same-variable? v1 v2)
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; (sum? e)
(define (sum? e)
  (and (pair? e) (eq? '+ (car e))))

;; (addend e)
(define addend cadr)

;; (augend e)

;; Ex.2.57 (any number of augends)
(define (augend exp)
  (define (aug-hlp exp)
    (if (null? exp)
        0
        ;; implement in terms of make-sum to preserve summing logic
        (make-sum (car exp) (aug-hlp (cdr exp)))))
  (aug-hlp (cddr exp)))


;; (make-sum a1 a2)
(define (make-sum-naive a1 a2)
  (list '+ a1 a2))

(define (make-sum a1 a2)
  (cond ((eq? a1 0) a2)
        ((eq? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))


;; (product? e)
(define (product? e)
  (and (pair? e) (eq? '* (car e))))

;; (multiplier e)
(define multiplier cadr)

;; (multiplicand e)

;; Solve Ex.2.57 (any number of multiplicands)
(define (multiplicand exp)
  (define (mul-hlp exp)
    (if (null? exp)
        1
        ;; implement in terms of make-product to preserve summing logic
        (make-product (car exp) (mul-hlp (cdr exp)))))
  (mul-hlp (cddr exp)))


;; (make-product m1 m2)
(define (make-product-naive m1 m2)
  (list '* m1 m2))


(define (=number? x y)
  (and (number? x) (number? y) (eq? x y)))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else
         (list '* m1 m2))))



;; Ex. 2.56 Implement differentiation rule for exponentiation
;; d(u^n)/dx = n*u^(n-1) * du/dx

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '^)))

(define base cadr)

(define exponent caddr)

(define (make-exponentiation u n)
  (cond ((=number? u 0) 1)
        ((=number? n 1) u)
        ((and (number? u) (number? n)) (expt u n))
        (else
         (list '^ u n))))


;; Implement basic rules of differentiation:
(define (deriv exp var)
  (cond ((number? exp)
         0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         ;; Ex.2.56 extend for exponentiation
         (let ((u (base exp))
               (n (exponent exp)))
           (make-product n
                         (make-product
                          (make-exponentiation u (make-sum n -1))
                          (deriv u var)))))
        (else
         (error "Unknown expression type: DERIV" exp))))



;; Examples:

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)

(deriv (make-exponentiation 'x 2) 'x)

(deriv '(* x y (+ x 3)) 'x)

(deriv
 (deriv
  (deriv '(+ (* x y z) (* x x) (^ x 3))
         'x)
  'x)
 'x)
