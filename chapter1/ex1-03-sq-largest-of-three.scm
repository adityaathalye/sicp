;Accept three numbers and sum the squares of the largest two

(define (square x) (* x x))

(define (sum-sq x y) (+ (square x) (square y)))

(define (sq-two-max a b c)
  (cond ((and (> a b) (> b c)) (sum-sq a b))
	((and (> b a) (> a c)) (sum-sq a b))
	((and (> a c) (> c b)) (sum-sq a c))
	((and (> c a) (> a b)) (sum-sq a c))
	((and (> c b) (> b a)) (sum-sq c b))
	((and (> b c) (> c a)) (sum-sq c b))))





