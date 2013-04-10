; Ex. 1.43 Repeated f

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (define (recurse result count)
        (if (= count n)
            result
            (recurse (compose f result) (+ count 1))))
    (recurse f 1))
    
