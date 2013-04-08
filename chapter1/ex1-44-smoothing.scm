; Ex. 1.44 Smoothing a function

(define (smooth f dx)
    (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
    
(define (n-fold-smooth f dx n)
    (repeated (smooth f dx) n))

; From Ex. 1.43: Repeated f

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (define (recurse result count)
        (if (= count n)
            result
            (recurse (compose f result) (+ count 1))))
    (recurse f 1))

    
; Testing for a simple sine function
; ((smooth sin 0.00001) 2)
; ;Value: .9092974267953718

; ((smooth sin 0.0001) 2)
; ;Value: .9092974237946901

; ((smooth sin 0.001) 2)
; ;Value: .9092971237265647

; ((smooth sin 0.01) 2)
; ;Value: .909267117164036

; ((smooth sin 0.1) 2)
; ;Value: .9062689603873233

; (sin 2)
; ;Value: .9092974268256817
