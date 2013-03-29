; Compute pi by using approximation for pi/8

(define (pi until)
    (* 4.0 
        ; Correct design using
        ; pi/4 = (2.4.8...(n-2))*(4.6.8...n) / (3.5.7.9...(n-1))^2
        (if (even? until)
            (/ (* (multi-fact 2 2 (- until 2))
                  (multi-fact 4 2 until))
               (square (multi-fact 3 2 (- until 1))))
            (/ (* (multi-fact 2 2 (- until 1))
                  (multi-fact 4 2 (+ until 1)))
               (square (multi-fact 3 2 until))))))
       ; FAULTY DESIGN using
       ; pi/4 = 2 * ((4.6.8...)/(3.5.7....))^2
       ; (if (even? until)
            ; (square (/ (multi-fact 4 2 until)
                       ; (multi-fact 3 2 (+ until 2))))
            ; (square (/ (multi-fact 4 2 (- until 2))
                       ; (multi-fact 3 2 until))))))

                       
(define (multi-fact start step n)
    ;(display multi-fact) ; debugging
    (define (identify x) x)

    (define (next x) (+ x step))

    (cond ((= n 0) 1)
          ((= n 1) 1)
          (else (product identify start next n))))


; Product computation via an iterative process
(define (product term a next b)
    (define (product-iter a result)
        (if (> a b)
            result
            (product-iter (next a) (* result (term a)))))
    (product-iter a 1))
    
    
(define (square x) (* x x))


(define (even? x)
    (= (remainder x 2) 0))
    
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Debuggig the FAULTY DESIGN of the 'pi' procedure:
; ; The digits of the results converged but the decimal shifted as
; ; the order of magnitude of the terminal value--'until'--grew.
; ; I played with how I passed 'until' to the multi-fact definition
; ; and a pattern emerged--the value of pi was being scaled by the
; ; value of 'until' (or thereabouts). Then I looked back at the
; ; textbook and realised that reformulating the pi/4 definition
; ; into a pi/8 definition was hasty. The numerator is NOT the 
; ; square of a single (factorial) series, unlike the denominator.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The faulty design copied for reference.
; ; (define (pi until)
    ; ; (* 8.0 
       ; ; (if (even? until)
            ; ; (square (/ (multi-fact 4 2 until)
                       ; ; (multi-fact 3 2 (+ until 2))))
            ; ; (square (/ (multi-fact 4 2 (- until 2))
                       ; ; (multi-fact 3 2 until))))))


; ;(- until 1) and (+ until 1)
; (pi 9999)
; ;Value: 31417.49737149267

; (pi 10000)
; ;Value: 31417.49737149267

; (pi 10001)
; ;Value: 31423.780556792

; (pi 10002)
; ;Value: 31423.780556792


; ; (- until 2) and (+ until 2)
; (pi 9999)
; ;Value: 31417.49737149267

; (pi 10000)
; ;Value: 3141121418619.3345

; (pi 10001)
; ;Value: 31423.780556792

; (pi 10002)
; ;Value: 3143006562714.116


; ; (- until 1) and (+ until 1)
; (pi 9999)
; ;Value: 31417.49737149267

; (pi 10000)
; ;Value: 3.1417497371492674e-4

; (pi 10001)
; ;Value: 31423.780556792

; (pi 10002)
; ;Value: 3.141121481441764e-4



; ; until as it is
; (pi 9999)
; ;Value: 3.1417497371492674e-4

; (pi 10000)
; ;Value: 31417.49737149267

; (pi 10001)
; ;Value: 3.141121481441764e-4

; (pi 10002)
; ;Value: 31423.780556792


; ; (+ until 1) and (- until 1)
; (pi 9999)
; ;Value: 3.1417497371492674e-4

; (pi 10000)
; ;Value: 3.141121481441764e-4

; (pi 10001)
; ;Value: 3.141121481441764e-4

; (pi 10002)
; ;Value: 3.140493476948618e-4