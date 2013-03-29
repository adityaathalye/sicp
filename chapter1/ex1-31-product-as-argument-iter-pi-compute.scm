; Ex. 1.31 Find pi using John Wallis's pi/4 formula (17th century)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; All variants of the pi computation in this script limit the range
; of computation by limiting the **VALUE OF THE** last term in the 
; multi-factorial series. 

; Another approach, not used in this script, is to limit the 
; computation **TO THE N-TH TERM** in the multi-factorial series.

; See also:
; ; APPENDIX A:
; ; Test data reveals asymptotic convergence to the value of pi.
; ; APPENDIX B:
; ; Debuggig the FAULTY DESIGN of the 'pi' procedure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Design #3:
; A correct (and cleaner) design using
; pi/4 = 2.((4.8...(k-2))^2).k / (3.5.7.9...(k-1))^2
; ; Also, after reviewing test output for this type of 
; ; design, it is clear that only even values of k are
; ; worth considering, as computation is duplicated for 
; ; (k-1) and k; assuming k is even.

(define (pi k)

    (define even-k 
        (if (even? k) k (+ k 1)))
    
    (* 4.0         
        (if (< even-k 3)
            1 ; To compensate for multi-fact's design given the 
              ; numerical approximation formula for pi/4.
            (/ (* 2 (square (multi-fact 4 2 (- even-k 2))) even-k)
               (square (multi-fact 3 2 (- even-k 1)))))))
        

; ; Design #2:
; ; Correct design using
; ; pi/4 = (2.4.8...(n-2))*(4.6.8...n) / (3.5.7.9...(n-1))^2
; (define (pi until)
    ; (* 4.0         
        ; (if (even? until)
            ; (/ (* (multi-fact 2 2 (- until 2))
                  ; (multi-fact 4 2 until))
               ; (square (multi-fact 3 2 (- until 1))))
            ; (/ (* (multi-fact 2 2 (- until 1))
                  ; (multi-fact 4 2 (+ until 1)))
               ; (square (multi-fact 3 2 until))))))

               
; ; Design #1:
; ; FAULTY DESIGN using
; ; pi/4 = 2 * ((4.6.8...)/(3.5.7....))^2
; (define (pi until)
    ; (* 4.0         
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


; Compute product using an iterative process
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
; APPENDIX A:
; Tests show that the computation SLOWS down EXPONENTIALLY with 
; increase in value of the limiting term of multi-factorial. And 
; computed pi converges asymptotically to the true value of pi.

; For the degree of accuracy in my computation, the truest value 
; of pi that can theoretically be achieved = 3.141592653589794. 

; It may take my procedure weeks if not months to converge to 
; this degree of accuracy! Assuming iterative 'product' procedure 
; prevents the overall script from reaching maxium recursion 
; depth and/or the limits of my PC's memory.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (pi 1)
; ;Value: 4.

; (pi 2)
; ;Value: 4.

; (pi 3)
; ;Value: 3.5555555555555554

; (pi 4)
; ;Value: 3.5555555555555554

; (pi 5)
; ;Value: 3.4133333333333336

; (pi 6)
; ;Value: 3.4133333333333336

; (pi 7)
; ;Value: 3.3436734693877552

; (pi 8)
; ;Value: 3.3436734693877552

; (pi 9)
; ;Value: 3.3023935500125976

; (pi 10)
; ;Value: 3.3023935500125976

; (pi 100)
; ;Value: 3.157339689217565

; (pi 1000)
; ;Value: 3.143163842419198

; (pi 10000)
; ;Value: 3.1417497371492673
; ;This computation lasted a few SECONDS

; (pi 100000)
; ;Value: 3.141608361592331
; ;This computation lasted a few MINUTES


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; APPENDIX B:
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