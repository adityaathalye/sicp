; Ex. 1.37 K-term Finite Generalized Continued Fractions
; https://en.wikipedia.org/wiki/Generalized_continued_fraction
; To solve this Iteratively, one may like to proceed "Bottom-up"
;http://www.cs.cas.cz/portal/AlgoMath/NumberTheory/ContinuedFractions/BasicDefinitions.htm

; The iterative transformation is defined thus:
; Suppose result = d(k) to begin with, then the next result is 
; arrived at, by the transform: result |--> (d(k-1) + n(k)/result)

(define (cont-frac d n k)
    (define (cont-frac-iter result count)
      (if (<= count 2)
          (/ (n 1) result)
          (cont-frac-iter (+ (d (- count 1)) (/ (n count) result))
                          (- count 1))))
    (cont-frac-iter (d k) k))
    
    
; Test data showing how cont-frac converges with value of k, when
; used to calculate the value of 1/phi.
; Assuming the output of cont-frac is consistently rounded UP to 
; the desired decimal accuracy;
; k >= 13 yields 1/phi accurate to 4 decimal places
; k >= 14 yields 1/phi accurate to 5 decimal places

; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 1)
; ;Value: 1.


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 2)
; ;Value: 1.


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 3)
; ;Value: .5


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 4)
; ;Value: .6666666666666666


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 5)
; ;Value: .6000000000000001


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 7)
; ;Value: .6153846153846154


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 8)
; ;Value: .6190476190476191

; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 9)
; ;Value: .6176470588235294


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 10)
; ;Value: .6181818181818182


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 11)
; ;Value: .6179775280898876


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 12)
; ;Value: .6180555555555556


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 13)
; ;Value: .6180257510729613


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 14)
; ;Value: .6180371352785146


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 15)
; ;Value: .6180327868852459


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 16)
; ;Value: .6180344478216819


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 17)
; ;Value: .6180338134001252


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 18)
; ;Value: .6180340557275542


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 19)
; ;Value: .6180339631667064


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 20)
; ;Value: .6180339985218034


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 21)
; ;Value: .6180339850173578


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 22)
; ;Value: .6180339901755971


; (cont-frac (lambda (x) 1.0)
           ; (lambda (x) 1.0)
           ; 30)
; ;Value: .6180339887505407