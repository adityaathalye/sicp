; Ex. 1.37 K-term Finite Generalized Continued Fractions
; https://en.wikipedia.org/wiki/Generalized_continued_fraction

; I correctly identified the transform f(i) |--> Ni/(Di + f(i + 1)),
; to be performed till the limit k is reached. Also identified a 
; need for a loop inside cont-frac. Yet I went stupidly alllll over 
; the place for a whole day, trying to implement it with let and/or 
; lambda and gave up, and looked at the answer at Scheme-wiki. 
; The direct answer is so trivial. I should have persisted. 
; Yeesh :( 
; Hat tip Scheme-wiki.

(define (cont-frac d n k)
  (define (recurse i)
    (/ (n i) 
       (+ (d i) 
	  (if (= i k)
	      0
	      (recurse (+ i 1))))))
  (recurse 1))
  
; To solve this recursively, one could perhaps also use the 
; "Fundamental Recurrence Formulas":
; https://en.wikipedia.org/wiki/Fundamental_recurrence_formulas
;http://www.cs.cas.cz/portal/AlgoMath/NumberTheory/ContinuedFractions/BasicDefinitions.htm