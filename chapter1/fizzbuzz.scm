;-*-Scheme-*-

; Fizzbuzz: For the numbers from 1 to 100,

; ; If number is multiple of 3, print fizz instead of the number
; ; If number is multiple of 5, print buzz instead of the number
; ; If number is multiple of 3 and of 5, print fizzbuzz instead 
; ; of the number.
; ; Otherwise, print the number itself.
; ; Each output should be followed by a new line.


; When the if statement and recursive procedure call appear 
; BEFORE the conditions, fizzbuzz is printed in ASCENDING order.
(define (fizzbuzz-a n)
    (if (> n 1)
        (fizzbuzz-a (- n 1))
        (display "Done!"))
        
    (if (= (modulo n 15) 0)
        (display "fizzbuzz ")
        (cond   ((= (modulo n 3) 0) (display "fizz "))
                ((= (modulo n 5) 0) (display "buzz "))
                (else
                    (display " -")
                    (display n)
                    (display "- ")
                    n))))

; When the if statement and recursive procedure call appear AFTER 
; the conditions, fizzbuzz is printed in DESCENDING order.
(define (fizzbuzz-b n)        
    (if (= (modulo n 15) 0)
        (display "fizzbuzz ")
        (cond   ((= (modulo n 3) 0) (display "fizz "))
                ((= (modulo n 5) 0) (display "buzz "))
                (else
                    (display " -")
                    (display n)
                    (display "- ")
                    n)))
    (if (> n 1)
        (fizzbuzz-b (- n 1))
        (display "Done!")))
    
; This version causes an error: "The object #t is not applicable."
; Note how the conditional is used... a direct translation of the
; fizzbuzz question.
(define (fizzbuzz-c n)
    (cond   ((and ((= (modulo n 3) 0) (= (modulo n 5) 0))) (display "fizzbuzz"))
            ((= (modulo n 3) 0) (display "fizz "))
            ((= (modulo n 5) 0) (display "buzz "))
            (else
                (display " -")
                (display n)
                (display "- ")
                n))
    (if (> n 1)
        (fizzbuzz-c (- n 1))
        (display "Done!")))
        
; This version works - the first conditional is simplified
(define (fizzbuzz-c n)
    (cond   ((= (modulo n 15) 0) (display "fizzbuzz"))
            ((= (modulo n 3) 0) (display "fizz "))
            ((= (modulo n 5) 0) (display "buzz "))
            (else
                (display " -")
                (display n)
                (display "- ")
                n))
    (if (> n 1)
        (fizzbuzz-c (- n 1))
        (display "Done!")))
               