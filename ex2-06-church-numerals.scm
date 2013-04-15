; Ex. 2.06 Church Numerals

(define zero 
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
  
(define one
  (lambda (f) (lambda (x) (f x))))
  
(define two
  (lambda (f) (lambda (x) (f (f x)))))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Defining `one' (by trying to add-1 to zero by hand)

; (add-1 zero)

; (lambda (f) 
  ; (lambda (x) (f ((zero f) x))))
  
; (lambda (f) 
  ; (lambda (x) (f (((lambda (f) (lambda (x) x))
                   ; f)
                 ; x))))

; (lambda (f) 
  ; (lambda (x) (f ((lambda (x) x) 
                  ; x))))
                  
; (lambda (f)
  ; (lambda (x) (f x)))
  
; ; ; HENCE we can directly define `one' as
; (lambda (f) (lambda (x) (f x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Defining `two' (by trying to add-1 to one by hand)

; (add-1 one)

; (lambda (f)
  ; (lambda (x) (f ((one f) x))))

; (lambda (f)
  ; (lambda (x) (f (((lambda (f) (lambda (x) (f x)))
                   ; f) ; this f is passed as value to lambda (f)
                  ; x))))

; (lambda (f)
  ; (lambda (x) (f ((lambda (x) (f x))
                  ; x)))) ; this x is passed as value to lambda (x)
                  
; (lambda (f)
  ; (lambda (x) (f (f x))))

; ; ; HENCE we can directly define `two' as
; (lambda (f) (lambda (x) (f (f x))))
