; Ex. 2.06 Church Numerals

(define zero 
  (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
  
(define one
  (lambda (f) (lambda (x) (f x))))
  
(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define three
  (lambda (f) (lambda (x) (f (f (f x))))))

(define (add m n)
  (lambda (f) (lambda (x) ((m f) 
                           ((n f) x)))))

; (((add two two) square) 2)
; ;Value: 65536

; (square (square (square (square 2))))
; ;Value: 65536

                           
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Arriving at a general way to add two values:

; Suppose add-1 n is rewritten as:
; (define (add one n)
  ; (lambda (f) (lambda (x) ((one f) ((n f) x)))))

; (define (add one n)
  ; (lambda (f) (lambda (x) (((lambda (f) 
                               ; (lambda (x) (f x)))
                             ; f)
                            ; ((n f) x)))))

; (define (add one n)
  ; (lambda (f) (lambda (x) ((lambda (x) (f x)) ((n f) x)))))
  
; (define (add one n)
  ; (lambda (f) (lambda (x) (f ((n f) x)))))
  
; Which is the same as the definition for add-1 n 

; Likewise (add-2 n) and (add two n) are exactly similar:

; (define (add two n)
  ; (lambda (f) (lambda (x) ((two f) ((n f) x)))))

; (define (add-2 n)
  ; (lambda (f) (lambda (x) ((f (f ((n f) x)))))))

  
; This can be generalised to arrive at (add m n).

; (define (add m n)
  ; (lambda (f) (lambda (x) ((m f) 
                           ; ((n f) x)))))

