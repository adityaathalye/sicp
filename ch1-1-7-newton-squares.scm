; FINDING SQUARE ROOTS

; Mathematically, we can define WHAT IS a square root:
;   _/x = the y such that y>=0 and y^2 = x

; BUT Programmatically, we need a way - i.e. HOW TO arrive
; at a square-root, with reasonable accuracy.

; Newton's method of successive approximation is one such way.
  ; Initially GUESS that some y is the square root of x
  ; Now find a BETTER GUESS closer to the square root by
  ; averaging guess y with x/y till desired accuracy is achieved

; Expressed as a Procedure below:


