; An example of tree recursion
; Chapter 1.2.2 Counting Change
	; Constructing the recursion logic with two ideas:
	; [A] Where the number of ways to count change for an AMOUNT 
	; using n KINDS-OF-COINS is equal to:
		; NUM. of ways to count using coins except FIRST-DENOMINATION
		; PLUS no. ways to count AMOUNT minus FIRST-DENOMINATION 
		; using all currently available denominations.
	; [B] And Where Degenerate Cases i.e. the outermost leaves of 
	; the recursion tree are defined as follows:
		; If AMOUNT = 0, then set no. of ways to count change = 1
		; If AMOUNT < 0 or if zero KINDS-OF-COINS are left, then
		; set the number of ways to count change = 0

(define (count-change amount)
	(cc amount 5)) ; Procedure (cc ... ) is defined bleow.

(define (cc amount kinds-of-coins)
	(cond 	((= amount 0) 1) 
			((or (< amount 0) (= kinds-of-coins 0)) 0)
			(else (+ (cc amount (- kinds-of-coins 1))
				(cc (- amount (first-denomination kinds-of-coins))
					kinds-of-coins))))) 
					; Degenerate case: See [B] at top
					; else recurse the counting logic: See [A] at top

(define (first-denomination kinds-of-coins)
	(cond 	((= kinds-of-coins 1) 1)
			((= kinds-of-coins 2) 5)
			((= kinds-of-coins 3) 10)
			((= kinds-of-coins 4) 25)
			((= kinds-of-coins 5) 50)
			))