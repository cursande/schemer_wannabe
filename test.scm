
; Pascal's triangle / binomial coefficients

; recursive process 

(define (pascal r c) ; r = rows, c = col
  (cond ((= c 1) 1) 
	((= c r) 1) ; first col always 1, final column (equal to current row) always 1 as well
	(else (+ (pascal (- r 1) (- c 1))
		 (pascal (- r 1) c))))) ; like fib, work back recursively to the beginning, and then sub the right values in


