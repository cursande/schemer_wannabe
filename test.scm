; Exercise 1.18. Using the results of exercises 1.16 and 1.17, devise a procedure that generates an
; iterative process for multiplying two integers in terms of adding, doubling, and halving and uses a
; logarithmic number of steps.

;; *1.16*

; (define (square x) (* x x))

; (define (fast-expt b n)
;   (fast-expt-iter b n 1)) ; a = 1 at the beginning of the process

; (define (fast-expt-iter b n a) ; iterative exponentiation
;   (cond ((= n 0) a) ; result (a) at the end of the process
;         ((even? n) (fast-expt-iter (square b) (/ n 2) a)) ; if n is even we can just square b and halve n instead of decrementing
;         (else (fast-expt-iter b (- n 1) (* a b))))) ; otherwise get the product of a and b and decrement n

; (define (double x) (+ x x))

; (define (halve x) (/ x 2)) ; only for even numbers

; (define (* a b)
;   (cond ((= a 0) 0) ; a is our counter/iterator, b is result
; 	((even? a) (* (halve a) (double b))) ; e.g. 6 * 6 = 3 * 12
; 	(else (+ b (* (- a 1) b))))) ; e.g. 3 * 12 = 12 + 2 * 12, makes a even again. Eventually you're left with 36 + 0 * 36 = 36
;; *1.18*

(define (double x) (+ x x))

(define (halve x) (/ x 2)) ; only for even numbers

(define (* a b)
  (*-iter a b 0))

(define (*-iter a b n) ; a is counter, b tracks multiplier, n tracks total sum/result
  (cond ((= a 0) n)
	((even? a) (*-iter (halve a) (double b) n))
	(else (*-iter (- a 1) b (+ b n)))))

