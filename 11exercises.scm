
;; *Exercise 1.1*

10 ; -> 10
(+ 5 3 4) ; -> 12
(- 9 1) ; -> 8
(/ 6 2) ; -> 3
(+ (* 2 4) (- 4 6)) ; -> 4 + -2 -> 2
(define a 3) ; -> a
(define b (+ a 1)) ; -> b
(+ a b (* a b)) ;-> 19
(= a b) ; -> false (#f)
(if (and (> b a) (< b (* a b)))
    b
    a) ; -> b / 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; -> 16
(+ 2 (if (> b a) b a)) ; -> 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; -> 16


;; *1.2*

(/ (+ 5 4 (- 2 (- 3 (+ 6 0.8))))
   (* 3 (- 6 2) (- 2 7))) 

;; *1.3*

(define (square x) (* x x))

(define (sum-of-squares x y z) ; work out which is the smallest number, and add the squares of the other two
  (cond ((and (< x y) (< x z)) (+ (square y) (square z)))
        ((and (< y x) (< y z)) (+ (square x) (square z)))
        ((and (< z x) (< z y)) (+ (square x) (square y)))))	

;; *1.4*

; Add b's absolute value to a

;; *1.5*

; Applicative: the interpreter will attempt to evaluate (p) as an operand before applying the procedure, and it won't know what to do

; Normal-order: the interpreter would simply sub in 0 and (p) and, since the first condition in the if statement will be true, it will return 0 as (p) won't matter

;; *1.6*

; After testing it out, it gets stuck in an infinite loop. If statements are special in that they evaluate either the then or else clauses...with the new-if function though, scheme will try and evaluate the else statement regardless of whether the predicate's conditions are met. So it will just keep running sqrt-iter over and over

;; *1.7*

(define (sqrt-iter guess x) ;to be repeatedly called until guess is 'good enough' as defined below
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x) ;to gradually improve the guess via successive approximations
  (average guess (/ x guess)))

(define (average x y) ;to find average of guess and previous guess
  (/ (+ x y) 2))

(define (good-enough? guess x) ;to test whether the guess is accurate enough
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x) ;to start the sqrt-iter 'loop', with an initial guess of 1
  (sqrt-iter 1.0 x))

(sqrt 0.000001)
;should be 0.001, but scheme returns 0.031260655525445276, as our definition of 'good-enough' is too imprecise for a number this small.

(sqrt 10000000000000)
;should be 3162277.660..., however, scheme gets stuck in a loop. I believe this is because it hits a point where it alternates between two guesses that aren't 'good enough'.

; Alternate strategy for good-enough?:
(define (good-enough-improved guess old-guess) ; check if difference between guesses is a tiny fraction of the guess
  (< (abs (- old-guess guess))
     (/ guess 100000)))

;; *1.8*

(define (improve guess x) ; new improve method for finding cube roots
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

;; *1.9*

; The first is recursive, the second is iterative as the variables are simply updated state to state. If I stop the process at some point in the middle, I can simply continue it by running the process with the variables saved at that last point in time.

;; *1.10*

(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1)
		 (A x (- y 1))))))

(A 1 10) ; -> 1024
(A 2 4)  ; -> 65536
(A 3 3)  ; -> 65536

(define (f n) (A 0 n)) ; -> 2n
(define (g n) (A 1 n)) ; -> 2^n
(define (h n) (A 2 n)) ; -> 2^n^n...however long n is, minus 1 (*more concise  way -> 2^h(n-1))

;; *1.11*

;	     { n     if n < 3 }
; f(n) = 
;        { f(n - 1) + 2f(n - 2) + 3f(n - 3)     if n >= 3) }

; recursive

(define (f n)
  (cond ((< n 3) n)
	(else (+ (f (- n 1))
		 (* 2 (f (- n 2)))
		 (* 3 (f (- n 3)))))))

; iterative
(define (f n)
  (f-iter n 0 1 2)) ; initialise state with these variables

(define (f-iter i x y z) 
     (cond ((= i 0) x) ; use i as counter to track loops
	   (else (f-iter (- i 1) ; just one recursive call, passing in the updated values each time
			 y 
			 z 
			 (+ z (* 2 y) (* 3 x)))))) 

;; *1.12*

; recursive

(define (pascal r c) ; r = rows, c = col
  (cond ((= c 1) 1) 
	((= c r) 1) ; first col always 1, final column (equal to current row) always 1 as well
	(else (+ (pascal (- r 1) (- c 1))
		 (pascal (- r 1) c))))) ; like fib, work back recursively to the beginning, and then sub the right values in

;; *1.15*
; *(a)
 (define angle)
  (if (not (> (abs angle) 0.1)) ; check angle is larger than 0.1 
           angle
           (p (sine (/ angle 3.0)))) ; p isn't done until angle is already smaller than 0.1!

(/ 12.15 3) ; => 4.05
(/ 4.05 3) ; => 1.3499999999999999
(/ 1.3499999999999999 3) ; => .44999999999999996
(/ .44999999999999996 3) ; => 0.15
(/ 0.15 3) ; = 0.049999999999999996

; p gets applied 5 times

; *(b)
; if we take n to be the number of times p must be processed in sine, then we have to run p again everytime n is 3 times bigger, as above.
; so the order of growth for f(a) is O(log n) it seems, as constants don't matter (https://stackoverflow.com/questions/20512642/big-o-confusion-log2n-vs-log3n)

;; *1.16*

(define (square x) (* x x))

(define (fast-expt b n)
  (fast-expt-iter b n 1)) ; a = 1 at the beginning of the process

(define (fast-expt-iter b n a) ; iterative exponentiation
  (cond ((= n 0) a) ; result (a) at the end of the process
        ((even? n) (fast-expt-iter (square b) (/ n 2) a)) ; if n is even we can just square b and halve n instead of decrementing
        (else (fast-expt-iter b (- n 1) (* a b))))) ; otherwise get the product of a and b and decrement n
