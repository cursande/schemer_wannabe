
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

;; *1.17*

(define (double x) (+ x x))

(define (halve x) (/ x 2)) ; only for even numbers

(define (* a b)
  (cond ((= a 0) 0) ; a is our counter/iterator, b is result
	((even? a) (* (halve a) (double b))) ; e.g. 6 * 6 = 3 * 12
	(else (+ b (* (- a 1) b))))) ; e.g. 3 * 12 = 12 + 2 * 12, makes a even again. Eventually you're left with 36 + 0 * 36 = 36

;; *1.18*

(define (* a b)
  (*-iter a b 0))

(define (*-iter a b n) ; a is counter, b tracks multiplier, n tracks total sum/result
  (cond ((= a 0) n)
	((even? a) (*-iter (halve a) (double b) n))
	(else (*-iter (- a 1) b (+ b n)))))

;; *1.19*

; 'Show that if we apply such a transformation T pq twice, the effect is the same as using a single transformation T p’q’
; of the same form, and compute p’ and q’ in terms of p and q.

; Tpq

; a <- bq + aq + ap 
; b <- bp + aq

; successive squaring: find the 'square' of Tpq by subbing in first iteration into the next one 

; a <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
; b <- (bp + aq)p + (bq + aq + ap)q

; then it's a matter of manipulating the above to be written as Tpq again..

; a: bpq + aq^2 + bqq + aq^2 + apq + bqp + aqp + ap^2
; => b(pq + q^2 + qp) + a(q^2 + pq + qp) + a(q^2 + p^2)
; => b(q^2 + 2pq) + a(2pq + q^2) + a(p^2 + q^2)

; b: bp^2 + aqp + bq^2 + aq^2 + apq
; => (bp^2 + q^2) + (2apq + aq^2)
; => b(p^2 + q^2) + a(2pq + q^2)

; so squared form of Tpq is...

;p' = p^2 + q^2
;q' = 2pq + q^2

(define (square x) (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square p) (square q)) ; p squared, transformation like in fast-expt
		   (+ (* 2 p q) (square q)) ; q squared
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))


;; *1.20*

; in normal order eval the substitution model is used, the program has to 'store up' each recursive call until it finally needs to evaluate it e.g. when b = 0
; e.g.

(gcd 40 (remainder 206 40))
; a = 40
; b = (remainder 206 40)

; so on the next call to gcd...
(gcd (remainder 206 40)
     (remainder 40 (remainder 206 40)))
; a = (remainder 206 40)
; b = (remainder 40 (remainder 206 40))

; We also call remainder when checking that b = 0..

; it would take many calls to remainder to eventually return a in normal-order eval.
; For now I cannot be bothered working out exactly how many.

; The difference between this and applicative-order eval is that (remainder a b) will be evaluated each time it's called, so it doesn't keep stacking up.
; each new call to gcd just has the new values for a and b e.g.

(gcd 206 40)
; => (gcd 40 (remainder 206 40))
(gcd 6 40)
; => (gcd 6 (remainder 40 6))
(gcd 6 4)
; => (gcd 4 (remainder 6 4))
(gcd 4 2)
; => (gcd 2 (remainder 4 2))
(gcd 2 0)
; => 2

; so it's just the 4 remainder operations here.

;; *1.21*

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2)) ; starting with 2, find the smallest divisor of a given number

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(smallest-divisor 199) ; => 199
(smallest-divisor 1999) ; => 1999
(smallest-divisor 19999) ; => 7

; *1.22*
; returns 3 smallest primes within a range from "min" to "max", and the time
; it takes to run timed-prime-test with them

(define (three-smallest-primes min max)
  (search-for-primes min max 3))

(define (search-for-primes min max counter)
  (cond ((= counter 0) true)
        ((= counter max) false)
        ((prime? min)
         (timed-prime-test min)
         (search-for-primes (+ min 1) max (- counter 1)))
        (else (search-for-primes (+ min 1) max counter))))

;------ results ------

; 1000 ~ 10,000

; 1009 *** .01
; 1013 *** 0.
; 1019 *** 0.

; 10,000 - 100,000

; 10007 *** 0.
; 10009 *** 0.
; 10037 *** 0.

; 100,000 - 100,000,000

; 100003 *** 0.
; 100019 *** 0.
; 100043 *** 0.


; 1,000,000 - 10,000,000

; 1000003 *** 0.
; 1000033 *** 0.
; 1000037 *** 0.

; Hm. Doesn't support sqrt(n) prediction at all! Maybe this computer is too fast.
; After looking online, the answer seems to be testing with even bigger numbers, so...

; 1,000,000,000 - 10,000,000,000

; 1000000007 *** .04999999999999999
; 1000000009 *** 4.0000000000000036e-2
; 1000000021 *** .03999999999999998

; 10,000,000,000 - 100,000,000,000

; 10000000019 *** .12
; 10000000033 *** .12
; 10000000061 *** .1200000000000001

; sqrt(1000000000) => 31622.78
; sqrt(10000000000) => 100000
; 0.05 * sqrt(10) => 0.16

; Not quite getting the same as sqrt(n) prediction, but reasonably close between each range

; *1.23*

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (next td)
  (if (= td 2)
      3
      (+ td 2)))

; testing above with prime?:

; 1,000,000,000 - 10,000,000,000

; 1000000007 *** .03
; 1000000009 *** .01999999999999999
; 1000000021 *** .03

; 10,000,000,000 - 100,000,000,000

; 10000000019 *** .07999999999999996
; 10000000033 *** .07999999999999996
; 10000000061 *** .07000000000000006

; not quite half the speed, but pretty close.

; *1.24*

; When using fast-prime? instead...

; 1,000,000,000 - 10,000,000,000

; 1000000007 *** 0.
; 1000000009 *** 9.999999999999998e-3
; 1000000021 *** 0.

; 10,000,000,000 - 100,000,000,000

; 10000000019 *** 0.
; 10000000033 *** 1.0000000000000002e-2
; 10000000061 *** 0.

; Appears to be in line with log(n) prediction

;; *1.25*

;; (define (ex-mod base ex m)
;;   (cond ((= ex 0) 1)
;;         ((even? ex)
;;          (remainder (square (ex-mod base (/ ex 2) m))
;;                     m))
;;         (else
;;          (remainder (* base (ex-mod base (- ex 1) m))
;;                     m))))

(define (ex-mod base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; On testing, it becomes much slower with big numbers. The main issue with using
; fast-expt is that we end up multiplying huge numbers: as fast-expt runs you never take the remainder of the exponent,
; you just keep squaring / multiplying until n = 0.

;; *1.26*

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

; this is slower because it needs to do the same thing twice: * gets called with 2 arguments,
; square gets called with just the one call to expmod.
; Both arguments need to be evaluated first then here, which means twice as many calls to expmod.

;; *1.27*


; tests whether a^n is congruent to a mod n for every a < n
(define (fermat-prime? n)
  (fermat-prime-iter n 1))

(define (fermat-prime-iter n a)
  (cond ((= a n) true) ; if a reaches n without returning false, n is prime
        ((not (= (ex-mod a n n) a)) false) ; if a^n is not congruent to a mod n, n is composite
  (else (fermat-prime-iter n (+ a 1))))) ; test all a < n

; smaller Carmichael numbers: 561, 1105, 1729, 2465, 2821, and 6601

(fermat-prime? 561) ; => #t
(fermat-prime? 1105) ; => #t
(fermat-prime? 1729) ; => #t
(fermat-prime? 2465) ; => #t
(fermat-prime? 2821) ; => #t
(fermat-prime? 6601) ; => #t

;; *1.28*

(define (square x) (* x x))

(define (ex-mod-mr base ex m)
  (cond ((= ex 0) 1)
        ((even? ex)
         (remainder (square (non-trivial-root? (ex-mod-mr base (/ ex 2) m) m)) ; returning 0 will signal that a nontrivial square root exists
                    m))
        (else
         (remainder (* base (ex-mod-mr base (- ex 1) m))
                    m))))

(define (miller-rabin-prime? n)
  (miller-rabin-prime-iter n 1))

(define (miller-rabin-prime-iter n a)
  (cond ((= a n) true)
        ((not (= (ex-mod-mr a n n) a)) false)
        (else (miller-rabin-prime-iter n (+ a 1)))))

(define (non-trivial-root? x n)
  (cond ((and (not (= x 1)) ; x is not equal to 1
              (not (= x (- n 1))) ; ...and not equal to -1
              (= (remainder (square x) n) 1)) ; ...and its square is equal to 1 modulo n
         0) ; non-trivial of square root of 1 exists -> n is not prime
        (else x))) ; otherwise return original value untouched

;; *1.29*

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (next n) (+ n 2))
  (* (/ h 3.0) 
     (+ (* 2 (sum y 2 next (- n 2))) ; sum of all even values between a and b
        (* 4 (sum y 1 next (- n 1))) ; sum of all odd values between a and b
        (y 0) ; first
        (y n)))) ; and last...all multiplied by h/3
