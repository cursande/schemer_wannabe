
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2)) ; starting with 2, find the smallest divisor of a given number

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

; A number can only be prime if its smallest divisor is itself

(define (prime? n) (= n (smallest-divisor n)))

; this approach finds the solution in O(sqrt(n)) time

; the below calculates the exponential of n % n...
; * note, an exponential is a function where the variable quantity is the exponent, not the base

(define (ex-mod base ex m)
  (cond ((= ex 0) 1)
	((even? ex)
	 (remainder (square (ex-mod base (/ ex 2) m))
		    m))
	(else
	  (remainder (* base (ex-mod base (- ex 1) m))
		     m))))

; as with fast-expt, this uses successive squaring

; The Fermat test is performed by choosing at random a number a between 1 and n - 1 inclusive and
; checking whether the remainder modulo n of the nth power of a is equal to a.

(define (fermat-test n)
  (define (test-n a)
    (= (ex-mod a n n) a))
  (test-n (+ (random (- n 1)) 1)))

; combine them all together to make a method for finding prime numbers in O(log n) time...

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

; this method is *probabilistic*, you determine the confidence of the result / how many times the fermat-test is run.
; then if any test fails, it just returns false
;======================================================

; *1.22*

(define (timed-prime-test n)
(newline)
(display n)
(start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))
      false))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  elapsed-time)

;-------
; returns 3 smallest primes within a range from "min" to "max", and the time
; it takes to run timed-prime-test with them

(define (three-smallest-primes min max)
  (search-for-primes min max 3))

(define (search-for-primes min max counter)
  (cond ((= counter 0) true)
        ((= counter max) false)
        ((fast-prime? min 10)
         (timed-prime-test min)
         (search-for-primes (+ min 1) max (- counter 1)))
        (else (search-for-primes (+ min 1) max counter))))

;------ results ------

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


; 1,000,000,000 - 10,000,000,000

; 1000000007 *** 0.
; 1000000009 *** 9.999999999999998e-3
; 1000000021 *** 0.

; 10,000,000,000 - 100,000,000,000

; 10000000019 *** 0.
; 10000000033 *** 1.0000000000000002e-2
; 10000000061 *** 0.

; Appears to be in line with log(n) prediction
