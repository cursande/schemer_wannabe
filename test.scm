
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

(define (prime? n)
  (find-divisor n 2))

;-----------

(define (ex-mod base ex m)
  (cond ((= ex 0) 1)
        ((even? ex)
         (remainder (square (ex-mod base (/ ex 2) m))
                    m))
        (else
         (remainder (* base (ex-mod base (- ex 1) m))
                    m))))

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

;; Exercise 1.28. One variant of the Fermat test that cannot be fooled is called the Miller-Rabin test
;; (Miller 1976; Rabin 1980). This starts from an alternate form of Fermat’s Little Theorem, which states
;; that if n is a prime number and a is any positive integer less than n, then a raised to the (n - 1)st power
;; is congruent to 1 modulo n.

;; To test the primality of a number n by the Miller-Rabin test, we pick a
;; random number a < n and raise a to the (n - 1)st power modulo n using the expmod procedure.
;; However, whenever we perform the squaring step in expmod, we check to see if we have discovered
;; a ‘‘nontrivial square root of 1 modulo n,’’ that is, a number not equal to 1 or n - 1 whose square is
;; equal to 1 modulo n. It is possible to prove that if such a nontrivial square root of 1 exists, then n is not
;; prime. It is also possible to prove that if n is an odd number that is not prime, then, for at least half the
;; numbers a<n, computing a n-1 in this way will reveal a nontrivial square root of 1 modulo n. (This is
;; why the Miller-Rabin test cannot be fooled.)

;; Modify the expmod procedure to signal if it discovers a
;; nontrivial square root of 1, and use this to implement the Miller-Rabin test with a procedure analogous
;; to fermat-test. Check your procedure by testing various known primes and non-primes. Hint:
;; One convenient way to make expmod signal is to have it return 0.

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
