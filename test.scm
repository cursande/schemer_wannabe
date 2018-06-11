;; Exercise 1.33. You can obtain an even more general version of accumulate (exercise 1.32) by
;; introducing the notion of a filter on the terms to be combined. That is, combine only those terms
;; derived from values in the range that satisfy a specified condition.
;; The resulting filtered-accumulate abstraction takes the same arguments as accumulate,
;; together with an additional predicate of one argument that specifies the filter.

;; Write filtered-accumulate as a procedure. Show how to express the following using filtered-accumulate:

(define (filtered-accumulate combiner null-value filter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (filter a)
                  (combiner result (term a))
                  result))))
  (iter a null-value))

;; a. the sum of the squares of the prime numbers in the interval a to b
;; (assuming that you have a prime? predicate already written)

; prime stuff
(define (square x) (* x x))

(define (ex-mod-mr base ex m)
  (cond ((= ex 0) 1)
        ((even? ex)
         (remainder (square (non-trivial-root? (ex-mod-mr base (/ ex 2) m) m))
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
  (cond ((and (not (= x 1))
              (not (= x (- n 1)))
              (= (remainder (square x) n) 1))
         0)
        (else x)))
; prime filter

(define (increment x) (+ x 1))

(define (sum-of-squared-primes a b)
  (filtered-accumulate + 0 miller-rabin-prime? square a increment b))

(sum-of-squared-primes 1 10) ; = 88
(sum-of-squared-primes 10 15) ; = 290

;; b. the product of all the positive integers less than n that are relatively prime to n
;; (i.e., all positive integers i < n such that GCD(i,n) = 1).



