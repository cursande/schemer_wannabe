
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

