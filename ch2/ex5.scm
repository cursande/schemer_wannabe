;; Exercise 2.5. Show that we can represent pairs of nonnegative integers using only numbers and
;; arithmetic operations if we represent the pair a and b as the integer that is the product 2ᵃ3ᵇ.
;; Give the corresponding definitions of the procedures cons, car, and cdr.

(define (square x) (* x x))

(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n) (expt (square b) (/ n 2)))
        (else (* b (expt b (- n 1))))))

(define (divides? a b) (= (remainder b a) 0))

; product 2ᵃ3ᵇ
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

; work backwards from power to get original exponent
(define (find-expt power div)
  (define (iter x n)
    (if (divides? div x)
        (iter (/ x div) (+ n 1))
        n))
  (iter power 0))


; return a
(define (car z) (find-expt z 2))

; return b
(define (cdr z) (find-expt z 3))

(cons 5 5) ; => 7776
(car (cons 5 5)) ; => 5
(cdr (cons 5 5)) ; => 5
