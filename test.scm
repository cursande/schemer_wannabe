;; Exercise 1.38. In 1737, the Swiss mathematician Leonhard Euler published a memoir
;; De Fractionibus Continuis, which included a continued fraction expansion for e - 2,
;; where e is the base of the natural logarithms. In this fraction, the N i are all 1,
;; and the D i are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program
;; that uses your cont-frac procedure from exercise 1.37 to approximate e, based on Euler’s expansion.

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (divides? a b)
  (= (remainder b a) 0))

(define (next-d k)
  (if (divides? 3 (+ k 1))
      (* 2 (/ (+ k 1) 3))
      1))

; e = 2.71828

(define (approx-e k)
  (+ 2 (cont-frac (lambda (n) 1.0)
                  next-d
                  k)))

(approx-e 1) ; = 3
(approx-e 2) ; = 2.6666666666666665
(approx-e 3) ; = 2.75
(approx-e 4) ; = 2.7142857142857144
(approx-e 10) ; = 2.7182817182817183
