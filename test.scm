;; Exercise 1.29. Simpson’s Rule is a more accurate method of numerical integration than the method
;; illustrated above. Using Simpson’s Rule, the integral of a function f between a and b is approximated
;; as:

;; where h = (b - a)/n, for some even integer n, and y k = f(a + kh). (Increasing n increases the accuracy of
;; the approximation.) Define a procedure that takes as arguments f, a, b, and n and returns the value of
;; the integral, computed using Simpson’s Rule. Use your procedure to integrate cube between 0 and 1
;; (with n = 100 and n = 1000), and compare the results to those of the integral procedure shown
;; above.

;; https://www.intmath.com/integration/6-simpsons-rule.php

(define (cube x) (* x x x))

(define (sum term a next b) ; expresses general abstraction of summing
  (if (> a b) ; when lower bound a exceeds upper bound b, return 0
      0
      (+ (term a)
         (sum term (next a) next b)))) ; else add together for instance the squared or cubed a, and then the next number in the series.

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* k h))))
  (define (next n) (+ n 2))
  (* (/ h 3.0) 
     (+ (* 2 (sum y 2 next (- n 2))) ; sum of all even values between a and b
        (* 4 (sum y 1 next (- n 1))) ; sum of all odd values between a and b
        (y 0) ; first
        (y n)))) ; and last...all multiplied by h/3

; TEST

(integral cube 0 1 0.01) ; = .24998750000000042
(integral cube 0 1 0.001) ; = .249999875000001

(simpson cube 0 1 100) ; = 0.25
(simpson cube 0 1 1000) ; =  0.25
(simpson cube 0 1 10) ; =  0.25
(simpson cube 0 1 5) ; =  .13493333333333332
