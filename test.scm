;; Exercise 1.39. A continued fraction representation of the tangent function was published
;; in 1770 by the German mathematician J.H. Lambert:where x is in radians.

;; Define a procedure (tan-cf x k) that computes an approximation to the tangent function based on
;; Lambert’s formula. K specifies the number of terms to compute, as in exercise 1.37.

(define (cont-frac n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (square x) (* x x))

(define (tan-cf x k)
  (define (next-n k)
    (if (= k 1)
    x
    (- (square x))))
  (define (next-d k)
    (- (* 2 k) 1))
  (cont-frac next-n next-d k))

(tan-cf 1.0 30) ; = 1.557407724654902
(tan-cf 10.0 30) ; = .6483608274590866
(tan-cf 20.0 30) ; = 2.237158493771187
