
(define (square x) (* x x))

(define (cbrt-iter guess old-guess x)
  (if (good-enough-improved guess old-guess)
      guess
      (cbrt-iter (improve guess x)
                 guess
                 x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough-improved guess old-guess) ; check if difference between guesses is a tiny fraction of the guess
  (< (abs (- old-guess guess))
     (/ guess 100000)))

(define (cbrt x)
  (cbrt-iter 1.0 2.0 x))

;(sqrt 0.000001)

(cbrt 1000000000000)

