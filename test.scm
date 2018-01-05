(define (sqrt-iter guess old-guess x)
  (if (good-enough-improved guess old-guess)
      guess
      (sqrt-iter (improve guess x)
                 guess
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough-improved guess old-guess) ; check if difference between guesses is a tiny fraction of the guess
  (< (abs (- old-guess guess))
     (/ guess 100000)))

(define (sqrt x)
  (sqrt-iter 1.0 2.0 x))

;(sqrt 0.000001)

(sqrt 1000000000000)

