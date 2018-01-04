(define (sqrt-iter guess old-guess x) ;to be repeatedly called until guess is 'good enough' as defined below
  (if (good-enough-improved guess old-guess)
      guess
      (sqrt-iter guess
		 (improve guess x)
                 x)))

(define (improve guess x) ;to gradually improve the guess via successive approximations
  (average guess (/ x guess)))

(define (average x y) ;to find average of guess and previous guess
  (/ (+ x y) 2))

(define (good-enough-improved guess old-guess) ; to test difference between new and old guess, stopping if the difference is a very small fraction of the guess
  (< (/ (abs (- old-guess guess)) guess)
     (/ 0.0001 guess)))

(define (sqrt x) ;to start the sqrt-iter 'loop', give it two guesses as parameters and the number whose square root we want to find
  (sqrt-iter 1 2 x))

(sqrt 0.000001)

(sqrt 1000000000000)

