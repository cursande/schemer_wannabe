(define (sqrt-iter guess x) ;to be repeatedly called until guess is 'good enough' as defined below
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x) ;to gradually improve the guess via successive approximations
  (average guess (/ x guess)))

(define (average x y) ;to find average of guess and previous guess
  (/ (+ x y) 2))

(define (good-enough? guess x) ;to test whether the guess is accurate enough
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x) ;to start the sqrt-iter 'loop', with an initial guess of 1
  (sqrt-iter 1.0 x))

(sqrt 0.000001)
;should be 0.001, but scheme returns 0.031260655525445276, as our definition of 'good-enough' is too imprecise for a number this small.

(sqrt 10000000000000)
;should be 3162277.660..., however, scheme gets stuck in a loop. I believe this is because it hits a point where it alternates between two guesses that aren't 'good enough'.
