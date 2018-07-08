;; Exercise 1.45. We saw in section 1.3.3 that attempting to compute square roots by naively finding a
;; fixed point of y -> x/y does not converge, and that this can be fixed by average damping. The same
;; method works for finding cube roots as fixed points of the average-damped y -> x/y 2 . Unfortunately,
;; the process does not work for fourth roots -- a single average damp is not enough to make a
;; fixed-point search for y -> x/y 3 converge.

;; On the other hand, if we average damp twice (i.e., use the average damp of the average damp of y -> x/y 3 )
;; the fixed-point search does converge.

;; Do some experiments to determine how many average damps are required to compute nth roots as a fixed-point
;; search based upon repeated average damping of y -> x/y^n-1 . Use this to implement a simple procedure
;; for computing nth roots using fixed-point, average-damp, and the repeated procedure of
;; exercise 1.43. Assume that any arithmetic operations you need are available as primitives.

(define tolerance 0.00001)

(define (average x y) (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f
               (repeated f (- n 1)))))

(define (square x) (* x x))

(define (expt b n)
  (cond ((= n 0) 1)
        ((even? n) (expt (square b) (/ n 2)))
        (else (* b (expt b (- n 1))))))

(define (convergence-test f first-guess guesses)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess i)
    (let ((next (f guess)))
      (newline)
      (display next)
      (cond ((close-enough? guess next) true)
            ((< i 1) false)
            (else (try next (- i 1))))))
  (try first-guess guesses))

; find nth-root for x
(define (nth-root x n)
  (lambda (y) (/ x (expt y (- n 1)))))

;----------

(convergence-test (nth-root 25 2) 1.0 100) ; #f

(convergence-test (average-damp (nth-root 25 2)) 1.0 100) ; #t

(convergence-test (average-damp (nth-root 8 3)) 1.0 100) ; #f

(convergence-test (average-damp (average-damp (nth-root 8 3))) 1.0 100) ; #t

(convergence-test (average-damp (average-damp (nth-root 16 4))) 1.0 100) ; #t

(convergence-test (average-damp (average-damp (nth-root 32 5))) 1.0 100) ; #t

(convergence-test ((repeated average-damp 2) (nth-root 64 6)) 1.0 100) ; #t

(convergence-test ((repeated average-damp 2) (nth-root 128 7)) 1.0 100) ; #t

(convergence-test ((repeated average-damp 2) (nth-root 256 8)) 1.0 100) ; #f

(convergence-test ((repeated average-damp 3) (nth-root 512 9)) 1.0 100) ; #t

(convergence-test ((repeated average-damp 3) (nth-root 1024 10)) 1.0 100) ; #t

(convergence-test ((repeated average-damp 3) (nth-root 2048 11)) 1.0 100) ; #t

(convergence-test ((repeated average-damp 3) (nth-root 4096 12)) 1.0 100) ; #t

(convergence-test ((repeated average-damp 3) (nth-root 8192 13)) 1.0 100) ; #t

(convergence-test ((repeated average-damp 3) (nth-root 16384 14)) 1.0 100) ; #t

(convergence-test ((repeated average-damp 3) (nth-root 32768 15)) 1.0 200) ; #t

(convergence-test ((repeated average-damp 3) (nth-root 65536 16)) 1.0 200) ; #f

; because I'm a dingus I didn't see that an average damp is needed every time the next power of 2 is reached e.g.
; 1 damp will work until n = 4,
; 2 damps will work until n = 8,
; 3 damps will work until n = 16

(define (log-2 n)
  (/ (log n) (log 2)))

(define (nth-root-damped x n)
  (fixed-point ((repeated average-damp (ceiling (log-2 n)))
                (nth-root x n))
               1.0))

(nth-root-damped 8 3) ; = 2.000002163438156
(nth-root-damped 16 4) ; = 2.0000000000021965
(nth-root-damped 32 5) ; = 2.000002548192768
