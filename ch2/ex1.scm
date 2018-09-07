;; Exercise 2.1. Define a better version of make-rat that handles both positive and negative
;; arguments. Make-rat should normalize the sign so that if the rational number is positive, both the
;; numerator and denominator are positive, and if the rational number is negative, only the numerator is
;; negative.

(define (make-rat n d)
  (let ((g (gcd n d)))
    (normalise (cons (/ n g) (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (gcd x y)
  (if (= y 0)
      (abs x)
      (gcd y (remainder x y))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  x)

(define (normalise rat)
  (cond ((and (negative? (numer rat)) (negative? (denom rat)))
         (cons (abs (numer rat)) (abs (denom rat))))
        ((or (negative? (numer rat)) (negative? (denom rat)))
         (cons (- 0 (abs (numer rat))) (denom rat)))
        (else (display (denom rat)) rat)))


(print-rat (make-rat 5 2)) ; => 5/2
(print-rat (make-rat -5 2)) ; => -5/2
(print-rat (make-rat -5 -2)) ; => 5/2
