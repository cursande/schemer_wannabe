;; Exercise 2.13. Show that under the assumption of small percentage tolerances there is a simple
;; formula for the approximate percentage tolerance of the product of two intervals in terms of the
;; tolerances of the factors. You may simplify the problem by assuming that all numbers are positive.

; tolerance of the factors?

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval p1 p4)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (* 100.0 (/ (width i) (center i))))

(define (make-center-percent center percent-tolerance)
  (let ((tolerance (abs (/ (* center percent-tolerance) 100.0))))
    (make-interval (- center tolerance)
                   (+ center tolerance))))

(define interval-one (make-center-percent 5 0.4)) ; => (4.98 . 5.02)
(define interval-two (make-center-percent 3 0.2)) ; => (2.994 . 3.006)

(percent (mul-interval interval-one interval-two)) ; => .599995200038381

; now the tolerance of the factors..

(percent interval-one) ; => .39999999999999153
(percent interval-two) ; => .19999999999999277

(+ .39999999999999153 .19999999999999277) ; => .5999999999999843 (close enough to 0.6)

; assuming small percentage tolerances..
(define (approx-tolerance-of-product x y)
  (+ (percent x) (percent y)))

(approx-tolerance-of-product interval-one interval-two) ; => .5999999999999843
