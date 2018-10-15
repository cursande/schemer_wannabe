;; Exercise 2.8. Using reasoning analogous to Alyssa’s, describe how the difference of two intervals
;; may be computed. Define a corresponding subtraction procedure, called sub-interval.

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

; lower-bound for difference will be the lower bound - upper-bound,
; the upper-bound will be the opposite, representing the greatest possible difference
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define interval-one (make-interval 4 5))
(define interval-two (make-interval 5 8))

(sub-interval interval-one interval-two) ; => (0 . -4)
