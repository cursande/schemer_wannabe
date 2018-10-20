;; Exercise 2.12. Define a constructor make-center-percent that takes a center and a percentage
;; tolerance and produces the desired interval. You must also define a selector percent that produces
;; the percentage tolerance for a given interval. The center selector is the same as the one shown
;; above.

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Real engineering situations usually involve measurements with only a small uncertainty,
;; measured as the ratio of the width of the interval to the midpoint of the interval.
(define (percent i)
  (* 100.0 (/ (width i) (center i))))

(percent (make-interval 5 10))

(define (make-center-percent center percent-tolerance)
  (let ((tolerance (abs (/ (* center percent-tolerance) 100.0))))
    (make-interval (- center tolerance)
                   (+ center tolerance))))

(make-center-percent 10 20) ; => (8 . 12)
(make-center-percent 50 10) ; => (45 . 55)
