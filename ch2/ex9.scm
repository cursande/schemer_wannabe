;; Exercise 2.9. The width of an interval is half of the difference between its upper and lower bounds.
;; The width is a measure of the uncertainty of the number specified by the interval. For some arithmetic
;; operations the width of the result of combining two intervals is a function only of the widths of the
;; argument intervals, whereas for others the width of the combination is not a function of the widths of
;; the argument intervals.

;; Show that the width of the sum (or difference) of two intervals is a function
;; only of the widths of the intervals being added (or subtracted). Give examples to show that this is not
;; true for multiplication or division.

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (lower-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;-------------------

(define (width-of-sum x y)
  (let ((sum (add-interval x y)))
    (/ (abs (- (lower-bound sum) (upper-bound sum)))
       2)))

(define (width-of-diff x y)
  (let ((diff (sub-interval x y)))
    (/ (abs (- (lower-bound diff) (upper-bound diff)))
       2)))

(define (width-of-product x y)
  (let ((product (mul-interval x y)))
    (/ (abs (- (lower-bound product) (upper-bound product)))
       2)))

(define (width-of-quotient x y)
  (let ((quotient (div-interval x y)))
    (/ (abs (- (lower-bound quotient) (upper-bound quotient)))
       2)))

(define interval-one (make-interval 4 5))
(define interval-two (make-interval 5 8))

(= (width-of-sum interval-one interval-two) (width-of-diff interval-one interval-two)) ; => #t
(= (width-of-product interval-one interval-two) (width-of-quotient interval-one interval-two)) ; => #f

; for individual interval
(define (width-of-interval x)
  (abs (/ (- (lower-bound x) (upper-bound x))
          2)))

(= (+ (width-of-interval interval-one) (width-of-interval interval-two))
   (width-of-sum interval-one interval-two)) ; => #t

(= (+ (width-of-interval interval-one) (width-of-interval interval-two))
   (width-of-diff interval-one interval-two)) ; => #t

(= (+ (width-of-interval interval-one) (width-of-interval interval-two))
   (width-of-product interval-one interval-two)) ; => #f

(= (+ (width-of-interval interval-one) (width-of-interval interval-two))
   (width-of-quotient interval-one interval-two)) ; => #f
