;; Exercise 2.14. Demonstrate that Lem is right. Investigate the behavior of the system on a variety of
;; arithmetic expressions.

;; Make some intervals A and B, and use them in computing the expressions A/A and A/B.
;; You will get the most insight by using intervals whose width is a small percentage of the
;; center value. Examine the results of the computation in center-percent form (see exercise 2.12).

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (lower-bound y))))
    (make-interval (* p1 p3) (* p2 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

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
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define interval-one (make-center-percent 5 1.0)) ; => (4.95 . 5.05)
(define interval-two (make-center-percent 12 2.0)) ; => (11.76 . 12.24)

(par1 interval-one interval-two) ; => (41610.846475179715 . 43055.148746610255)
(par2 interval-one interval-two) ; => (453.6345954341113 . 464.15920326227234)

(percent (par1 interval-one interval-two)) ; => 1.7058823529411835
(percent (par2 interval-one interval-two)) ; => 1.1467290194278916

(div-interval interval-one interval-one) ; => (.9801980198019803 . 1.) WTF?
(div-interval interval-one interval-two) ; => (.16685289311803153 . .17366321528611445)

; I think the problem is that par1 assumes R1 in the numerator is the same as R1 in the
; denominator; likewise with R2?
