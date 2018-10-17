;; Exercise 2.10. Ben Bitdiddle, an expert systems programmer, looks over Alyssa’s shoulder and
;; comments that it is not clear what it means to divide by an interval that spans zero.

;; Modify Alyssa’s code to check for this condition and to signal an error if it occurs.


(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (lower-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (spans-zero? x)
  (and (<= (lower-bound x) 0)
       (>= (upper-bound x) 0)))

(define (div-interval x y)
  (display (lower-bound y))
  (newline)
  (display (spans-zero? y))
  (if (or (spans-zero? x)
          (spans-zero? y))
      "Cannot divide interval than spans 0"
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define interval-one (make-interval 7 12))
(define interval-two (make-interval 5 8))
(define interval-three (make-interval -2 2))

(div-interval interval-one interval-two) ; => (.875 . 1.5)
(div-interval interval-one interval-three) ; => "Cannot divide interval than spans 0"
