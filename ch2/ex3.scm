;; Exercise 2.3. Implement a representation for rectangles in a plane. (Hint: You may want to make use
;;  of exercise 2.2.) In terms of your constructors and selectors, create procedures that compute the
;; perimeter and the area of a given rectangle. Now implement a different representation for rectangles.

;; Can you design your system with suitable abstraction barriers, so that the same perimeter and area
;; procedures will work using either representation?

(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment x y) (cons x y))

(define (start-segment p) (car p))
(define (end-segment p) (cdr p))

(define (midpoint-segment p1 p2)
  (cons
   (/ (+ (car p1) (car p2)) 2)
   (/ (+ (cdr p1) (cdr p2)) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  p)

(define (perimeter rectangle)
  (* (+ (car rectangle) (cdr rectangle)) 2))

(define (area rectangle)
  (* (car rectangle) (cdr rectangle)))

; 1st
; a rectangle is 2 sets of parallel line segments. If we find the difference between the 2 x's for the width, and
; the difference between the 2 y's for the height, we'll have the dimensions of the rectangle. We can create the 3rd
; and 4th points with the data available.

(define (rectangle-height-and-width p1 p2)
  (cons (rect-width p1 p2) (rect-height p1 p2)))

(define (rect-width p1 p2)
  (abs (- (x-point p1) (x-point p2))))

(define (rect-height p1 p2)
  (abs (- (y-point p1) (y-point p2))))

(define first-segment (make-segment 3 4))
(define second-segment (make-segment 5 5))
(define test-rectangle (rectangle-height-and-width first-segment second-segment)) ; => (2 . 1)
(perimeter test-rectangle) ; => 6
(area test-rectangle) ; => 2

; 2nd
; We could have four points instead, and simply build the rectangle with the point differences. We have to assume the order of the points to an extent, and that a lower level of abstraction has taken care of that.
(define (make-point x y) (cons x y))
(define first-point (make-point 3 3))
(define second-point (make-point 3 5))
(define third-point (make-point 5 5))
(define fourth-point (make-point 5 3))

(define (rectangle-by-point-difference p1 p2 p3 p4)
  (define (x-diff p1 p2) (abs (- (car p1) (car p2))))
  (define (y-diff p1 p2) (abs (- (cdr p1) (cdr p2))))
  (cons (x-diff p1 p3) (y-diff p2 p4)))

(define rectangle (rectangle-by-point-difference first-point second-point third-point fourth-point)) ; => (2 . 2)

(perimeter rectangle) ; => 8
(area rectangle) ; => 4
