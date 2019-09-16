;; Exercise 2.49. Use segments->painter to define the following primitive painters:
;; a. The painter that draws the outline of the designated frame.
;; b. The painter that draws an ‘‘X’’ by connecting opposite corners of the frame.
;; c. The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
;; d. The wave painter.

(define (make-vect x y) (list x y))

(define (xcor-vect vect) (car vect))

(define (ycor-vect vect) (cadr vect))

(define (add-vect vect1 vect2)
  (make-vect (+ (xcor-vect vect1) (xcor-vect vect2))
             (+ (ycor-vect vect1) (ycor-vect vect2))))

(define (sub-vect vect1 vect2)
  (make-vect (- (xcor-vect vect1) (xcor-vect vect2))
             (- (ycor-vect vect1) (ycor-vect vect2))))

(define (scale-vect s vect)
  (make-vect (* s (xcor-vect vect))
             (* s (ycor-vect vect))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (make-segment vect1 vect2) (list vect1 vect2))

(define (start-segment vector) (car vector))

(define (end-segment vector) (cadr vector))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; a)
(define top-segment (make-segment (make-vect 0 0.99) (make-vect 0.99 0.99)))
(define right-segment (make-segment (make-vect 0.99 0) (make-vect 0.99 0.99)))
(define left-segment (make-segment (make-vect 0 0) (make-vect 0 0.99)))
(define bottom-segment (make-segment (make-vect 0 0) (make-vect 0.99 0)))

(define (outline)
  (segments->painter
   (list top-segment
         right-segment
         left-segment
         bottom-segment)))
