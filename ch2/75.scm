;; Exercise 2.75. Implement the constructor make-from-mag-ang in message-passing style. This
;; procedure should be analogous to the make-from-real-imag procedure given above.

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          ((eq? op 'real-part)
           (* (cos x) y))
          ((eq? op 'imag-part)
           (* (sin x) y))
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define data-object (make-from-mag-ang 20 90.0))

(data-object 'magnitude) ; => 20
(data-object 'angle) ; => 90.
(data-object 'real-part) ; => 36.727385563205274
(data-object 'imag-part) ; => 82.1650725654865
