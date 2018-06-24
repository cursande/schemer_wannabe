;; Exercise 1.40. Define a procedure cubic that can be used together with the newtons-method
;; procedure in expressions of the form

;; (newtons-method (cubic a b c) 1)

;; to approximate zeros of the cubic x^3 + ax^2 + bx + c.

; approx root of x^3 + x^2 + 2x + 3 = -1.2757
; approx root of x^3 + 2x^2 + 3x + 4 = -1.6506

(define tolerance 0.00001)

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* (square x) a)
                 (* b x)
                 c)))

(newtons-method (cubic 1 2 3) 1) ; = -1.2756822036498454
(newtons-method (cubic 2 3 4) 1) ; = -1.6506291914330982
