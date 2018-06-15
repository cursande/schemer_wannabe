;; Exercise 1.35. Show that the golden ratio (section 1.2.2) is a fixed point of the transformation:
;; x -> 1 + (1/x)
;; and use this fact to compute by means of the fixed-point procedure.


; ϕ = (1 + √5) / 2
; ϕ^2 = ϕ + 1

; => ϕ = 1 + (1 / ϕ)

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

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

; find fixed-point of golden raio
(fixed-point (lambda (x) (+ 1 (/ 1 x)))
             1.0)

; = 1.6180327868852458
