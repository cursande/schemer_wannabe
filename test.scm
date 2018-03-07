
(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1)) ; 
	   angle
	   (p (sine (/ angle 3.0))))) ; procedure will only return angle if angle is already smaller than 0.1!


