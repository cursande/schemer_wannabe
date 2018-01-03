
;; *Exercise 1.1*

10 ; -> 10
(+ 5 3 4) ; -> 12
(- 9 1) ; -> 8
(/ 6 2) ; -> 3
(+ (* 2 4) (- 4 6)) ; -> 4 + -2 -> 2
(define a 3) ; -> a
(define b (+ a 1)) ; -> b
(+ a b (* a b)) ;-> 19
(= a b) ; -> false (#f)
(if (and (> b a) (< b (* a b)))
    b
    a) ; -> b / 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; -> 16
(+ 2 (if (> b a) b a)) ; -> 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; -> 16


;; *1.2*

(/ (+ 5 4 (- 2 (- 3 (+ 6 0.8))))
   (* 3 (- 6 2) (- 2 7))) 

;; *1.3*

(define (square x) (* x x))

(define (sum-of-squares x y z) ; work out which is the smallest number, and add the squares of the other two
  (cond ((and (< x y) (< x z)) (+ (square y) (square z)))
        ((and (< y x) (< y z)) (+ (square x) (square z)))
        ((and (< z x) (< z y)) (+ (square x) (square y)))))	

;; *1.4*

; Add b's absolute value to a

;; *1.5*

; Applicative: the interpreter will attempt to evaluate (p) as an operand before applying the procedure, and it won't know what to do

; Normal-order: the interpreter would simply sub in 0 and (p) and, since the first condition in the if statement will be true, it will return 0 as (p) won't matter

;; *1.6*

; After testing it out, it gets stuck in an infinite loop. If statements are special in that they evaluate either the then or else clauses...with the new-if function though, scheme will try and evaluate the else statement regardless of whether the predicate's conditions are met. So it will just keep running sqrt-iter over and over

;; *1.7*

(define (sqrt (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x) 
  (average guess (/ x guess)))



(define (good-enough? guess x) 
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(display (sqrt 0.000001))
;should be 0.001

(sqrt 99999999999)
;should be 316227.766015257
