
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

(define (sum-of-squares x y z)
  (+ (* x x) (* y y) (* z z))) 

;; *1.4*

; Add b's absolute value to a

;; *1.5*

; Applicative: the interpreter will attempt to evaluate (p) as an operand before applying the procedure, and it won't know what to do

; Normal-order: the interpreter would simply sub in 0 and (p) and, since the first condition in the if statement will be true, it will return 0 as (p) won't matter
