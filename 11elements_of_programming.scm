;; Elements of Programming

;; Ultimately formalising intuitions about process, a way to answer the question 'how' precisely. Computers just the current medium for carrying that out

;; Basic Procedures

;; Squaring ->
(define (square x) (* x x))
   
;; (square 5) -> 25
;; (square (+ 3 5)) -> 64

;; or if it's clearer...
(define square (lambda (x) (* x x))) ;lambda tells lisp 'this is a procedure'. Above is just syntactic sugar

;; Compound procedures are used in the exact same way as primitive procedures (+, -,...)

;; Sum-of-squares (nested procedure) -> 
(define (sum-of-squares x y) 
  (+ (square x) (square y)))

;; (sum-of-squares 3 5) -> 34

;; Substitution model:
;;	- We need to evaluate the operator to apply the procedure
;;	- We need to evaluate the operands to get the arguments
;;	- This model becomes reductive, especially with regards to how it treats mutable data 

;; Applicative order vs Normal order
;;	- Applicative evaluates both operator and operands first
;;	- Normal-order 'lazy' evaluates, only evaluating once the argument values are needed
;;	- lisp uses applicative eval, but both approaches can be great tools to use


;; Conditional Expressions (cond)

(define (abs x)
  (if (< x 0)
      (- x)
      x ))
 
;; or the longer way...
(define (abs x)
  (cond ((> x 0) x)
	((= x 0) 0)
	((< x 0) (- x))))

;;  Each pair of expressions after cond are clauses:
;;	- first expression is a predicate, value = true or false

; logical composition operations = (and, or, not)
;	subexpressions in and and not are not necessarily evaluated, it will only evaluate what it needs to work out true or false

(and (> x 5) (< x 10)) ; 5 < x < 10

(define (>= x y)
  (or (> x y) (= x y))); is one number greater than another?

;; or alternatively

(define (>= x y)
  (not (< x y)))


;; Mathematical functions vs. computer procedures: procedures must be effective

;; finding a square root

(define (sqrt (sqrt-iter guess x) ;to be repeatedly called until guess is 'good enough' as defined below
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x) ;to gradually improve the guess via successive approximations
  (average guess (/ x guess)))

(define (good-enough? guess x) ;to test whether the guess is accurate enough
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x) ;to start the sqrt-iter 'loop', with an initial guess of 1
  (sqrt-iter 1.0 x))
