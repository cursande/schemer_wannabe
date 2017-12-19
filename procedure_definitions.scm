;; Procedure Definitions

;; General form of a procedure def: (define (<name> <formal parameters>) <body>)

;; Compound procedures are used in the exact same way as primitive procedures (+, -,...)

;; Squaring ->
(define (square x) (* x x))
   
;; (square 5) -> 25
;; (square (+ 3 5)) -> 64

;; Sum-of-squares (nested procedure) -> 
(define (sum-of-squares x y) ; should this be in above list??
  (+ (square x) (square y)))

;; (sum-of-squares 3 5) -> 34

;; Substitution model:
;;	- We need to evaluate the operator to apply the procedure
;;	- We need to evaluate the operands to get the arguments
