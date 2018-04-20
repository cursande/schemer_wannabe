
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Exercise 1.20. The process that a procedure generates is of course dependent on the rules used by the
;; interpreter. As an example, consider the iterative gcd procedure given above. Suppose we were to
;; interpret this procedure using normal-order evaluation, as discussed in section 1.1.5. (The normal-order-evaluation rule for if is described in
;; exercise 1.5.) Using the substitution method (for normal order), illustrate the process generated in evaluating (gcd 206 40) and indicate the remainder operations that are actually performed. How many remainder operations are actually
;; performed in the normal-order evaluation of (gcd 206 40)? In the applicative-order evaluation?

; in normal order eval the substitution model is used, the program has to 'store up' each recursive call until it finally needs to evaluate it e.g. when b = 0
; e.g.

(gcd 40 (remainder 206 40))
; a = 40
; b = (remainder 206 40)

; so on the next call to gcd...
(gcd (remainder 206 40)
     (remainder 40 (remainder 206 40)))
; a = (remainder 206 40)
; b = (remainder 40 (remainder 206 40))

; We also call remainder when checking that b = 0..

; it would take many calls to remainder to eventually return a in normal-order eval.
; For now I cannot be bothered working out exactly how many.

; The difference between this and applicative-order eval is that (remainder a b) will be evaluated each time it's called, so it doesn't keep stacking up.
; each new call to gcd just has the new values for a and b e.g.

(gcd 206 40)
; => (gcd 40 (remainder 206 40))
(gcd 6 40)
; => (gcd 6 (remainder 40 6))
(gcd 6 4)
; => (gcd 4 (remainder 6 4))
(gcd 4 2)
; => (gcd 2 (remainder 4 2))
(gcd 2 0)
; => 2

; so it's just the 4 remainder operations here.
