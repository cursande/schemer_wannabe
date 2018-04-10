; Exercise 1.19. There is a clever algorithm for computing the Fibonacci numbers in a logarithmic
; number of steps. 

; Recall the transformation of the state variables a and b in the fib-iter process of
; section 1.2.2: a a + b and b a. Call this transformation T, and observe that applying T over and
; over again n times, starting with 1 and 0, produces the pair Fib(n + 1) and Fib(n). In other words, the
; Fibonacci numbers are produced by applying T n , the nth power of the transformation T, starting with
; the pair (1,0). 

; Now consider T to be the special case of p = 0 and q = 1 in a family of transformations
; T pq, where T pq transforms the pair (a,b) according to a bq + aq + ap and b bp + aq. Show that if
; we apply such a transformation T pq twice, the effect is the same as using a single transformation T p’q’
; of the same form, and compute p’ and q’ in terms of p and q. 

; Tpq

; a <- bq + aq + ap 
; b <- bp + aq

; successive squaring: find the 'square' of Tpq by subbing in first iteration into the next one 

; a <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
; b <- (bp + aq)p + (bq + aq + ap)q

; then it's a matter of manipulating the above to be written as Tpq again..

; a: bpq + aq^2 + bqq + aq^2 + apq + bqp + aqp + ap^2
; => b(pq + q^2 + qp) + a(q^2 + pq + qp) + a(q^2 + p^2)
; => b(q^2 + 2pq) + a(2pq + q^2) + a(p^2 + q^2)

; b: bp^2 + aqp + bq^2 + aq^2 + apq
; => (bp^2 + q^2) + (2apq + aq^2)
; => b(p^2 + q^2) + a(2pq + q^2)

; so squared form of Tpq is...

;p' = p^2 + q^2
;q' = 2pq + q^2

; This gives us an explicit way to square these transformations, and thus we can compute T n using successive squaring, as in the fast-expt
; procedure. Put this all together to complete the following procedure, which runs in a logarithmic
; number of steps: 

(define (square x) (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square p) (square q)) ; a <- bq + aq + ap squared, transformation like in fast-expt
		   (+ (* 2 p q) (square q)) ; b <- bp + aq squared
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))
