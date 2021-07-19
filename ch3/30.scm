;; Exercise 3.30. Figure 3.27 shows a ripple-carry adder formed by stringing together n full-adders.
;; This is the simplest form of parallel adder for adding two n-bit binary numbers. The inputs A 1 , A 2 ,
;; A 3 , ..., A n and B 1 , B 2 , B 3 , ..., B n are the two binary numbers to be added (each A k and B k is a
;; 0 or a 1). The circuit generates S 1 , S 2 , S 3 , ..., S n , the n bits of the sum, and C, the carry from the
;; addition. Write a procedure ripple-carry-adder that generates this circuit. The procedure
;; should take as arguments three lists of n wires each -- the A k , the B k , and the S k -- and also another
;; wire C. The major drawback of the ripple-carry adder is the need to wait for the carry signals to
;; propagate. What is the delay needed to obtain the complete output from an n-bit ripple-carry adder,
;; expressed in terms of the delays for and-gates, or-gates, and inverters?

;; TODO Actually test this out using propagate/simulation

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (ripple-carry-adder ak bk sk)
  (define (iter a b s c-in c-out)
    (if (null? (cdr a))
        (full-adder (car a) (car b) c-in (car s) c)
        (let ((d (make-wire)))
          (full-adder (car a) (car b) d (car s) c)
          (iter (cdr a) (cdr b) (cdr s) d)))))
