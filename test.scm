
;	 { n     if n < 3 }
; f(n) = 
;        { f(n - 1) + 2f(n - 2) + 3f(n - 3)     if n >= 3) }


(define (f n)
    (cond ((< n 3) n)
	 (else (+ (f (- n 1))
	          (* 2 (f (- n 2)))
	          (* 3 (f (- n 3)))))))

;--------------

define (f n)
  (f-iter n 0 1 2))

(define (f-iter i x y z) 
   (cond ((= i 0) x) 
	 (else (f-iter (- i 1) y z (+ z (* 2 y) (* 3 x)))))) 


