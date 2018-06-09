; a. The sum procedure is only the simplest of a vast number of similar abstractions that can be
; captured as higher-order procedures. Write an analogous procedure called product that returns
; the product of the values of a function at points over a given range. Show how to define factorial
; in terms of product. Also use product to compute approximations to using the formula...

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

(define (icrement x) (+ x 1))
(define (identity x) x)

(define (factorial n)
  (product identity 1 icrement n)) 

(factorial 3)
(factorial 4)
(factorial 5)

(define (approx-pi x)
  (define (col x) ; for each 'column' in the expression, find the right expression
    (if (even? x)
      (/ (+ x 2) (+ x 1))
      (/ (+ x 1) (+ x 2))))
  (define (increment x) (+ 1 x))
  (* 4 (product col 1.0 increment x)))

(approx-pi 5) ; = 2.9257142857142857
(approx-pi 10) ; = 3.2751010413348065
(approx-pi 20) ; = 3.2137849402931877
(approx-pi 1000) ; = 3.1431607055322552

; hmmm....

; b. If your product procedure generates a recursive process, write one that generates an iterative
; process. If it generates an iterative process, write one that generates a recursive process.

(define (product term a next b)
  (if (> a b)
    0
    (* (term a)
       (product term (next a) next b))))

