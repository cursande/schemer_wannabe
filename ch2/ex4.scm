;; Exercise 2.4. Here is an alternative procedural representation of pairs.
;; For this representation, verify that (car (cons x y)) yields x for any objects x and y.
;; What is the corresponding definition of cdr?
;; (Hint: To verify that this works, make use of the substitution model of section 1.1.5.)
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(car (cons 1 2)) ; => 1

(car (cons "shoe" "pig")) ; => "shoe"

(define square (car (cons (lambda (x) (* x x))
                          (lambda (x) (* x x x)))))

(square 4) ; => 16

;--------

(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons 1 2)) ; => 2

(define (three a b c)
  (lambda (m) (m a b c)))

(define (first z)
  (z (lambda (a b c) a)))

(first (three 3 6 8))
