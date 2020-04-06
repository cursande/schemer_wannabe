;; section 2.3.2 described a program that performs symbolic differentiation:

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum
;;           (make-product (multiplier exp)
;;                         (deriv (multiplicand exp) var))
;;           (make-product (deriv (multiplier exp) var)
;;                         (multiplicand exp))))
;;         ;<more rules can be added here>
;;         (else (error "unknown expression type -- DERIV" exp))))

;; We can regard this program as performing a dispatch on the type of the expression to be differentiated.
;; In this situation the ‘‘type tag’’ of the datum is the algebraic operator symbol (such as +) and the
;; operation being performed is deriv. We can transform this program into data-directed style by
;; rewriting the basic derivative procedure as:

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp) (if (same-variable? exp var) 1 0))
;;         (else ((get ’deriv (operator exp)) (operands exp)
;;                var))))
;; (define (operator exp) (car exp))
;; (define (operands exp) (cdr exp))

;; a. Explain what was done above. Why can’t we assimilate the predicates number? and
;; same-variable? into the data-directed dispatch?

;; We can't assimilate those two procedures because they don't give enough information to dispatch on. Our table needs
;; an 'op' to actually perform, and if we have just a number or variable, there's no operation/'deriv provided.

;; b. Write the procedures for derivatives of sums and products, and the auxiliary code required to install
;; them in the table used by the program above.

;; Without implementing 'get' and 'put', below is totally theoretical and untested:

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (install-sum-package)
  (define (addend operands) (cadr operands))

  (define (augend operands) (caddr operands))

  (define (deriv operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (addend operands) var)))

  (put 'deriv '+ deriv)
  'done)

(define (install-product-package)
  (define (multiplier operands) (cadr operands))

  (define (multiplicand operands) (caddr operands))

  (define (deriv operands var)
    (make-sum
     (make-product (multiplier operands)
                   (deriv (multiplicand operands) var))
     (make-product (deriv (multiplier operands) var)
                   (multiplicand operands))))

  (put 'deriv '* deriv)
  'done)

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
               var))))

;; c. Choose any additional differentiation rule that you like, such as the one for exponents (exercise 2.56),
;; and install it in this data-directed system.

(define (install-exponentiation-package)
  (define (base operands) (cadr operands))

  (define (exponent operands) (caddr operands))

  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((and (number? base) (number? exponent))
           (expt base exponent))
          (else (list '** base exponent))))

  (define (deriv operands var)
    (make-product (exponent operands)
                  (make-exponentiation (base operands)
                                       (make-sum (exponent operands) -1))))

  (put 'deriv '** deriv)
  'done)

;; d. In this simple algebraic manipulator the type of an expression is the algebraic operator that binds it
;; together. Suppose, however, we indexed the procedures in the opposite way, so that the dispatch line
;; in deriv looked like:

;; ((get (operator exp) ’deriv) (operands exp) var)

;; What corresponding changes to the derivative system are required?

;; If they're indexed with the operator/<type> first, We'd need to change the way we 'put' so that the <type> comes before the <op>
