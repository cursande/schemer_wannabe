;; Exercise 2.77. Louis Reasoner tries to evaluate the expression (magnitude z) where z is the
;; object shown in figure 2.24. To his surprise, instead of the answer 5 he gets an error message from
;; apply-generic, saying there is no method for the operation magnitude on the types
;; (complex). He shows this interaction to Alyssa P. Hacker, who says ‘‘The problem is that the
;; complex-number selectors were never defined for complex numbers, just for polar and
;; rectangular numbers. All you have to do to make this work is add the following to the complex
;; package:

;; (put 'real-part '(complex) real-part)
;; (put 'imag-part '(complex) imag-part)
;; (put 'magnitude '(complex) magnitude)
;; (put 'angle '(complex) angle)

;; Describe in detail why this works. As an example, trace through all the procedures called in evaluating
;; the expression (magnitude z) where z is the object shown in figure 2.24. In particular, how many
;; times is apply-generic invoked? What procedure is dispatched to in each case?

;; This works because we've now exposed the procedure in the complex package that was previously local only
;; to the two packages it's using internally: the rectangular and polar packages.

;; (define (install-complex-package)
;;   ;; imported procedures from rectangular and polar packages
;;   (define (make-from-real-imag x y)
;;     ((get 'make-from-real-imag 'rectangular) x y))
;;   (define (make-from-mag-ang r a)
;;     ((get 'make-from-mag-ang 'polar) r a))
;;   ;; internal procedures
;;   (define (add-complex z1 z2)
;;     (make-from-real-imag (+ (real-part z1) (real-part z2))
;;                          (+ (imag-part z1) (imag-part z2))))
;;   (define (sub-complex z1 z2)
;;     (make-from-real-imag (- (real-part z1) (real-part z2))
;;                          (- (imag-part z1) (imag-part z2))))
;;   (define (mul-complex z1 z2)
;;     (make-from-mag-ang (* (magnitude z1) (magnitude z2))
;;                        (+ (angle z1) (angle z2))))

;; (define (apply-generic op arg) (arg op))

;; The original structure z is: '(complex '(rectangular (cons 3 4)))

;; And the operation Louis is calling is (apply-generic 'magnitude '(complex '(rectangular (cons 3 4))))

;; How many times is apply-generic invoked and what procedures are dispatched to each time?

;; If we install the package like this:

;; (put 'magnitude '(complex) magnitude)

;; The first time apply-generic is called is calling apply-generic -> 'magnitude in the complex package.
;; When it finds the installed procedure, it will be called again, this time calling apply-generic -> 'magnitude
;; on the internal package (in the case of z, this will be rectangular)
