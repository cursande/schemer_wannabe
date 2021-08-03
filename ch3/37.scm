;; Exercise 3.37. The celsius-fahrenheit-converter procedure is cumbersome when
;; compared with a more expression-oriented style of definition, such as

;; (define (celsius-fahrenheit-converter x)
;;   (c+ (c* (c/ (cv 9) (cv 5))
;;           x)
;;       (cv 32)))
;; (define C (make-connector))
;; (define F (celsius-fahrenheit-converter C))

;; Here c+, c*, etc. are the ‘‘constraint’’ versions of the arithmetic operations. For example, c+ takes
;; two connectors as arguments and returns a connector that is related to these by an adder constraint:

;; (define (c+ x y)
;;   (let ((z (make-connector)))
;;     (adder x y z)
;;     z))

;; Define analogous procedures c-, c*, c/, and cv (constant value) that enable us to define compound
;; constraints as in the converter example above.

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (constant value connector)
  (define (me request)
    (error "Unknown request - CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (inform-about-value constraint)
  (constraint 'i-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'i-lost-my-value))

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request - ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (subtractor s1 s2 difference)
  (define (process-new-value)
    (cond ((and (has-value? s1) (has-value? s2))
           (set-value! difference
                       (- (get-value s1) (get-value s2))
                       me))
          ((and (has-value? s1) (has-value? difference))
           (set-value! s2
                       (+ (get-value difference) (get-value s1))
                       me))
          ((and (has-value? s2) (has-value? difference))
           (set-value! s1
                       (+ (get-value difference) (get-value s2))
                       me))))
  (define (process-forget-value)
    (forget-value! difference me)
    (forget-value! s1 me)
    (forget-value! s2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request - SUBTRACTOR" request))))
  (connect s1 me)
  (connect s2 me)
  (connect difference me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product)
                          (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product)
                          (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request - MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (divider d1 d2 quotient)
  (define (process-new-value)
    (cond ((or (and (has-value? d1) (= (get-value d1) 0))
               (and (has-value? d2) (= (get-value d2) 0)))
           (set-value! quotient 0 me))
          ((and (has-value? d1) (has-value? d2))
           (set-value! quotient
                       (/ (get-value d1) (get-value d2))
                       me))
          ((and (has-value? quotient) (has-value? d1))
           (set-value! d2
                       (* (get-value quotient)
                          (get-value d1))
                       me))
          ((and (has-value? quotient) (has-value? d2))
           (set-value! d1
                       (* (get-value quotient)
                          (get-value d2))
                       me))))
  (define (process-forget-value)
    (forget-value! quotient me)
    (forget-value! d1 me)
    (forget-value! d2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request - DIVIDER" request))))
  (connect d1 me)
  (connect d2 me)
  (connect quotient me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

;; ==========================

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (subtractor x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (divider x y z)
    z))

(define (cv val)
  (let ((z (make-connector)))
    (constant val z)
    z))

;; =============================

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe "value C" C)
(probe "value F" F)

(set-value! C 50 'user)
;; Probe: value C = 50
;; Probe: value F = 122
;Value: done
