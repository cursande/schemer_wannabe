;; Exercise 3.35. Ben Bitdiddle tells Louis that one way to avoid the trouble in exercise 3.34 is to define
;; a squarer as a new primitive constraint. Fill in the missing portions in Ben’s outline for a procedure to
;; implement such a constraint:

;; (define (squarer a b)
;;   (define (process-new-value)
;;     (if (has-value? b)
;;         (if (< (get-value b) 0)
;;             (error "square less than 0 -- SQUARER" (get-value b))
;;             <alternative1>)
;;         <alternative2>))
;;   (define (process-forget-value) <body1>)
;;   (define (me request) <body2>)
;;   <rest of definition>
;;   me)

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

(define (inform-about-value constraint)
  (constraint 'i-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'i-lost-my-value))

(define (constant value connector)
  (define (me request)
    (error "Unknown request - CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (make-connector)
  (let ((value false)
        (informant false)
        (constraints '()))
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


;; ===========================

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (set-value! b
                    (* (get-value a)
                       (get-value a))
                    me)))
  (define (process-forget-value)
    (forget-value! a)
    (forget-value! b)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'i-have-a-value)
           (process-new-value))
          ((eq? request 'i-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request - SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(define A (make-connector))
(define B (make-connector))

(probe "value A" A)
(probe "value B" B)

(squarer A B)

(set-value! A 5 'user)
;; Probe: value B = 25
;; Probe: value A = 5
;Value: done

(define C (make-connector))
(define D (make-connector))

(probe "value C" C)
(probe "value D" D)

(squarer C D)

(set-value! D 81 'user)
;; Probe: value C = 9
;; Probe: value D = 81
;Value: done
