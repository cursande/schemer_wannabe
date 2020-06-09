;; Exercise 3.4. Modify the make-account procedure of exercise 3.3 by adding another local state
;; variable so that, if an account is accessed more than seven consecutive times with an incorrect
;; password, it invokes the procedure call-the-cops.

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define fail-attempts 0)
  (define (call-the-cops amount)
    "Call the cops")
  (define (incorrect-password amount)
    (set! fail-attempts (+ fail-attempts 1))
    "Incorrect password")
  (define (dispatch pass-attempt m)
    (if (eq? pass-attempt password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (if (>= fail-attempts 7)
            call-the-cops
            incorrect-password)))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'pusword 'withdraw) 100) ; => "Incorrect password"
;; etc...
((acc 'pusword 'withdraw) 100) ; => "Call the cops"
