;; Exercise 3.29. Another way to construct an or-gate is as a compound digital logic device, built from
;; and-gates and inverters. Define a procedure or-gate that accomplishes this. What is the delay time
;; of the or-gate in terms of and-gate-delay and inverter-delay?

(define (or-gate a b output)
  (let ((not-a (make-wire))
        (not-b (make-wire))
        (c (make-wire)))
    (inverter a not-a)
    (inverter b not-b)
    (and-gate not-a not-b c)
    (inverter c output)
    'ok))

;; Total delay time would be the and-gate delay and then the time of the first and last inverters (x2)
