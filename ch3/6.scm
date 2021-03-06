;; Exercise 3.6. It is useful to be able to reset a random-number generator to produce a sequence starting
;; from a given value. Design a new rand procedure that is called with an argument that is either the
;; symbol generate or the symbol reset and behaves as follows: (rand ’generate) produces a
;; new random number; ((rand ’reset) <new-value>) resets the internal state variable to the
;; designated <new-value>. Thus, by resetting the state, one can generate repeatable sequences. These are
;; very handy to have when testing and debugging programs that use random numbers.

(define (rand-update seed)
  (let ((multiplier 987)
        (increment 456)
        (modulus 123))
    (modulo (+ (* multiplier seed) increment) modulus)))

(define rand
  (let ((x 0))
    (define (generate)
      (begin (set! x (rand-update x))
             x))
    (define (reset new-value)
      (begin (set! x new-value)
             new-value))
    (lambda (sym)
      (cond ((eq? sym 'generate) (generate))
            ((eq? sym 'reset) reset)
            (else (error "Unknown symbol -- RAND"
                         sym))))))


(rand 'generate) ; => 87
(rand 'generate) ; => 102
(rand 'generate) ; => 24

((rand 'reset) 102) ; => 102
(rand 'generate) ; => 24
