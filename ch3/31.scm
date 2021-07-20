;; Exercise 3.31. The internal procedure accept-action-procedure! defined in make-wire
;; specifies that when a new action procedure is added to a wire, the procedure is immediately run.
;; Explain why this initialization is necessary. In particular, trace through the half-adder example in the
;; paragraphs above and say how the system’s response would differ if we had defined
;; accept-action-procedure! as

;; (define (accept-action-procedure! proc)
;;   (set! action-procedures (cons proc action-procedures)))

;; When we call set-my-signal!, we need to know whether the signal value coming from the
;; newly added proc will change the signal on the wire. All of the wires are have an initial
;; signal-value of 0, so changing accept-action-procedure! to not call the proc would mean none
;; of our gates will work anymore as the signals won't be updated.
