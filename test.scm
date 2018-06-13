;; Exercise 1.34. Suppose we define the procedure

;; (define (f g)
;;   (g 2))

;; Then we have

;; (f square)
;; 4
;; (f (lambda (z) (* z (+ z 1))))
;; 6

;; What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.

(f f) ; = The object 2 is not applicable.

; It's trying to call 2 with an argument of 2, obviously 2 is not a procedure so nothing can happen.
