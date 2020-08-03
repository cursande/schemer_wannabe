;; Exercise 3.9. In section 1.2.1 we used the substitution model to analyze two procedures for
;; computing factorials, a recursive version:

;; (define (factorial n)
;;   (if (= n 1)
;;       1
;;       (* n (factorial (- n 1)))))

;; and an iterative version:

;; (define (factorial n)
;;   (fact-iter 1 1 n))

;; (define (fact-iter product counter max-count)
;;   (if (> counter max-count)
;;       product
;;       (fact-iter (* counter product)
;;                  (+ counter 1)
;;                  max-count)))

;; Show the environment structures created by evaluating (factorial 6) using each version of the
;; factorial procedure.

;; For recursive definition, the environments created are:
;; (factorial n)
;; [n: 6]
;; [n: 5]
;; [n: 4]
;; [n: 3]
;; [n: 2]
;; [n: 1]
;; -> 720

;; For iterative definition:
;; (factorial n)
;; [n: 6]
;; (factorial-iter product counter max-count)
;; [product:   1
;;  counter:   1
;;  max-count: 6] ->
;; [product:   1
;;  counter:   2
;;  max-count: 6] ->
;; [product:   2
;;  counter:   3
;;  max-count: 6] ->
;; [product:   6
;;  counter:   4
;;  max-count: 6] ->
;; [product:   24
;;  counter:   5
;;  max-count: 6] ->
;; [product:   120
;;  counter:   6
;;  max-count: 6] ->
;; [product:   720
;;  counter:   7
;;  max-count: 6] ->
;; 720
