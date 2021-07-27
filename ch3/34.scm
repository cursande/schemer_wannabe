;; Exercise 3.34. Louis Reasoner wants to build a squarer, a constraint device with two terminals such
;; that the value of connector b on the second terminal will always be the square of the value a on the
;; first terminal. He proposes the following simple device made from a multiplier:

;; (define (squarer a b)
;;   (multiplier a a b))

;; There is a serious flaw in this idea. Explain.

;; Operations on connectors need to be able to determine new values of connectors based on
;; value updates upstream or downstream. When `a' has a value, we can multiply it to get the product,
;; `b'. The problem is with these clauses:

;; ((and (has-value? product) (has-value? m1))
;;  (set-value! m2
;;              (/ (get-value product)
;;                 (get-value m1))
;;              me))

;; ((and (has-value? product) (has-value? m2))
;;  (set-value! m1
;;              (/ (get-value product)
;;                 (get-value m2))
;;              me))
;;
;; If `a' doesn't have a value, neither `m1` or `m2` have a value. So we can't use `product'
;; to update the value of `m1' and `m2'.
