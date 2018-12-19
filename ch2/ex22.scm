;; Exercise 2.22. Louis Reasoner tries to rewrite the first square-list procedure of exercise 2.21 so
;; that it evolves an iterative process:

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things)
;;               (cons (square (car things))
;;                     answer))))
;;   (iter items nil))

;; Unfortunately, defining square-list this way produces the answer list in the reverse order of the
;; one desired. Why?

;; Louis is cons-ing onto the front of the list, so items that were originally on the right are being sequentially appended to the left
;; instead.
