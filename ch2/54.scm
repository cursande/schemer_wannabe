;; Exercise 2.54. Two lists are said to be equal? if they contain equal elements arranged in the same
;; order. For example,

;; (equal? ’(this is a list) ’(this is a list))

;; is true, but:

;; (equal? ’(this is a list) ’(this (is a) list))

;; is false.

;; To be more precise, we can define equal? recursively in terms of the basic eq? equality of
;; symbols by saying that a and b are equal? if they are both symbols and the symbols are eq?, or
;; if they are both lists such that (car a) is equal? to (car b) and (cdr a) is equal? to
;; (cdr b). Using this idea, implement equal? as a procedure.

;; - if they are both symbols:
;;   - and the symbols are eq?
;; - if they are both lists such that:
;;   - (car a) is equal? to (car b)
;;   - (cdr a) is equal? to (cdr b)

(define (both-symbols? item1 item2)
  (and (not (pair? item1)) (not (pair? item2))))

(define (both-lists? item1 item2)
  (and (pair? item1) (pair? item2)))

(define (equal? item1 item2)
  (cond ((both-symbols? item1 item2)
         (eq? item1 item2))
        ((both-lists? item1 item2)
         (and (equal? (car item1) (car item2))
              (equal? (cdr item1) (cdr item2))))
        (else #f)))

(equal? '(this is a list) '(this is a list)) ; => #t
(equal? '(this is a list) '(this (is a) list)) ; => #f
(equal? '(this (is a) list) '(this (is a) list)) ; => #t
(equal? 'is 'unequal) ; => #f
(equal? 'equal 'equal) ; => #t
