;; Exercise 3.18. Write a procedure that examines a list and determines whether it contains a
;; cycle, that is, whether a program that tried to find the end of the list by taking successive cdr s
;; would go into an infinite loop. Exercise 3.13 constructed such lists.

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define infinite-list (make-cycle '(1 2 3)))

(define (contains? x pairs)
  (cond ((null? pairs) false)
        ((eq? x (car pairs)) true)
        (else (contains? x (cdr pairs)))))

(define (cycle? x)
  (let ((previous (list)))
    (define (loop x)
      (cond ((null? x) false)
            ((contains? x previous) true)
            (else
              (set! previous (cons x previous))
              (loop (cdr x)))))
    (loop x)))

(cycle? infinite-list) ; => true
