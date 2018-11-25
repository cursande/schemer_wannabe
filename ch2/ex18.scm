;; Exercise 2.18. Define a procedure reverse that takes a list as argument and returns a list of the
;; same elements in reverse order:

(define (reverse list)
  (define (reverse-iter list rev-list)
    (display rev-list)
    (if (null? list)
        rev-list
        (reverse-iter (cdr list)
                      (cons (car list) rev-list))))
  (reverse-iter list '()))

(reverse (list 1 4 9 16 25)) ; => (25 16 9 4 1)
