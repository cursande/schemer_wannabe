;; Exercise 2.27. Modify your reverse procedure of exercise 2.18 to produce a deep-reverse
;; procedure that takes a list as argument and returns as its value the list with its elements reversed and
;; with all sublists deep-reversed as well. For example,

;; (define x (list (list 1 2) (list 3 4)))

;; x
;; ((1 2) (3 4))
;; (reverse x)
;; ((3 4) (1 2))
;; (deep-reverse x)
;; ((4 3) (2 1))

(define (reverse list)
  (define (reverse-iter list rev-list)
    (if (null? list)
        rev-list
        (reverse-iter (cdr list)
                      (cons (car list) rev-list))))
  (reverse-iter list '()))

(define (deep-reverse list)
  (define (reverse-iter list rev-list)
    (cond ((null? list)
           rev-list)
          ((pair? (car list))
           (reverse-iter (cdr list) (cons (reverse-iter (car list) '()) rev-list)))
          (else (reverse-iter (cdr list)
                              (cons (car list) rev-list)))))
  (reverse-iter list '()))

;; -------
(define x (list (list 1 2) (list 3 4)))

(reverse x) ; => ((3 4) (1 2))
(deep-reverse x) ; => ((4 3) (2 1))