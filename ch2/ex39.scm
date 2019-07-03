;; Exercise 2.39. Complete the following definitions of reverse (exercise 2.18) in terms of
;; fold-right and fold-left from exercise 2.38:
;;
;; (define (reverse sequence)
;; (fold-right (lambda (x y) <??>) nil sequence))
;; (define (reverse sequence)
;; (fold-left (lambda (x y) <??>) nil sequence))

(define (right-reverse sequence)
  (fold-right (lambda (x y) (append y (list x)))
              '()
              sequence))

(right-reverse (list 1 2 3)) ; => (3 2 1)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (left-reverse sequence)
  (fold-left (lambda (x y) (cons y x))
             '()
             sequence))

(left-reverse (list 1 2 3)) ; => (3 2 1)
