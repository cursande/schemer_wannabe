;; Exercise 2.59. Implement the union-set operation for the unordered-list representation of sets.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (if (null? set2)
      set1
      (let ((next (car set2)))
        (if (element-of-set? next set1)
            (union-set set1 (cdr set2))
            (union-set (cons next set1) (cdr set2))))))

(define test-set-1 (list 1 3 2 4))
(define test-set-2 (list 3 2 5 7))

(union-set test-set-1 test-set-2) ; => (7 5 1 3 2 4)
