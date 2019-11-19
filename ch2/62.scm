;; Exercise 2.62. Give a O(n) implementation of union-set for sets represented as ordered lists.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
       (let ((x1 (car set1)) (x2 (car set2)))
         (cond ((= x1 x2)
                (cons x1
                      (intersection-set (cdr set1)
                                        (cdr set2))))
               ((< x1 x2)
                (intersection-set (cdr set1) set2))
               ((< x2 x1)
                (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (union-set set1 set2)
  (if (null? set2)
      set1
      (let ((next (car set2)))
        (if (element-of-set? next set1)
            (union-set set1 (cdr set2))
            (union-set (cons next set1) (cdr set2))))))

(define (union-set set1 set2)
  (cond ((and (null? set2) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((next1 (car set1))
                    (next2 (car set2)))
                (cond ((< next1 next2) (cons next1 (union-set (cdr set1) set2)))
                      ((> next1 next2) (cons next2 (union-set set1 (cdr set2))))
                      (else (cons next1 (union-set (cdr set1) (cdr set2)))))))))

(define test-set-1 (list 4 5 11 12))
(define test-set-2 (list 2 5 10 12))

(union-set test-set-1 test-set-2) ; => (2 4 5 10 11 12)
