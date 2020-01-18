;; Exercise 2.65. Use the results of exercises 2.63 and 2.64 to give O(n) implementations of
;; union-set and intersection-set for sets implemented as (balanced) binary trees.

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
  (if (null? set2)
      set1
      (let ((next (car set2)))
        (if (element-of-set? next set1)
            (union-set set1 (cdr set2))
            (union-set (cons next set1) (cdr set2))))))

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

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define test-tree-1 (list->tree (list 1 3 5 7 9 11))) ; => (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))
(define test-tree-2 (list->tree (list 2 4 5 9 11 14))) ; => (5 (2 () (4 () ())) (11 (9 () ()) (14 () ())))

(define (tree-union-set set1 set2)
  (let ((set1-list (tree->list-2 set1))
        (set2-list (tree->list-2 set2)))
    (list->tree (union-set set1-list
                           set2-list))))

(tree-union-set test-tree-1 test-tree-2) ; => ((11 (9 () ()) (14 () ())) (2 () (4 () ())) 5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

(define (tree-intersection-set set1 set2)
  (let ((set1-list (tree->list-2 set1))
        (set2-list (tree->list-2 set2)))
    (list->tree (intersection-set set1-list
                                  set2-list))))

(tree-intersection-set test-tree-1 test-tree-2) ; => (9 (5 () ()) (11 () ()))
