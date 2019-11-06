;; Exercise 2.60. We specified that a set would be represented as a list with no duplicates.

;; Now suppose we allow duplicates. For instance, the set {1,2,3} could be represented as
;; the list (2 3 2 1 3 2 2). Design procedures element-of-set?, adjoin-set, union-set, and
;; intersection-set that operate on this representation.

;; How does the efficiency of each compare with the corresponding procedure for the
;; non-duplicate representation? Are there applications for which you would use
;; this representation in preference to the non-duplicate one?

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;; element-of-set? doesn't need to change at all to support duplicate elements. It will be
;; less efficient, as it may have to walk through a greater number of elements to find the
;; first element in the ste that is equal to x. Not only that, if the element isn't in the set
;; at all, we'll have to walk through more elements anyway for no gain.

(define (adjoin-set x set)
  (cons x set))

;; adjoin-set becomes more efficient because we no longer have to scan potentially the entire
;; list before inserting an element in the set so it's now constant-time.

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; intersection-set is at best equally efficient if there are only unique elements in each
;; set, else it becomes less efficient with every duplicated value in either set,
;; especially set1, as everytime we call element-of-set? we have to walk through the
;; whole of set2 again.

(define (union-set set1 set2)
  (if (null? set2)
      set1
      (let ((next (car set2)))
        (union-set (cons next set1) (cdr set2)))))

;; union-set could be more performant, if there is high cardinality. Otherwise it will become
;; less performant for the same reasons as element-of-set?

(define test-set-1 (list 1 3 3 5))
(define test-set-2 (list 1 2 2 5 7))

(adjoin-set 5 test-set-2) ; => (5 1 2 2 5 7)
(union-set test-set-1 test-set-2) ; => (7 5 2 2 1 1 3 3 5)
