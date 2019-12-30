;; Exercise 2.64. The following procedure list->tree converts an ordered list to a balanced binary
;; tree. The helper procedure partial-tree takes as arguments an integer n and list of at least n
;; elements and constructs a balanced tree containing the first n elements of the list. The result returned
;; by partial-tree is a pair (formed with cons) whose car is the constructed tree and whose cdr
;; is the list of elements not included in the tree.

;; (define (list->tree elements)
;;   (car (partial-tree elements (length elements))))
;; (define (partial-tree elts n)
;;   (if (= n 0)
;;       (cons '() elts)
;;       (let ((left-size (quotient (- n 1) 2)))
;;         (let ((left-result (partial-tree elts left-size)))
;;           (let ((left-tree (car left-result))
;;                 (non-left-elts (cdr left-result))
;;                 (right-size (- n (+ left-size 1))))
;;             (let ((this-entry (car non-left-elts))
;;                   (right-result (partial-tree (cdr non-left-elts)
;;                                               right-size)))
;;               (let ((right-tree (car right-result))
;;                     (remaining-elts (cdr right-result)))
;;                 (cons (make-tree this-entry left-tree right-tree)
;;                       remaining-elts))))))))

;; a. Write a short paragraph explaining as clearly as you can how partial-tree works. Draw the
;; tree produced by list->tree for the list (1 3 5 7 9 11).

(define (make-tree entry left right) (list entry left right))

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

(list->tree (list 1 3 5 7 9 11)) ; => (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

;; partial-tree breaks down the (ordered) list into a series of subtrees by recursing down the 'left' and 'right'
;; paths (left-result and right-result). Every time partial-tree returns, it gives back the current tree, and also the remaining
;; elements as the cdr. At each subtree, it takes half the total number of elements as the
;; size of the left branch, and conses each element to that tree, plucks the first of the remaining elements to be used as the middle
;; value, and then stores the remaining elements in the right-result. Every node in the binary tree is created with `this-entry`
;; being the node itself (the middle value of the new tree), and its left branch being left-tree and right branch being right-tree.
;; Leaves of the tree are found when n is zero for both left-result and right-result.

;; b. What is the order of growth in the number of steps required by list->tree to convert a list of n
;; elements?

;; Everytime you need to do the half division to derive left-size and therefore left-result (and ultimately
;; make the two calls to partial-tree for each branch) you halve the number of steps to get to n being zero.
;; In the above example, we have 6 elements and end up with 5 divisions (when the n passed to a call to partial-tree
;; is not zero).
;; The order of growth for list->tree is O(n).
