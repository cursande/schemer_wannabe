;; Exercise 2.72. Consider the encoding procedure that you designed in exercise 2.68. What is the order
;; of growth in the number of steps needed to encode a symbol? Be sure to include the number of steps
;; needed to search the symbol list at each node encountered.

;; To answer this question in general is difficult. Consider the special case where the relative frequencies
;; of the n symbols are as described in exercise 2.71, and give the order of growth (as a function of n)
;; of the number of steps needed to encode the most frequent and least frequent symbols in the alphabet.

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (cond ((symbol-present? symbol left)
               (cons 0 (encode-symbol symbol left)))
              ((symbol-present? symbol right)
               (cons 1 (encode-symbol symbol right)))
              (else (error "symbol not found!"))))))

;; In general, the number of steps should depend on the structure of the tree. The more balanced the tree is, the less steps to reach a given symbol.

;; If though relative frequency is 2^n-1, and for instance we have 5 symbols in our tree:
;; For each bit, we have to call symbol-present? at least once. For the symbol with the least weight (encoded as 1111),
;; The number of steps needed to search the tree to encode the symbol is as follows:
;; - At the first node, our left branch contains just the highest weight symbol. We cons 1 on and call encode-symbol again
;; - This continues 4 times, with the highest weight symbol on the left branch, until we've reached the leaf of the symbol
;;   with the least weight and can return the final value.
;;
;; So with n = 5, we end up having to call encode-symbol 4 times, for each '1' we cons on.
;; This pattern would continue, so if n = 10, and we were encoding the symbol with the least weight (111111111)
;; We end up with 9 calls to encode-symbol after the initial call.
;;
;; So if each call to encode-symbol takes n steps (to check which branch the symbol is in), and we have to recursively call encode-symbol n times,
;; it's n * (n - 1) steps to encode the symbol with the least weight. Though each new call to encode-symbol will be taking a sub-tree of the previous.
