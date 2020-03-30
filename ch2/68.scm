;; Exercise 2.68. The encode procedure takes as arguments a message and a tree and produces the list
;; of bits that gives the encoded message.

;; (define (encode message tree)
;;   (if (null? message)
;;       ’()
;;        (append (encode-symbol (car message) tree)
;;                (encode (cdr message) tree))))

;; Encode-symbol is a procedure, which you must write, that returns the list of bits that encodes a
;; given symbol according to a given tree. You should design encode-symbol so that it signals an
;; error if the symbol is not in the tree at all.

;;Test your procedure by encoding the result you obtained in
;; exercise 2.67 with the sample tree and seeing whether it is the same as the original sample message.

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (symbol-present? symbol tree)
  (let ((symbol-list (symbols tree)))
    (define (check-symbol symbol symbol-list)
      (if (null? symbol-list)
          false
          (if (eq? symbol (car symbol-list))
              true
              (check-symbol symbol (cdr symbol-list)))))
    (check-symbol symbol symbol-list)))

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

(define (encode message tree)
  (if (null? message)
      '()
       (append (encode-symbol (car message) tree)
               (encode (cdr message) tree))))

; ========================

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(A D A B B C A))

(encode sample-message sample-tree) ; => (0 1 1 0 0 1 0 1 0 1 1 1 0)
