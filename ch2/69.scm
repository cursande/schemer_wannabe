;; Exercise 2.69. The following procedure takes as its argument a list of symbol-frequency pairs (where
;; no symbol appears in more than one pair) and generates a Huffman encoding tree according to the
;; Huffman algorithm.

;; (define (generate-huffman-tree pairs)
;;   (successive-merge (make-leaf-set pairs)))

;; Make-leaf-set is the procedure given above that transforms the list of pairs into an ordered set of
;; leaves. Successive-merge is the procedure you must write, using make-code-tree to
;; successively merge the smallest-weight elements of the set until there is only one element left, which
;; is the desired Huffman tree. (This procedure is slightly tricky, but not really complicated. If you find
;; yourself designing a complex procedure, then you are almost certainly doing something wrong. You
;; can take significant advantage of the fact that we are using an ordered set representation.)

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

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (let ((smallest (car leaf-set))
            (next-smallest (cadr leaf-set))
            (remaining-elts (cddr leaf-set)))
        (successive-merge (adjoin-set (make-code-tree smallest next-smallest)
                                      remaining-elts)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define sample-leaf-set (make-leaf-set '((a 5) (e 1) (c 2) (b 4) (d 2)))) ; => ((leaf e 1) (leaf d 2) (leaf c 2) (leaf b 4) (leaf a 5))

(successive-merge sample-leaf-set) ; => (((leaf c 2) ((leaf e 1) (leaf d 2) (e d) 3) (c e d) 5) ((leaf b 4) (leaf a 5) (b a) 9) (c e d b a) 14)
