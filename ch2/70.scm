;; Exercise 2.70. The following eight-symbol alphabet with associated relative frequencies was
;; designed to efficiently encode the lyrics of 1950s rock songs. (Note that the ‘‘symbols’’ of an
;; ‘‘alphabet’’ need not be individual letters.)

;; A    2
;; NA   16
;; BOOM 1
;; SHA  3
;; GET  2
;; YIP  9
;; JOB  2
;; WAH  1

;; Use generate-huffman-tree (exercise 2.69) to generate a corresponding Huffman tree, and use
;; encode (exercise 2.68) to encode the following message:

;; Get a job
;; Sha na na na na na na na na
;; Get a job
;; Sha na na na na na na na na
;; Wah yip yip yip yip yip yip yip yip yip
;; Sha boom

;; How many bits are required for the encoding? What is the smallest number of bits that would be
;; needed to encode this song if we used a fixed-length code for the eight-symbol alphabet?

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

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

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

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

(define 50s-rock-pairs '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

(define 50s-rock-huffman (generate-huffman-tree 50s-rock-pairs))

(define lyrics '(Get a job
                     Sha na na na na na na na na
                     Get a job
                     Sha na na na na na na na na
                     Wah yip yip yip yip yip yip yip yip yip
                     Sha boom))

(encode lyrics 50s-rock-huffman) ; => (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1)

(length (encode lyrics 50s-rock-huffman)) ; => 84

;; The message above can be encoded in 84 bits.

;; For fixed-length encoding, we are using eight different symbols, and so will need at least 3 bits per symbol to
;; encode it. There are 36 symbols in lyrics, which gives us 36 * 3 = 108 bits required to encode the message.
