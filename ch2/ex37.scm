;; Exercise 2.37. Fill in the missing expressions in the following procedures for computing the
;; other matrix operations. (The procedure accumulate-n is defined exercise 2.36.)
;;
;; (define (matrix-*-vector m v)
;;   (map <??> m))

;; (define (transpose mat)
;;   (accumulate-n <??> <??> mat))

;; (define (matrix-*-matrix m n)
;;   (let ((cols (transpose n)))
;;     (map <??> m)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car
                                     seqs))
            (accumulate-n op init (map cdr
                                       seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; Should return vector `t', which will be a single list
;; take the dot-product of v with each of the rows `a' of m
(define (matrix-*-vector m v)
  (map (lambda (a) (dot-product a v))
       m))

(define test-matrix (list (list 1 -1 2)
                          (list 0 -3 1)))
(define test-vector (list 2 1 0))

(matrix-*-vector test-matrix test-vector) ; => (1 -3)

(define (transpose mat)
  (accumulate-n cons
                '()
                mat))

(transpose test-matrix) ; => ((1 0) (-1 -3) (2 1))
