;; Exercise 2.41. Write a procedure to find all ordered triples of
;; distinct positive integers `i`, `j`, and `k` less than or equal to a given
;; integer `n` that sum to a given integer `s`.

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append
              '()
              (map proc seq)))

(define (ordered-triple-sums k j i s)
  (if (and (= (+ k j i) s)
           (<= k j)
           (<= j i))
      (list (list k j i))
      '()))

(define (all-ordered-triples n s)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                    (flatmap (lambda (k) (ordered-triple-sums k j i s))
                         (enumerate-interval 1 n)))
                  (enumerate-interval 1 n)))
           (enumerate-interval 1 n)))

(all-ordered-triples 5 5) ; => ((1 2 2) (1 1 3))
(all-ordered-triples 8 8) ; => ((2 3 3) (2 2 4) (1 3 4) (1 2 5) (1 1 6))
