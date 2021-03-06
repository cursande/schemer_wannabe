;; Exercise 2.33. Fill in the missing expressions to complete the following definitions of some basic
;; list-manipulation operations as accumulations:

;; (define (map p sequence)
;;   (accumulate (lambda (x y) <??>) nil sequence))

;; (define (append seq1 seq2)
;;   (accumulate cons <??> <??>))

;; (define (length sequence)
;;   (accumulate <??> 0 sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              '()
              sequence))

(define (cube x) (* x x x))

(map cube '(1 2 3)) ; => (1 8 27)

;; ----------------

(define (append seq1 seq2)
  (accumulate cons
              seq1
              seq2))

(append '(1 2) '(3 4)) ; => (3 4 1 2)

;; ----------------

(define (length sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
              0
              sequence))

(length '(5 6 9 1 5 6)) ; => 6
