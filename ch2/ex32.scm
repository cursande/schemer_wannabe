;; Exercise 2.32. We can represent a set as a list of distinct elements, and we can
;; represent the set of all subsets of the set as a list of lists.

;; For example, if the set is (1 2 3), then the set of all subsets is
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)).

;; Complete the following definition of a procedure that generates the set of subsets
;; of a set and give a clear explanation of why it works:

;; (define (subsets s)
;;   (if (null? s)
;;       (list nil)
;;       (let ((rest (subsets (cdr s))))
;;         (append rest (map <??> rest)))))

(define (subsets set)
  (if (null? set)
      (list '())
      (let ((rest (subsets (cdr set))))
        (append rest (map (lambda (subset)
                            (append (list (car set)) subset))
                          rest)))))

(define set (list 1 2 3))

(subsets set) ; => (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;; Why it works:

(define (subsets set)
  (if (null? set)
      (list '())
      (let ((rest (subsets (cdr set))))
        (append rest (map (lambda (subset)
                            (newline)
                            (display "(car set) = ")
                            (display (car set))
                            (display " / subset = ")
                            (display subset)
                            (append (list (car set)) subset))
                          rest)))))

(subsets set)
;; (car set) = 3 / subset = ()
;; (car set) = 2 / subset = ()
;; (car set) = 2 / subset = (3)
;; (car set) = 1 / subset = ()
;; (car set) = 1 / subset = (3)
;; (car set) = 1 / subset = (2)
;; (car set) = 1 / subset = (2 3)

;; The first call to subsets appears down the bottom.
;; Starting from the end of our original set passed in, we work through
;; our list of distinct elements and add them to each new subset created
;; by appending to rest above. By cdring down through the original set we
;; eventually will have an empty set, returning the fully appended list.
