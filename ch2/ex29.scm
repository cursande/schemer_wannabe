;; Exercise 2.29. A binary mobile consists of two branches, a left branch and a right branch. Each
;; branch is a rod of a certain length, from which hangs either a weight or another binary mobile. We can
;; represent a binary mobile using compound data by constructing it from two branches (for example,
;; using list):

(define (make-mobile left right)
  (list left right))

;; A branch is constructed from a length (which must be a number) together with a structure,
;; which may be either a number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

;; a. Write the corresponding selectors left-branch and right-branch, which return the
;; branches of a mobile, and branch-length and branch-structure, which return the
;; components of a branch.

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))

(define branch-1 (make-branch 5 4))
(define branch-2 (make-branch 9 (make-mobile (make-branch 3 5)
                                             (make-branch 5 2))))

(define mobile-1 (make-mobile branch-1 branch-2))

(left-branch mobile-1) ; => (5 4)
(right-branch mobile-1) ; => (9 ((3 5) (5 2)))

(branch-length branch-1) ; => 5
(branch-structure branch-1) ; => 4

;; b. Using your selectors, define a procedure total-weight that returns the total weight of a mobile.

(define (total-weight mobile)
  (define (branch-weight branch)
    (if (pair? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)))
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; weight of mobile-1 => 4 + 5 + 2 = 11
(total-weight mobile-1) ; => 11

;; c. A mobile is said to be balanced if the torque applied by its top-left branch is equal to that applied
;; by its top-right branch (that is, if the length of the left rod multiplied by the weight hanging from that
;; rod is equal to the corresponding product for the right side) and if each of the submobiles hanging off
;; its branches is balanced. Design a predicate that tests whether a binary mobile is balanced.


(define (mobile-balanced? mobile)
  (define (torque length weight) (* length weight))
  (define (branch-torque branch)
    (torque (branch-length branch)
            (if (pair? (branch-structure branch))
                (total-weight (branch-structure branch))
                (branch-structure branch))))
  (= (branch-torque (left-branch mobile))
     (branch-torque (right-branch mobile))))

(mobile-balanced? mobile-1) ; => #f

(define branch-3 (make-branch 8 (make-mobile (make-branch 4 4)
                                             (make-branch 7 7))))

(define mobile-2 (make-mobile branch-3 branch-3))

(mobile-balanced? mobile-2) ; => #t

;; d. Suppose we change the representation of mobiles so that the constructors are

;; (define (make-mobile-left-right left right) (cons left right))
;; (define (make-branch length structure) (cons length structure))

;; How much do you need to change your programs to convert to the new representation?

(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))

(define branch-4 (make-branch 8 7))
(define branch-5 (make-branch 8 9))
(define mobile-3 (make-mobile branch-4 branch-5))

(define (right-branch mobile) (car (cdr mobile)))
(define (branch-structure branch) (car (cdr branch)))

(right-branch mobile-3) ; => 8
(branch-structure branch-4) ; => Error

(define (right-branch mobile) (cdr mobile))
(define (branch-structure branch) (cdr branch))

(right-branch mobile-3) ; => (8 . 9)
(branch-structure branch-4) ; => 7
