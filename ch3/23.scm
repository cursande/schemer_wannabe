;; Exercise 3.23. A deque (‘‘double-ended queue’’) is a sequence in which items can be inserted and
;; deleted at either the front or the rear. Operations on deques are the constructor make-deque, the
;; predicate empty-deque?, selectors front-deque and rear-deque, and mutators
;; front-insert-deque!, rear-insert-deque!, front-delete-deque!, and
;; rear-delete-deque!. Show how to represent deques using pairs, and give implementations of
;; the operations. 23 All operations should be accomplished in (1) steps.

(define (make-deque)
  (let ((front-ptr (list))
        (rear-ptr (list)))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-deque?) (null? front-ptr))
    (define (front-insert! item)
      (let ((new-pair (cons (cons item '()) front-ptr)))
        (cond ((empty-deque?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (map car front-ptr))
              (else
               (set-cdr! (car front-ptr) new-pair)
               (set-front-ptr! new-pair)
               (map car front-ptr)))))
    (define (rear-insert! item)
      (let ((new-pair (cons (cons item rear-ptr) '())))
        (cond ((empty-deque?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (map car front-ptr))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               (map car front-ptr)))))
    (define (front-delete!)
      (cond ((empty-deque?)
             (error "FRONT-DELETE! called on empty queue" front-ptr))
            (else
             (set-front-ptr! (cdr front-ptr))
             (if (empty-deque?)
                 (map car front-ptr)
                 (begin (set-cdr! (car front-ptr) '())
                        (map car front-ptr))))))
    (define (rear-delete!)
      (cond ((empty-deque?)
             (error "REAR-DELETE! called on empty queue" front))
            (else
             (set-rear-ptr! (cdar rear-ptr))
             (if (null? rear-ptr)
                 (set-front-ptr! '())
                 (set-cdr! rear-ptr '()))
             (map car front-ptr))))
    (define (print) (display (map car front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'front-insert!) front-insert!)
            ((eq? m 'rear-insert!)  rear-insert!)
            ((eq? m 'front-delete!) front-delete!)
            ((eq? m 'rear-delete!)  rear-delete!)
            ((eq? m 'print)         print)
            (else (error "Uknown operation -- MAKE-QUEUE" m))))
    dispatch))

(define dq1 (make-deque))

((dq1 'rear-insert!) 'a) ; => (a)
((dq1 'front-insert!) 'b) ; => (b a)
((dq1 'rear-insert!) 'b) ; => (b a b)

((dq1 'front-delete!)) ; => (a b)
((dq1 'rear-delete!)) ; => (a)
