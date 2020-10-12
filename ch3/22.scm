;; Exercise 3.22. Instead of representing a queue as a pair of pointers, we can build a queue as a
;; procedure with local state. The local state will consist of pointers to the beginning and the end of an
;; ordinary list. Thus, the make-queue procedure will have the form

;; (define (make-queue)
;;   (let ((front-ptr ...)
;;         (rear-ptr ...))
;;     <definitions of internal procedures>
;;     (define (dispatch m) ...)
;;     dispatch))

;; Complete the definition of make-queue and provide implementations of the queue operations using
;; this representation.

(define (make-queue)
  (let ((front-ptr (list))
        (rear-ptr (list)))
    (define (queue-struct) (cons front-ptr rear-ptr))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty?) (null? front-ptr))
    (define (front)
      (if (empty?)
          (error "FRONT called with an empty queue" (queue-struct))
          (car front-ptr)))
    (define (print) (display front-ptr))
    (define (insert! item)
      (let ((new-pair (cons item '())))
        (cond ((empty?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               (queue-struct))
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               (queue-struct)))))
    (define (delete!)
      (cond ((empty?)
             (error "DELETE! called with an empty queue" (queue-struct)))
            (else
             (set-front-ptr! (cdr front-ptr))
             (queue-struct))))
    (define (dispatch m)
      (cond ((eq? m 'front) front)
            ((eq? m 'empty?) empty?)
            ((eq? m 'print) print)
            ((eq? m 'insert!) insert!)
            ((eq? m 'delete!) delete!)
            (else (error "Uknown operation -- MAKE-QUEUE"
                         m))))
    dispatch))

(define q1 (make-queue))

((q1 'insert!) 'a) ; => ((a) a)
((q1 'insert!) 'b) ; => ((a b) b)

((q1 'front)) ; => a
((q1 'print)) ; => (a b)

((q1 'delete!)) ; => ((b) b)
