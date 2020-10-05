;; Exercise 3.17. Devise a correct version of the count-pairs procedure of exercise 3.16 that
;; returns the number of distinct pairs in any structure. (Hint: Traverse the structure, maintaining an
;; auxiliary data structure that is used to keep track of which pairs have already been counted.)

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define (contains? x pairs)
  (cond ((null? pairs) false)
        ((eq? x (car pairs)) true)
        (else (contains? x (cdr pairs)))))

(define (count-distinct-pairs x)
  (let ((tracked (list)))
    (define (loop v)
      (if (not (pair? v))
          0
          (if (not (contains? v tracked))
              (begin (set! tracked (append tracked (list v)))
                     (+ (loop (car v))
                        (loop (cdr v))
                        1))
              (+ (loop (car v))
                 (loop (cdr v))))))
    (loop x)))

(define x (cons 'a '()))
(define y (cons 'a x))
(define z (cons x x))

(count-distinct-pairs (list 'a 'b 'c)) ; => 3
(count-distinct-pairs (cons x y)) ; => 3
(count-distinct-pairs (cons z z)) ; => 3
