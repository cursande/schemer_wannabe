;; Exercise 2.35. Redefine count-leaves from section 2.2.2 as an accumulation:
;; (define (count-leaves t)
;;   (accumulate <??> <??> (map <??> <??>)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; -------------------------------------

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (tree) (if (not (pair? tree))
                                      1
                                      (count-leaves tree)))
                   t)))

(count-leaves (list (list 1 2) (list 3 4))) ; => 4
(count-leaves (list 1 4 5 (list 3 4) (list 1 5 (list 3 (list 4 3))))) ; => 10
