;; Exercise 2.25. Give combinations of cars and cdrs that will pick 7 from each of the following lists:

(define list-one '(1 3 (5 7) 9))

(define list-two '((7)))

(define list-three '(1 (2 (3 (4 (5 (6 7)))))))

(car (cdr (car (cdr (cdr list-one))))) ; => 7
(car (car list-two)) ; => 7
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr list-three)))))))))))) ; => 7
