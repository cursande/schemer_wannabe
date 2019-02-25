;; Exercise 2.31. Abstract your answer to exercise 2.30 to produce a procedure tree-map with the
;; property that square-tree could be defined as

;; (define (square-tree tree) (tree-map square tree))

(define (tree-map proc tree)
  (map (lambda (subtree)
         (if (not (pair? subtree))
             (proc subtree)
             (tree-map proc subtree)))
       tree))

(define (square x) (* x x))

(define (square-tree tree) (tree-map square tree))

(define number-tree (list 1
                      (list 2 (list 3 4) 5)
                      (list 6 7)))

(square-tree number-tree) ; => (1 (4 (9 16) 25) (36 49))
