;; The ‘‘eight-queens puzzle’’ asks how to place eight queens on a chessboard so that no queen is in
;; check from any other (i.e., no two queens are in the same row, column, or diagonal).

;; One way to solve the puzzle is to work across the board, placing a queen in each column.
;; Once we have placed k - 1 queens, we must place the kth queen in a position where it does not
;; check any of the queens already on the board.

;; We can formulate this approach recursively:
;; Assume that we have already generated the sequence of all possible ways to place k - 1 queens in
;; the first k - 1 columns of the board. For each of these ways, generate an extended set of
;; positions by placing a queen in each row of the kth column.

;; Now filter these, keeping only the positions for which the queen in the kth column is
;; safe with respect to the other queens. This produces the sequence of all ways to place k
;; queens in the first k columns. By continuing this process, we will produce not only one solution,
;; but all solutions to the puzzle.

;; We implement this solution as a procedure `queens`, which returns a sequence of all solutions to the
;; problem of placing n queens on an n x n chessboard. Queens has an internal procedure queen-cols
;; that returns the sequence of all ways to place queens in the first k columns of the board.

;; (define (queens board-size)
;;   (define (queen-cols k)
;;     (if (= k 0)
;;         (list empty-board)
;;         (filter
;;          (lambda (positions) (safe? k positions))
;;          (flatmap
;;           (lambda (rest-of-queens)
;;             (map (lambda (new-row)
;;                    (adjoin-position new-row k rest-of-queens))
;;                  (enumerate-interval 1 board-size)))
;;           (queen-cols (- k 1))))))
;;   (queen-cols board-size))

;; In this procedure rest-of-queens is a way to place k - 1 queens in the first k - 1 columns, and
;; new-row is a proposed row in which to place the queen for the kth column.

;; Complete the program by implementing the representation for sets of board positions,
;; including the procedure
;; `adjoin-position`, which adjoins a new row-column position to a set of positions, and
;; `empty-board`, which represents an empty set of positions.
;; You must ;; also write the procedure `safe?`, which determines for a set of positions,
;; whether the queen in the kth column is safe with respect to the others.
;; (Note that we need only check whether the new queen is safe -- the other queens
;; are already guaranteed safe with respect to each other.)

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

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define (queen-in-kth-column k positions)
  (if (= k (cadar positions))
      (car positions)
      (queen-in-kth-column k (cdr positions))))

(define (matching-row? queen other-queen)
  (= (car queen) (car other-queen)))

(define (matching-column? queen other-queen)
  (= (cadr queen) (cadr other-queen)))

(define (matching-diagonal? queen other-queen)
  (= (abs (- (car queen) (car other-queen)))
     (abs (- (cadr queen) (cadr other-queen)))))

(define (under-attack? queen other-queen)
  (or (matching-row? queen other-queen)
      (matching-diagonal? queen other-queen)))

;; goddamn, I could not figure out that I needed this without help
(define (board-without-current-queen queen positions)
  (filter (lambda (other-queen)
            (not (and (matching-row? queen other-queen)
                      (matching-column? queen other-queen))))
          positions))

;; You must also write the procedure `safe?`, which determines for a set of positions,
;; whether the queen in the kth column is safe with respect to the others.
;; (Note that we need only check whether the new queen is safe -- the other queens
;; are already guaranteed safe with respect to each other.)
;; Needs to return a boolean value
(define (safe? k positions)
  (let ((current-queen (queen-in-kth-column k positions)))
    (let ((current-queen-row (car current-queen))
          (current-queen-column (cadr current-queen)))
      (null?
       (filter (lambda (other-queen)
                 (let ((other-queen-row (car other-queen))
                       (other-queen-column (cadr other-queen)))
                   (under-attack? current-queen other-queen)))
               (board-without-current-queen current-queen positions))))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
