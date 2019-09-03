;; Exercise 2.47. Here are two possible constructors for frames:

;; (define (make-frame origin edge1 edge2)
;;   (list origin edge1 edge2))

;; (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))

;; For each constructor supply the appropriate selectors to produce an implementation for frames.

(define (frame-origin1 frame) (car frame))

(define (frame-first-edge1 frame) (cadr frame))

(define (frame-second-edge1 frame) (caddr frame))

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

;; ---------------------------

(define (frame-origin2 frame) (car frame))

(define (frame-first-edge2 frame) (cadr frame))

(define (frame-second-edge2 frame) (cddr frame))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
