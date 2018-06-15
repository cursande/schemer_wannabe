;; Exercise 1.37.

;; a. An infinite continued fraction is an expression of the form...
;; As an example, one can show that the infinite continued fraction expansion with the
;; Ni and the Di all equal to 1 produces 1/ϕ, where ϕ is the golden ratio (described in section 1.2.2).
;; One way to approximate an infinite continued fraction is to truncate the expansion after a given number of terms.

;; Such a truncation -- a so-called k-term finite continued fraction -- has the form...

;; Suppose that n and d are procedures of one argument (the term index i) that return the
;; Ni and Di of the terms of the continued fraction.

;; Define a procedure cont-frac such that evaluating(cont-frac n d k) computes the value of the k-term finite continued fraction. Check your procedure by approximating 1/ϕ using

;; (cont-frac (lambda (i) 1.0)
;;            (lambda (i) 1.0)
;;            k)

;; for successive values of k. How large must you make k in order to get an approximation that is
;; accurate to 4 decimal places?

(define (cont-frac n d k)
  (define (iter i result)
    (newline)
    (display result)
    (display " - ")
    (display i)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result))))) ; for each iteration of k, (Nk/Dk)
  (iter k 0))


(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           15)

;; 0 - 15
;; 1. - 14
;; .5 - 13
;; .6666666666666666 - 12
;; .6000000000000001 - 11
;; .625 - 10
;; .6153846153846154 - 9
;; .6190476190476191 - 8
;; .6176470588235294 - 7
;; .6181818181818182 - 6
;; .6179775280898876 - 5
;; .6180555555555556 - 4
;; .6180257510729613 - 3
;; .6180371352785146 - 2
;; .6180327868852459 - 1
;; .6180344478216819 - 0
;Value: .6180344478216819

; 15 - 4 = 11
; k needs to be atleast 11 to be accurate to 4 decimal places

;; b. If your cont-frac procedure generates a recursive process, write one that
;; generates an iterative process. If it generates an iterative process, write one
;; that generates a recursive process.
