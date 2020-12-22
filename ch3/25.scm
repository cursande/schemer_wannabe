;; Exercise 3.25. Generalizing one- and two-dimensional tables, show how to implement a table in
;; which values are stored under an arbitrary number of keys and different values may be stored under
;; different numbers of keys. The lookup and insert! procedures should take as input a list of keys
;; used to access the table.

(define (make-table same-key?-proc)
  (let ((local-table (list '*table*)))

    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key?-proc key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (lookup keys)
      (define (iter ks subtable)
        (let ((found (assoc (car ks) (cdr subtable)))
              (last-key? (null? (cdr ks))))
          (cond ((and found last-key?) (cdr found))
                (found (iter (cdr ks) found))
                (else false))))
      (iter keys local-table))

    (define (insert! keys value)
      (define (iter ks subtable)
        (let ((found (assoc (car ks) (cdr subtable)))
              (last-key? (null? (cdr ks))))
          (cond ((and found last-key?)
                 (begin (set-cdr! found value)
                        'ok))
                (last-key?
                 (begin (set-cdr! subtable
                                  (cons (cons (car ks) value)
                                        (cdr subtable)))
                        'ok))
                (found
                 (iter (cdr ks) found))
                (else
                 (let ((new-subtable (list (car ks))))
                   (begin (set-cdr! subtable
                                    (cons new-subtable
                                          (cdr subtable)))
                          (iter (cdr ks) new-subtable)))))))
      (iter keys local-table))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put (list 'a 'b) "foo")
(get (list 'a 'b)) ;; => "foo"

(put (list 'a 'c 'x) "foo")
(put (list 'a 'c 'd 'e) 12)

(get (list 'a 'c 'x)) ;; => "foo"
(get (list 'a 'c 'd 'e)) ;; => 12
