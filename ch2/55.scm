;; Exercise 2.55. Eva Lu Ator types to the interpreter the expression

;; (car ''abracadabra)

;; To her surprise, the interpreter prints back quote. Explain.

;; `'foo is just shorthand for `(quote foo)`, as foo is just given as argument to the quote operator.

;; The interpreter reads (car (quote (quote abracadabra))), where `quote` the symbol is the first element of the list,
;; while `abracadabra` the symbol is the second element in the list.
