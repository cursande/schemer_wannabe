;; Exercise 3.26. To search a table as implemented above, one needs to scan through the list of records.
;; This is basically the unordered list representation of section 2.3.3. For large tables, it may be more
;; efficient to structure the table in a different manner. Describe a table implementation where the (key,
;; value) records are organized using a binary tree, assuming that keys can be ordered in some way (e.g.,
;; numerically or alphabetically). (Compare exercise 2.66 of chapter 2.)

;; Assuming keys can be ordered, an effective implementation could be implementing a binary search for each
;; (sub)table.
;;
;; The difficulty is in modifying the table each time with insert. As we insert values it will become unbalanced, so
;; having to rebuild the tree everytime means that reading is cheap but writing is very expensive. Maybe a solution
;; is to have each subtable have a distinct binary tree, so that insert only replaces one level of the table, but even
;; this would have a great cost as that subtable grows.
