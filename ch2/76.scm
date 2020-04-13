;; Exercise 2.76. As a large system with generic operations evolves, new types of data objects or new
;; operations may be needed. For each of the three strategies -- generic operations with explicit dispatch,
;; data-directed style, and message-passing-style -- describe the changes that must be made to a system in
;; order to add new types or new operations. Which organization would be most appropriate for a system
;; in which new types must often be added? Which would be most appropriate for a system in which new
;; operations must often be added?

;; for explicit dispatch:
;; - system must be modified to accommodate a new type or operation
;; - name conflicts must be avoided by using things like prefixes and suffixes
;; - We only have to implement each operation once, but for every new operation we
;;   add, we then have to add as many conditional dispatches as there are
;;   different types, as well as modify existing generic operations

;; for data-directed style:
;; - if operations are added to existing packages, nothing about the broader system or
;;   higher level processes needs to change
;; - New types mean new packages, this allows us to add new types without having to worry about
;;   name conflicts.
;; - New type means a new column in the table. New operation means a new row.

;; for message-passing style:
;; - Similar benefits to data-directed style
;; - if new types appear, we need to build new entities or data objects to represent them
;; - if a new operation appears, we need to modify the dispatch procedure in previously constructed entities so
;;   that they can handle it
;; - Once we do that though, we can pass the same messages to the new entities as the existing ones

;; Data-directed and message-passing are quite fundamentally similar.

;; Most appropriate where new types are often added:
;; For new types, both the message-passing and data-directed approaches will work. We can create the new entity,
;; and define the procedures for that entity so that it can handle the same messages as existing
;; entities. A new type just means adding a new 'smart' object to handle existing operations. In the same way, installing
;; a new package with an interface to the system is equally localised. So they're both totally viable.
;; Would just depend on which makes more sense for the problem at hand?

;; Most appropriate where new operations are often added:
;; If there are very limited types and risk of name conflict is low, functions with explicit dispatch might be fine
;; and the simplest approach. Otherwise the data-directed and message-passing approaches are a similar situation of
;; updating either existing objects or packages to support the new operation. Maybe semantically it might make more
;; sense to have a data-directed approach if these are very abstract methods of a general process and don't make
;; sense as distinct 'entities'.
