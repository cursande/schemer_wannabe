;; Exercise 3.32. The procedures to be run during each time segment of the agenda are kept in a queue.
;; Thus, the procedures for each segment are called in the order in which they were added to the agenda
;; (first in, first out). Explain why this order must be used. In particular, trace the behavior of an and-gate
;; whose inputs change from 0,1 to 1,0 in the same segment and say how the behavior would differ if we
;; stored a segment’s procedures in an ordinary list, adding and removing procedures only at the front
;; (last in, first out).
;;
;; We need to ensure the chronological order is enforced, as the values for each wire get propagated.
;; In the example of the and-gate, the problem would be that by using a last-in, first-out stack, we would be calling
;; wire actions in reverse. This incorrect ordering means that the output of of our and-gate would be 1 instead of 0.
