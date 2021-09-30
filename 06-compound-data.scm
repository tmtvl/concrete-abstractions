;;; Exercise 6.1
;; The advantage of having three operations, of which one is
;; remove-coins-from-pile with a limit is that it is easier to change the limit
;; of coins that can be taken from a pile. The disadvantage is that we have to
;; clearly express when the limit is being exceeded and we may have to take
;; care to handle that situation if it comes up.
;; The advantage to having five operations, of which three happen to be the
;; remove-one, remove-two, and remove-three operations is that it is no longer
;; possible for an illegal action to occur. The downside is that we need to add
;; additional operations if we want to increase the limit or delete them to
;; decrease the limit. There may also be a disadvantage of having duplicate
;; code which would need to be updated if we change other aspects of the game.
