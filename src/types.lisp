;;;; The types used in the system.

(in-package #:starfish)

;;; Types and type functions

(deftype non-negative-integer-type ()
  "Integers greater than or equal to zero."
  `(integer 0))

(deftype positive-integer-type ()
  "Integers greater than zero."
  `(integer 1))

(deftype linked-list-position-type ()
  "Symbols representing :front and :rear."
  `(member :front :rear))



