;;;; The types used in the system.

(in-package #:starfish)

;;; Types and type functions

(deftype non-negative-integer-type ()
  "Integers greater than or equal to zero."
  `(satisfies non-negative-integer-type-p))

(declaim (ftype (function (t) (or null t)) non-negative-integer-type-p))
(defun non-negative-integer-type-p (var)
  "Determines if var is a non-negative-integer."
  (and (typep var 'integer)
       (>= var 0)))

(deftype positive-integer-type ()
  "Integers greater than zero."
  `(satisfies positive-integer-p))

(declaim (ftype (function (t) (or null t)) positive-integer-p))
(defun positive-integer-p (var)
  "Determines if var is a positive integer."
  (and (typep var 'integer)
       (plusp var)))

(deftype linked-list-position-type ()
  "Symbols representing :front and :rear."
  `(satisfies linked-list-position-p))

(declaim (ftype (function (t) (or null t)) linked-list-position-p))
(defun linked-list-position-p (item)
  "Determines if item is :front or :rear."
  (or (eq item :front)
      (eq item :rear)))



