(in-package #:starfish)

;;; Types and type functions

(deftype non-negative-integer-type ()
  `(satisfies non-negative-integer-type-p))

(declaim (ftype (function (t) (or null t)) non-negative-integer-type-p))
(defun non-negative-integer-type-p (var)
  (and (typep var 'integer)
       (>= var 0)))

(deftype positive-integer-type ()
  `(satisfies positive-integer-p))

(declaim (ftype (function (t) (or null t)) positive-integer-p))
(defun positive-integer-p (var)
  (and (typep var 'integer)
       (plusp var)))

(deftype linked-list-position-type ()
  `(satisfies linked-list-position-p))

(declaim (ftype (function (t) (or null t)) linked-list-position-p))
(defun linked-list-position-p (item)
  (or (eq item :front)
      (eq item :rear)))



