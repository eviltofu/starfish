(in-package #:starfish)

(declaim
 (ftype
  (function
   (&key (:days non-negative-integer-type)
	 (:hours non-negative-integer-type)
	 (:minutes non-negative-integer-type)
	 (:seconds non-negative-integer-type))
   non-negative-integer-type)
  time-interval-in-seconds))
(defun time-interval-in-seconds (&key (days 0) (hours 0) (minutes 0) (seconds 0))
  (+ seconds
     (minutes minutes)
     (hours hours)
     (days days)))

(declaim (ftype (function (non-negative-integer-type) non-negative-integer-type) days))
(defun days (var)
  (* var 24 60 60))

(declaim (ftype (function (non-negative-integer-type) non-negative-integer-type) hours))
(defun hours (var)
  (* var 60 60))

(declaim (ftype (function (non-negative-integer-type) non-negative-integer-type) minutes))
(defun minutes (var)
  (* var 60))

(declaim (ftype (function (non-negative-integer-type) non-negative-integer-type) seconds))
(defun seconds (var)
  var)