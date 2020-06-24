;;;; Functions that handle time.

(in-package #:starfish)


(declaim (type (non-negative-integer-type) +days-in-seconds+))
(defconstant +days-in-seconds+ 86400
  "Number of seconds in a day.")

(declaim (type (non-negative-integer-type) +hours-in-seconds+))
(defconstant +hours-in-seconds+ 3600
  "Number of seconds in an hour.")

(declaim (type (non-negative-integer-type) minutes-in-seconds))
(defconstant +minutes-in-seconds+ 60
  "Number of seconds in a minute.")

(declaim
 (ftype (function
	 (&key (:days non-negative-integer-type)
	       (:hours non-negative-integer-type)
	       (:minutes non-negative-integer-type)
	       (:seconds non-negative-integer-type)) non-negative-integer-type)
	time-interval-in-seconds))
(defun time-interval-in-seconds (&key (days 0) (hours 0) (minutes 0) (seconds 0))
  "Create a time interval given days, hours, minutes, and seconds for input."
  (+ seconds
     (minutes minutes)
     (hours hours)
     (days days)))

(declaim
 (ftype (function
	 (non-negative-integer-type
	  &key (:days non-negative-integer-type)
	  (:hours non-negative-integer-type)
	  (:minutes non-negative-integer-type)
	  (:seconds non-negative-integer-type)) non-negative-integer-type)
	offset-from-time))
(defun offset-from-time (time &key (days 0) (hours 0) (minutes 0) (seconds 0))
  "Compute the new time given a time and an offset."
  (+ time
     (time-interval-in-seconds :days days
			       :hours hours
			       :minutes minutes
			       :seconds seconds)))

(declaim
 (ftype (function
	 (&key (:days non-negative-integer-type)
	       (:hours non-negative-integer-type)
	       (:minutes non-negative-integer-type)
	       (:seconds non-negative-integer-type)) non-negative-integer-type)
	offset-from-now))
(defun offset-from-now (&key (days 0) (hours 0) (minutes 0) (seconds 0))
  "Compute the new time from now and an offset."
  (offset-from-time (get-universal-time)
		    :days days
		    :hours hours
		    :minutes minutes
		    :seconds seconds))

(declaim (ftype (function (non-negative-integer-type) non-negative-integer-type) days))
(defun days (var)
  "Convert days into seconds."
  (* var +days-in-seconds+))

(declaim (ftype (function (non-negative-integer-type) non-negative-integer-type) hours))
(defun hours (var)
  "Convert hours into seconds."
  (* var +hours-in-seconds+))

(declaim (ftype (function (non-negative-integer-type) non-negative-integer-type) minutes))
(defun minutes (var)
  "Convert minutes into seconds."
  (* var +minutes-in-seconds+))

(declaim (ftype (function (non-negative-integer-type) non-negative-integer-type) seconds))
(defun seconds (var)
  "Number of seconds. Use as a mnemonic device."
  var)
