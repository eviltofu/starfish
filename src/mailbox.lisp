(in-package #:starfish)

;;; Global variables

(declaim (type (positive-integer-type) *default-mailbox-size*))
(defparameter *default-mailbox-size* 1024)

(declaim (type (positive-integer-type) *default-timeout*))
(defparameter *default-timeout* (minutes 5))

;;; Classes

(defclass mailbox ()
  ((mailbox-lock
    :initform (bt:make-lock)
    :accessor mailbox-lock)
   (read-condition
    :initform (bt:make-condition-variable)
    :accessor mailbox-read-condition)
   (write-condition
    :initform (bt:make-condition-variable)
    :accessor mailbox-write-condition)
   (storage
    :initarg :storage
    :initform (make-linked-list)
    :accessor mailbox-storage)))

;;; Public functions

(declaim (ftype (function (&key (:size positive-integer-type)) mailbox) make-mailbox))
(defun make-mailbox (&key (size *default-mailbox-size*))
  (let ((mailbox (make-instance 'mailbox :storage (make-linked-list :size size))))
    mailbox))

(declaim (ftype (function (mailbox t &key (:timeout (or null non-negative-integer-type))) mailbox) mailbox-write))
(defun mailbox-write (mailbox message &key (timeout *default-timeout*))
  (if (null timeout)
      (mailbox-write-with-no-timeout mailbox message)
      (mailbox-write-with-timeout mailbox message timeout)))

(declaim (ftype (function (mailbox &key (:timeout (or null non-negative-integer-type))) t) mailbox-read))
(defun mailbox-read (mailbox &key (timeout *default-timeout*))
  (if (null timeout)
      (mailbox-read-with-no-timeout mailbox)
      (mailbox-read-with-timeout mailbox timeout)))

;;; Private functions

(declaim (ftype (function (mailbox t) mailbox) mailbox-write-with-no-timeout))
(defun mailbox-write-with-no-timeout (mailbox message)
  (with-slots (mailbox-lock write-condition read-condition storage) mailbox
    (bt:with-lock-held (mailbox-lock)
      (loop
	(when (not (starfish:linked-list-full-p storage))
	  (return (starfish:add-value storage message)))
	(bt:condition-wait write-condition mailbox-lock))
      (bt:condition-notify read-condition)))
  mailbox)

(declaim (ftype (function (mailbox t non-negative-integer-type) mailbox) mailbox-write-with-timeout))
(defun mailbox-write-with-timeout (mailbox message timeout)
  (bt:with-timeout (timeout)
    (mailbox-write-with-no-timeout mailbox message)))

(declaim (ftype (function (mailbox) t) mailbox-read-with-no-timeout))
(defun mailbox-read-with-no-timeout (mailbox)
  (let ((return-value nil))
    (with-slots (mailbox-lock write-condition read-condition storage) mailbox
      (bt:with-lock-held (mailbox-lock)
	(loop
	  (when (not (starfish:linked-list-empty-p storage))
	    (setf return-value (starfish:remove-value storage))
	    (return))
	  (bt:condition-wait read-condition mailbox-lock))
	(bt:condition-notify write-condition)))
    return-value))

(declaim (ftype (function (mailbox non-negative-integer-type) t) mailbox-read-with-timeout))
(defun mailbox-read-with-timeout (mailbox timeout)
  (bt:with-timeout (timeout)
    (mailbox-read-with-no-timeout mailbox)))
