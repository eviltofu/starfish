;;;; Mailboxes allow messages to be written to and read from in a coherent way.

(in-package #:starfish)

;;; Global variables

(declaim (type (positive-integer-type) *default-mailbox-size*))
(defparameter *default-mailbox-size* 1024
  "Default maximum size of a mailbox.")

(declaim (type (positive-integer-type) *default-timeout*))
(defparameter *default-timeout* (minutes 5)
  "Default time out.")

;;; Classes

(defclass mailbox ()
  ((mailbox-lock
    :type bt:lock
    :initform (bt:make-lock)
    :accessor mailbox-lock
    :documentation "Lock to allow coherent multiple access to mailbox.")
   (read-condition
    :initform (bt:make-condition-variable)
    :accessor mailbox-read-condition
    :documentation "Condition variable for reading access.")
   (write-condition
    :initform (bt:make-condition-variable)
    :accessor mailbox-write-condition
    :documentation "Condition variable for writing access.")
   (storage
    :type linked-list
    :initarg :storage
    :initform (make-linked-list)
    :accessor mailbox-storage
    :documentation "A linked list used to store all messages."))
  (:documentation "A mailbox used for reading and writing messages."))

;;; Public functions

(declaim (ftype (function (&key (:size positive-integer-type)) mailbox) make-mailbox))
(defun make-mailbox (&key (size *default-mailbox-size*))
  "Create a mail box given the size. If size is not specified, the default mailbox size will be used."
  (let ((mailbox (make-instance 'mailbox :storage (make-linked-list :size size))))
    mailbox))

(declaim (ftype (function (mailbox t &key (:timeout (or null non-negative-integer-type))) mailbox) mailbox-write))
(defun mailbox-write (mailbox message &key (timeout *default-timeout*))
  "Write the message to the mail box, given a time out. 
When the time out expires, a bt:timeout error is signaled. 
If the time out is nil, the function will block until space is available in the mailbox.
If the time out is not specified, the default time out is used."
  (if (null timeout)
      (mailbox-write-with-no-timeout mailbox message)
      (mailbox-write-with-timeout mailbox message timeout)))

(declaim (ftype (function (mailbox &key (:timeout (or null non-negative-integer-type))) t) mailbox-read))
(defun mailbox-read (mailbox &key (timeout *default-timeout*))
  "Read from the mail box and return a message, given a time out.
When the time out expires, a bt:error error is signaled.
If the time out is nil, the function will block until a message is available in the mailbox.
If the time out is not specified, the default time out is used."
  (if (null timeout)
      (mailbox-read-with-no-timeout mailbox)
      (mailbox-read-with-timeout mailbox timeout)))

;;; Private functions

(declaim (ftype (function (mailbox t) mailbox) mailbox-write-with-no-timeout))
(defun mailbox-write-with-no-timeout (mailbox message)
  "Write to the mail box."
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
  "Write to the mail box with a time out."
  (bt:with-timeout (timeout)
    (mailbox-write-with-no-timeout mailbox message)))

(declaim (ftype (function (mailbox) t) mailbox-read-with-no-timeout))
(defun mailbox-read-with-no-timeout (mailbox)
  "Read from the mail box."
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
  "Read from the mailbox with a time out."
  (bt:with-timeout (timeout)
    (mailbox-read-with-no-timeout mailbox)))
