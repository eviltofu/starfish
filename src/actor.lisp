;;;; Actors contain a mailbox each. They can send messages to actors. Actors process their own mailbox.

(in-package #:starfish)

(defclass actor ()
  ((mailbox
    :type mailbox
    :initarg mailbox
    :accessor actor-mailbox
    :documentation "A mailbox.")
   (done
    :type boolean
    :initform nil
    :accessor actor-done
    :documentation "A boolean to store the completed state of the actor.")
   (status
    :initarg status
    :accessor actor-status
    :documentation "The status of the actor.")
   (time-out-function
    :type function
    :initarg :process-time-out-function
    :accessor actor-time-out-function
    :documentation "The function to execute when a time out occurs. 
The status is the only arg passed to the function. 
The function needs to return one or two values. 
The first is the new status.
The second optional value is a boolean indicating if the actor should stop.
True values means the actor stops.")
   (time-out-interval
    :type positive-integer-type
    :initarg :time-out-interval
    :initform *default-timeout*
    :accessor actor-time-out-interval
    :documentation "The time out interval")
   (process-mailbox-function
    :type function
    :initarg :process-mailbox-function
    :accessor actor-process-mailbox-function
    :documentation "The function to execute when you have to read the mailbox.
The status is the only arg passed to the function.
The function needs to return one or two values.
The first is the new status.
The second optional value is a boolean indicating if the actor should stop.
True values means the actor stops.")
   (thread
    :type bt:thread
    :initarg thread
    :reader actor-thread
    :documentation "The thread running the actor process.")
   (actor-lock
    :type bt:lock
    :initform (bt:make-lock)
    :reader actor-lock
    :documentation "This guards access to the actor in multi-threaded situations."))
  (:documentation "This class implements a simple actor model."))

(defgeneric start (actor)
  (:documentation "Starts the actor."))
(defmethod start ((actor actor))
  )

(defgeneric stop (actor)
  (:documentation "Stops the actor."))
(defmethod stop ((actor actor))
  )

(defgeneric is-running-p (actor)
  (:documentation "Is the actor running?"))
(defmethod is-running-p ((actor actor))
  )

(defgeneric send-message (actor message)
  (:documentation "Sends a message to the actor."))
(defmethod send-message ((actor actor) message)
  )

(declaim (ftype (function (&key
			   (:process-mailbox-function (function (t) (values t boolean)))
			   (:mailbox-size positive-integer-type)
			   (:process-time-out-function (or (function (t) (values t boolean)) nil)) 
			   (:time-out-interval-in-seconds (or positive-integer-type nil)))
			  actor)
		make-actor))
(defun make-actor (&key
		     (process-mailbox-function)
		     (mailbox-size *default-mailbox-size*)
		     (process-time-out-function)
		     (time-out-interval-in-seconds *default-timeout*))
  (let ((actor (make-instance 'actor
			      :mailbox (make-instance 'mailbox :size mailbox-size)
			      :process-mailbox-function process-mailbox-function
			      :process-time-out-function process-time-out-function
			      :time-out-interval time-out-interval-in-seconds)))
    actor))


