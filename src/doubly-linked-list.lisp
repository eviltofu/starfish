;;;; A doubly-linked-list implementation.

(in-package #:starfish)

;;; Global variables

(declaim (type (positive-integer-type) *default-maximum-list-size*))
(defparameter *default-maximum-list-size* 256)

;;; Conditions

(define-condition unknown-position-error (error)
  ((position
    :initarg :position)))

(define-condition linked-list-full-error (error)
  ((linked-list
    :initarg :linked-list)))

(define-condition linked-list-empty-error (error)
  ((linked-list
    :initarg :linked-list)))

;;; Classes

(defclass node ()
  ((previous-node
    :type node
    :initarg :previous-node
    :initform nil
    :accessor previous-node
    :documentation "The previous node. Nil indicates no previous node.")
   (next-node
    :type node
    :initarg :next-node
    :initform nil
    :accessor next-node
    :documentation "The next node. Nil indicates no next node.")
   (value
    :initarg :value
    :accessor value
    :documentation "The value stored in the node. Can be anything."))
  (:documentation "A node of a doubly-linked-list."))

(defclass linked-list ()
  ((head
    :type node
    :initform nil
    :reader head
    :documentation "The head of the list. Nil means no head.")
   (tail
    :type node
    :initform nil
    :reader tail
    :documentation "The tail of the list. Nil means no tail.")
   (size
    :type non-negative-integer-type
    :initarg :size
    :reader linked-list-size
    :documentation "How many nodes can the linked list contain.")
   (length
    :type non-negative-integer-type
    :initform 0
    :reader linked-list-length
    :documentation "How many nodes the linked list actually contains."))
  (:documentation "A doubly-linked-list."))

;;; Public functions

(declaim (ftype (function (&key (:size positive-integer-type)) linked-list) make-linked-list))
(defun make-linked-list (&key (size *default-maximum-list-size*))
  "Make a linked list. Size determines how many nodes can be in the list. If no size is given, a default size is used."
  (let ((created-linked-list (make-instance 'linked-list :size size)))
    created-linked-list))

(declaim (ftype (function (linked-list t &optional linked-list-position-type) linked-list) add-value))
(defun add-value (linked-list value &optional (position :rear))
  "Adds something to the linked list.
You can specify which end of the list the value is added to via :front or :rear symbols.
If you specify an unknown symbol, the function will signal an unknown-position-error.
If the list is at maximum capacity (length == size), the function will signal a linked-list-full-error."
  (cond
    ((linked-list-full-p linked-list)
     (error 'linked-list-full-error :linked-list linked-list))
    ((linked-list-empty-p linked-list)
     (add-to-empty linked-list value))
    ((eql position :rear)
     (add-to-rear linked-list value))
    ((eql position :front)
     (add-to-front linked-list value))
    (t (error 'unknown-position-error :position position))))

(declaim (ftype (function (linked-list &optional linked-list-position-type) t) remove-value))
(defun remove-value (linked-list &optional (position :front))
  "Remove something from the linked list. 
You can specify which end of the list the value is removed from via :front or :rear symbols.
If the list is empty, linked-list-empty-error is signaled. "
  (cond
    ((linked-list-empty-p linked-list)
     (error 'linked-list-empty-error :linked-list linked-list))
    ((eql 1 (linked-list-length linked-list))
     (remove-from-single-item-list linked-list))
    ((eql position :rear)
     (remove-from-rear linked-list))
    ((eql position :front)
     (remove-from-front linked-list))
    (t (error 'unknown-position-error :position position))))

(declaim (ftype (function (linked-list) (or null t)) linked-list-empty-p))
(defun linked-list-empty-p (linked-list)
  "Is the linked list empty?"
  (with-slots (length) linked-list
    (eq 0 length)))


(declaim (ftype (function (linked-list) (or null t)) linked-list-full-p))
(defun linked-list-full-p (linked-list)
  "Is the linked list at maximum capacity?"
  (with-slots (length size) linked-list
    (eq length size)))

;;; Private functions

(declaim (ftype (function (linked-list t) linked-list) add-to-front))
(defun add-to-front (linked-list value)
  "Add the value to the front of the linked list."
  (with-slots (head length) linked-list
    (let ((new-node (make-instance 'node :value value :previous-node head)))
      (setf (next-node head) new-node)
      (setf head new-node)
      (setf length (1+ length)))
    linked-list))

(declaim (ftype (function (linked-list t) linked-list) add-to-rear))
(defun add-to-rear (linked-list value)
  "Add the value to the rear of the linked list."
  (with-slots (tail length) linked-list
    (let ((new-node (make-instance 'node :value value :next-node tail)))
      (setf (previous-node tail) new-node)
      (setf tail new-node)
      (setf length (1+ length)))
    linked-list))

(declaim (ftype (function (linked-list t) linked-list) add-to-empty))
(defun add-to-empty (linked-list value)
  "Add the value to an empty list."
  (with-slots (head tail length) linked-list
    (let ((new-node (make-instance 'node :value value)))
      (setf head new-node)
      (setf tail new-node)
      (setf length 1))
    linked-list))

(declaim (ftype (function (linked-list) t) remove-from-single-item-list))
(defun remove-from-single-item-list (linked-list)
  "Remove the value from a list containing only one item."
  (with-slots (head tail length) linked-list
    (let ((return-value (value head)))
      (reset-node head)
      (setf head nil)
      (setf tail nil)
      (setf length 0)
      return-value)))

(declaim (ftype (function (linked-list) t) remove-from-front))
(defun remove-from-front (linked-list)
  "Remove the value from the front of the list."
  (with-slots (head length) linked-list
    (let ((return-value (value head))
	  (new-head (previous-node head)))
      (reset-node head)
      (setf head new-head)
      (setf length (1- length))
      return-value)))

(declaim (ftype (function (linked-list) t) remove-from-rear))
(defun remove-from-rear (linked-list)
  "Remove the value from the rear of the list."
  (with-slots (tail length) linked-list
    (let ((return-value (value tail))
	  (new-tail (next-node tail)))
      (reset-node tail)
      (setf tail new-tail)
      (setf length (1- length))
      return-value)))

(declaim (ftype (function (node)) nil-node))
(defun reset-node (node)
  "Reset the node so that it points to nothing and contains nothing."
  (with-slots (value next-node previous-node) node
    (setf value nil)
    (setf next-node nil)
    (setf previous-node nil)))
