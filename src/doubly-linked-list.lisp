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
    :initarg :previous-node
    :initform nil
    :accessor previous-node)
   (next-node
    :initarg :next-node
    :initform nil
    :accessor next-node)
   (value
    :initarg :value
    :accessor value)))

(defclass linked-list ()
  ((head
    :initform nil
    :reader head)
   (tail
    :initform nil
    :reader tail)
   (size
    :initarg :size
    :reader linked-list-size)
   (length
    :initform 0
    :reader linked-list-length)))

;;; Public functions

(declaim (ftype (function (&key (:size positive-integer-type)) linked-list) make-linked-list))
(defun make-linked-list (&key (size *default-maximum-list-size*))
  (let ((created-linked-list (make-instance 'linked-list :size size)))
    created-linked-list))

(declaim (ftype (function (linked-list t &optional linked-list-position-type) linked-list) add-value))
(defun add-value (linked-list value &optional (position :rear))
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
  (with-slots (length) linked-list
    (eq 0 length)))

(declaim (ftype (function (linked-list) (or null t)) linked-list-full-p))
(defun linked-list-full-p (linked-list)
  (with-slots (length size) linked-list
    (eq length size)))

;;; Private functions

(declaim (ftype (function (linked-list t) linked-list) add-to-front))
(defun add-to-front (linked-list value)
  (with-slots (head length) linked-list
    (let ((new-node (make-instance 'node :value value :previous-node head)))
      (setf (next-node head) new-node)
      (setf head new-node)
      (setf length (1+ length)))
    linked-list))

(declaim (ftype (function (linked-list t) linked-list) add-to-rear))
(defun add-to-rear (linked-list value)
  (with-slots (tail length) linked-list
    (let ((new-node (make-instance 'node :value value :next-node tail)))
      (setf (previous-node tail) new-node)
      (setf tail new-node)
      (setf length (1+ length)))
    linked-list))

(declaim (ftype (function (linked-list t) linked-list) add-to-empty))
(defun add-to-empty (linked-list value)
  (with-slots (head tail length) linked-list
    (let ((new-node (make-instance 'node :value value)))
      (setf head new-node)
      (setf tail new-node)
      (setf length 1))
    linked-list))

(declaim (ftype (function (linked-list) t) remove-from-single-item-list))
(defun remove-from-single-item-list (linked-list)
  (with-slots (head tail length) linked-list
    (let ((return-value (value head)))
      (nil-node head)
      (setf head nil)
      (setf tail nil)
      (setf length 0)
      return-value)))

(declaim (ftype (function (linked-list) t) remove-from-front))
(defun remove-from-front (linked-list)
  (with-slots (head length) linked-list
    (let ((return-value (value head))
	  (new-head (previous-node head)))
      (nil-node head)
      (setf head new-head)
      (setf length (1- length))
      return-value)))

(declaim (ftype (function (linked-list) t) remove-from-rear))
(defun remove-from-rear (linked-list)
  (with-slots (tail length) linked-list
    (let ((return-value (value tail))
	  (new-tail (next-node tail)))
      (nil-node tail)
      (setf tail new-tail)
      (setf length (1- length))
      return-value)))

(declaim (ftype (function (node)) nil-node))
(defun nil-node (node)
  (with-slots (value next-node previous-node) node
    (setf value nil)
    (setf next-node nil)
    (setf previous-node nil)))
