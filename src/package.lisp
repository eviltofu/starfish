(defpackage #:starfish
  (:use #:cl)
  (:export
   ;;; types
   #:non-negative-integer-type
   #:positive-integer-type
   #:linked-list-position-type
   ;;; time
   #:time-interval-in-seconds
   #:offset-from-time
   #:offset-from-now
   #:days
   #:hours
   #:minutes
   #:seconds
   #:+days-in-seconds+
   #:+hours-in-seconds+
   #:+minutes-in-seconds+
   ;;; doubly-linked list
   #:linked-list
   #:make-linked-list
   #:linked-list-size
   #:linked-list-length
   #:linked-list-empty-p
   #:linked-list-full-p
   #:add-value
   #:remove-value
   #:*default-maximum-list-size*
   #:unknown-position-error
   #:linked-list-full-error
   #:linked-list-empty-error
   ;;; mailbox
   #:mailbox
   #:make-mailbox
   #:mailbox-write
   #:mailbox-read
   #:*default-mailbox-size*
   #:*default-timeout*
   ;;; address
   ;;; postal service
   ))


