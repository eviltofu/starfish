(defpackage "starfish/tests/mailbox"
  (:use #:cl
	#:rove))

(in-package "starfish/tests/mailbox")

(setf *break-on-signals* nil)

(deftest mailbox-creation-tests
  (testing "Creation"
    (let ((mailbox (starfish:make-mailbox :size 10)))
      (ok (typep mailbox 'starfish:mailbox)))))

(deftest mailbox-reading-and-writing-tests
  (testing "Sequential order"
    (let ((mailbox (starfish:make-mailbox)))
      (starfish:mailbox-write mailbox "one" :timeout nil)
      (starfish:mailbox-write mailbox "two" :timeout nil)
      (starfish:mailbox-write mailbox "three" :timeout nil)
      (ok (string= "one" (starfish:mailbox-read mailbox :timeout nil)))
      (ok (string= "two" (starfish:mailbox-read mailbox :timeout nil)))
      (ok (string= "three" (starfish:mailbox-read mailbox :timeout nil)))))
  (testing "Time outs"
    (let ((mailbox (starfish:make-mailbox :size 2)))
      (ok (signals (starfish:mailbox-read mailbox :timeout 1)
	      'bt:timeout))
      (starfish:mailbox-write mailbox "one")
      (starfish:mailbox-write mailbox "two")
      (ok (signals (starfish:mailbox-write mailbox "three" :timeout 1)
	      'bt:timeout))))
  (testing "Multiple threads"
    (let* ((max-count 1000)
	   (mailbox (starfish:make-mailbox :size 2))
	   (ones 0)
	   (twos 0)
	   (threes 0)
	   (fours 0)
	   (one-count (random max-count))
	   (two-count (random max-count))
	   (three-count (random max-count))
	   (four-count (random max-count))
	   one-writer-thread
	   two-writer-thread
	   three-writer-thread
	   four-writer-thread
	   reader-thread)
      (setf starfish:*default-timeout* 2)
      (setf one-writer-thread
	    (bt:make-thread
	     (lambda ()
	       (loop repeat one-count do (starfish:mailbox-write mailbox "1")))
	     :name "One"))
      (setf two-writer-thread
	    (bt:make-thread
	     (lambda ()
	       (loop repeat two-count do (starfish:mailbox-write mailbox "2")))
	     :name "Two"))
      (setf three-writer-thread
	    (bt:make-thread
	     (lambda ()
	       (loop repeat three-count do (starfish:mailbox-write mailbox "3")))
	     :name "Three"))
      (setf four-writer-thread
	    (bt:make-thread
	     (lambda ()
	       (loop repeat four-count do (starfish:mailbox-write mailbox "4")))
	     :name "Four"))
      (setf reader-thread
	    (bt:make-thread
	     (lambda ()
	       (let ((limit (+ one-count two-count three-count four-count))
		     result)
		 (loop repeat limit do
		   (progn
		     (setf result (starfish:mailbox-read mailbox))
		     (cond
		       ((eql "1" result)
			(incf ones))
		       ((eql "2" result)
			(incf twos))
		       ((eql "3" result)
			(incf threes))
		       ((eql "4" result)
			(incf fours)))))))
	     :name "Reader"))
      (bt:join-thread one-writer-thread)
      (bt:join-thread two-writer-thread)
      (bt:join-thread three-writer-thread)
      (bt:join-thread four-writer-thread)
      (bt:join-thread reader-thread)
      (ok (eql one-count ones))
      (ok (eql two-count twos))
      (ok (eql three-count threes))
      (ok (eql four-count fours)))))

