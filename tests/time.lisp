(defpackage "starfish/tests/time"
  (:use #:cl
	#:rove))

(in-package "starfish/tests/time")

(setf *break-on-signals* nil)

(deftest time-tests
  (testing "seconds"
    (ok (eql 0
	    (starfish:time-interval-in-seconds :seconds 0)))
    (ok (eql 60
	    (starfish:time-interval-in-seconds :minutes 1)))
    (ok (eql 3600
	    (starfish:time-interval-in-seconds :hours 1)))
    (ok (eql 86400
	    (starfish:time-interval-in-seconds :days 1)))
    (ok (eql 90061
	    (starfish:time-interval-in-seconds :days 1 :hours 1 :minutes 1 :seconds 1)))))
