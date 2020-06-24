(defsystem "starfish"
  :description "A simple actor system."
  :author "Jerome Chan <eviltofu@mac.com>"
  :maintainer "Jerome Chan <eviltofu@mac.com>"
  :version "0.1.0"
  :license "LGPL"
  :depends-on ("bordeaux-threads"
	       "local-time"
	       "trivial-types"
	       "usocket"
	       "uuid")
  :components ((:module "src"
		:serial t
		:components
		((:file "package")
		 (:file "types")
		 (:file "time")
		 (:file "doubly-linked-list")
		 (:file "mailbox"))))
  :in-order-to ((test-op (test-op "starfish/tests"))))

(defsystem "starfish/tests"
  :depends-on ("starfish"
	       "rove")
  :components ((:module "tests"
		:serial t
		:components
		((:file "time")
		 (:file "doubly-linked-list")
		 (:file "mailbox")
		 )))
  :perform (test-op (op c) (symbol-call :rove :run c)))
