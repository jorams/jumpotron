(asdf:defsystem #:jumpotron
  :serial t
  :description "Go ahead and jump."
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:clack #:ningle #:split-sequence
               #:alexandria #:yason)
  :components ((:file "packages")
               (:file "jumpotron")
               (:file "redirecting-jump")
               (:file "bookmarks"))
  :in-order-to ((test-op (test-op :jumpotron-test))))

(defsystem jumpotron-test
  :licence "MIT"
  :pathname "test"
  :components ((:file "jumpotron"))
  :depends-on (#:jumpotron #:hu.dwim.stefil)
  :perform (test-op (o s)
                    (uiop:symbol-call :jumpotron-test '#:test-suite)))
