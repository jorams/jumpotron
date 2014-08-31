(asdf:defsystem #:jumpotron
  :serial t
  :description "Go ahead and jump."
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:clack #:ningle #:split-sequence)
  :components ((:file "packages")
               (:file "jumpotron")))
