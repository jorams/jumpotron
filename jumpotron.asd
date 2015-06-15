(asdf:defsystem #:jumpotron
  :description "Go ahead and jump."
  :author "Joram Schrijver <i@joram.io>"
  :license "MIT"
  :depends-on (#:hunchentoot #:alexandria #:yason #:cl-ppcre)
  :serial t
  :pathname "src"
  :components ((:file "core")
               (:file "redirecting-jump")
               (:file "bookmarks")
               (:file "jumpotron")))
