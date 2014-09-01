(defpackage #:jumpotron-test
  (:use :cl :jumpotron :hu.dwim.stefil)
  (:export #:test-suite))
(in-package #:jumpotron-test)

(defsuite* (test-suite
            :in root-suite
            :documentation "Tests for Jumpotron"))

;;; Some plumbing to imitate an actual request context

(defvar *redirect*)

(defclass fake-response () ())

(defmethod clack.response:redirect ((response fake-response) url
                                    &optional status)
  (setf *redirect* (list url status)))

(defmacro with-request-context (&body body)
  `(let ((jumpotron::*jumps* (make-hash-table :test #'equal))
         (*response* (make-instance 'fake-response))
         (*redirect* nil))
     ,@body))

;;; Testing redirects

(defun test-redirect (trigger target input output)
  (with-request-context
    (define-redirect trigger target)
    (jumpotron::process-query input)
    (is (string= (first *redirect*) output))))

(deftest simple-redirect ()
  (test-redirect "test-trigger" "success" "test-trigger" "success"))

(deftest rest-redirect ()
  (test-redirect "test-trigger" "~@{~A~^+~}"
                 "Testing test-trigger for science!" "Testing+for+science!"))
