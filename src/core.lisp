(defpackage :jumpotron/core
  (:use :cl :hunchentoot)
  (:import-from :alexandria
                #:hash-table-keys)
  (:shadow #:redirect
           #:url-encode)
  (:export #:jump                       ; Both class and generic function
           #:suggest
           #:add-jump
           #:make-jumpotron
           #:redirect
           #:url-encode))
(in-package :jumpotron/core)

;;; Basics --------------------------------------------------------------------

(defvar *jumps* (make-hash-table :test 'equal))

(defclass jump ()
  ((exclude-trigger-p :initarg :exclude-trigger-p
                      :initform t
                      :reader exclude-trigger-p
                      :documentation "Whether or not the query to pass to this
jump should be stripped of the triggering word.")))

(defgeneric jump (jump query-parts)
  (:documentation "Do something with JUMP based on the words in QUERY-PARTS.
This generic function shouldn't ever be called outside the context of a request.
Therefore, when implementing methods, you can rely on *CONTEXT*, *REQUEST* and
*RESPONSE* having appropriate values."))

(defgeneric suggest (jump query-parts)
  (:documentation "Return a list of (string) suggestions based on QUERY-PARTS.
This generic function shouldn't ever be called outside the context of a request.
Therefore, when implementing methods, you can rely on *CONTEXT*, *REQUEST* and
*RESPONSE* having appropriate values.")
  (:method (jump query-parts)
    "By default we suggest jump names."
    (declare (ignore jump query-parts))
    (remove nil (hash-table-keys *jumps*))))

(defun add-jump (trigger jump)
"Add JUMP to the global hash table of jumps under the key TRIGGER.

If TRIGGER is NIL, JUMP will be used when no other jump is triggered."
  (setf (gethash trigger *jumps*) jump))

;;; Query processing ----------------------------------------------------------

(defun process-query (query)
  "Splits the query into multiple words and finds the associated jump (if any).
Returns two values: The associated jump (or nil) and a list of words in the
query."
  (let* ((parts (ppcre:split "\\s" query))
         (trigger (find-if #'(lambda (part) (gethash part *jumps*))
                           parts))
         (jump (gethash trigger *jumps*))
         (remaining-parts (if (exclude-trigger-p jump)
                              (remove trigger parts :test #'equal
                                                    :count 1)
                              parts)))
    (values jump remaining-parts)))

;;; Acceptor ------------------------------------------------------------------

(defclass jumpotron-acceptor (acceptor) ())

(defmethod acceptor-dispatch-request ((acceptor jumpotron-acceptor) request)
  (cond
    ((string= (script-name*) "/")
     "Thanks for passing by. Now jump back into hyperspace!")
    ((string= (script-name*) "/jump")
     (multiple-value-call #'jump
       (process-query (or (parameter "q") ""))))
    ((string= (script-name*) "/suggest")
     (setf (header-out "Content-Type") "application/json")
     (multiple-value-bind (jump words)
         (process-query (parameter "q"))
       (with-output-to-string (json-stream)
         (yason:encode (list (format nil "~{~A~^ ~}" words)
                             (suggest jump words))
                       json-stream))))))

;;; Utilities -----------------------------------------------------------------

(defun make-jumpotron (&optional (port 5000))
  (make-instance 'jumpotron-acceptor
                 :port port))

(defun redirect (url)
  (hunchentoot:redirect url :code +http-see-other+))

(defun url-encode (string)
  (hunchentoot:url-encode string))
