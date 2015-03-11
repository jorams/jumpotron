(in-package #:jumpotron)

(defvar *handler*)

(defvar *app* (make-instance '<app>))

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
*RESPONSE* having appropriate values."))

(defmethod suggest (jump query-parts)
  (remove nil (hash-table-keys *jumps*)))

(defun process-query (query)
  "Splits the query into multiple words and finds the associated jump (if any).
Returns two values: The associated jump (or nil) and a list of words in the
query."
  (let* ((parts (split-sequence #\Space query))
         (trigger (find-if #'(lambda (part) (gethash part *jumps*))
                           parts))
         (jump (gethash trigger *jumps*))
         (remaining-parts (if (exclude-trigger-p jump)
                              (remove trigger parts :test #'equal
                                                    :count 1)
                              parts)))
    (values jump remaining-parts)))

(defun parameter (name params)
  (cdr (assoc name params :test #'equalp)))

(defun jump-route (params)
  (multiple-value-call #'jump
    (process-query (or (parameter "q" params)
                       (first (parameter :splat params))))))

(defun suggest-route (params)
  (clack.response:push-header *response* :content-type "application/json")
  (multiple-value-bind (jump words)
      (process-query (parameter "q" params))
    (with-output-to-string (json-stream)
      (yason:encode (list (format nil "~{~A~^ ~}" words)
                          (suggest jump words))
                    json-stream))))

(setf (route *app* "/")
      #'(lambda (params)
          (declare (ignore params))
          "Thanks for passing by. Now jump back into hyperspace!"))

(setf (route *app* "/jump") 'jump-route)
(setf (route *app* "/suggest") 'suggest-route)
(setf (route *app* "/*") 'jump-route)

(defun add-jump (trigger jump)
"Add JUMP to the global hash table of jumps under the key TRIGGER.

If TRIGGER is NIL, JUMP will be used when no other jump is triggered."
  (setf (gethash trigger *jumps*) jump))


;;; Plumbing

(defun start (&optional (port 5000))
  "Starts Jumpotron on PORT"
  (if (not (boundp '*handler*))
      (setf *handler*
            (clack:clackup *app*
                           :port port))
      (error "Already started.")))

(defun stop ()
  "Stops a running Jumpotron"
  (when (boundp '*handler*)
    (clack.handler:stop *handler*)
    (makunbound '*handler*)))
