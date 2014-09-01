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

(defclass redirecting-jump (jump)
  ((target :initarg :target
           :initform (error "A jump needs a target")
           :reader target
           :documentation "FORMAT-string that will consume the words in the
query to construct a url to redirect the user to.")))

(defgeneric jump (jump query-parts)
  (:documentation "Do something with JUMP based on the words in QUERY-PARTS.
This generic function shouldn't ever be called outside the context of a request.
Therefore, when implementing methods, you can rely on *CONTEXT*, *REQUEST* and
*RESPONSE* having appropriate values."))

(defmethod jump ((jump redirecting-jump) query-parts)
  (redirect *response*
            (apply #'format nil (target jump) query-parts)))

(defun process-query (query)
  (let* ((parts (split-sequence #\Space query))
         (trigger (find-if #'(lambda (part) (gethash part *jumps*))
                           parts))
         (jump (gethash trigger *jumps*))
         (remaining-parts (if (exclude-trigger-p jump)
                              (remove trigger parts :test #'equal
                                                    :count 1)
                              parts)))
    (jump jump remaining-parts)))

(defun jump-route (params)
  (process-query (or (getf params :|q|)
                     (first (getf params :splat)))))

(setf (route *app* "/")
      #'(lambda (params)
          (declare (ignore params))
          "Thanks for passing by. Now jump back into hyperspace!"))

(setf (route *app* "/jump") #'jump-route)
(setf (route *app* "/*") #'jump-route)

(defun add-jump (trigger jump)
"Add `JUMP` to the global hash table of jumps under the key `TRIGGER`."
  (setf (gethash trigger *jumps*) jump))

(defun define-redirect (trigger format-string &optional (exclude-trigger-p t))
  "Defines a new redirecting jump. If a request comes in where a word in the
query is EQUAL to PREFIX, the user will be redirected to the result of calling
FORMAT with FORMAT-STRING and the words in the query. If EXCLUDE-TRIGGER-P is
NIL those words will also still contain TRIGGER.

All jumps are stored in a global hash table. "
  (setf (gethash trigger *jumps*)
        (make-instance 'redirecting-jump
                       :target format-string
                       :exclude-trigger-p exclude-trigger-p)))

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
