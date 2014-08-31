(in-package #:jumpotron)

(defvar *handler*)

(defvar *app* (make-instance '<app>))

(defvar *jumps* (make-hash-table :test 'equal))

(defclass jump ()
  ((trigger :initarg :trigger
            :initform (error "A jump needs a trigger")
            :reader trigger)
   (target :initarg :target
           :initform (error "A jump needs a target")
           :reader target)
   (exclude-trigger-p :initarg :exclude-trigger-p
                      :initform t
                      :reader exclude-trigger-p)))

(defun jump (query)
  (let* ((parts (split-sequence #\Space query))
         (jump-trigger (find-if #'(lambda (part) (gethash part *jumps*))
                                parts))
         (jump (gethash jump-trigger *jumps*))
         (remaining-parts (if (exclude-trigger-p jump)
                              (remove (trigger jump) parts :test #'equal
                                                           :count 1)
                              parts)))
    (redirect *response*
              (apply #'format nil (target jump) remaining-parts))))

(defun jump-route (params)
  (jump (or (getf params :|q|)
            (first (getf params :splat)))))

(setf (route *app* "/")
      #'(lambda (params)
          (declare (ignore params))
          "Thanks for passing by. Now jump back into hyperspace!"))

(setf (route *app* "/jump") #'jump-route)
(setf (route *app* "/*") #'jump-route)


(defun defjump (trigger format-string &optional (exclude-trigger-p t))
  "Defines a new jump. If a request comes in where a word in the query
is EQUAL to PREFIX, the user will be redirected to the result of calling
FORMAT with FORMAT-STRING and the words in the query. If EXCLUDE-TRIGGER-P is
NIL those words will also still contain TRIGGER.

All jumps are stored in a global hash table. "
  (setf (gethash trigger *jumps*)
        (make-instance 'jump
                       :trigger trigger
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
