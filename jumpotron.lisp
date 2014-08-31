(in-package #:jumpotron)

(defvar *handler*)

(defvar *app* (make-instance '<app>))

(defvar *jumps* (make-hash-table :test 'equal))

(defun jump (format arguments)
  (when format
    (redirect *response*
              (apply #'format nil format arguments))))

(defun jump-route (params)
  (let* ((query (or (getf params :|q|)
                    (first (getf params :splat))))
         (arguments (split-sequence #\Space query)))
    (jump (gethash (first arguments) *jumps*)
          (rest arguments))))

(setf (route *app* "/")
      #'(lambda (params)
          (declare (ignore params))
          "Thanks for passing by. Now jump back into hyperspace!"))

(setf (route *app* "/jump") #'jump-route)
(setf (route *app* "/*") #'jump-route)


(defun defjump (prefix format-string)
  "Defines a new jump. If a request comes in where the first word in the query
is EQUAL to PREFIX, the user will be redirected to the result of calling
FORMAT with FORMAT-STRING and the rest of the words in the query.

All jumps are stored in a global hash table. "
  (setf (gethash prefix *jumps*) format-string))

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
