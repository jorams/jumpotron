(in-package #:jumpotron)

(defclass redirecting-jump (jump)
  ((target :initarg :target
           :initform (error "A jump needs a target")
           :reader target
           :documentation "FORMAT-string that will consume the words in the
query to construct a url to redirect the user to.")))

(defmethod jump ((jump redirecting-jump) query-parts)
  (redirect *response*
            (apply #'format nil (target jump) query-parts)))

(defun define-redirect (trigger format-string &optional (exclude-trigger-p t))
  "Defines a new redirecting jump. If a request comes in where a word in the
query is EQUAL to PREFIX, the user will be redirected to the result of calling
FORMAT with FORMAT-STRING and the words in the query. If EXCLUDE-TRIGGER-P is
NIL those words will also still contain TRIGGER.

This is equivalent to, and actually does, the following:

    (add-jump trigger
                (make-instance 'redirecting-jump
                               :target format-string
                               :exclude-trigger-p exclude-trigger-p))"
  (add-jump trigger
            (make-instance 'redirecting-jump
                           :target format-string
                           :exclude-trigger-p exclude-trigger-p)))
