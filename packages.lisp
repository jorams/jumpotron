(defpackage #:jumpotron
  (:use #:cl #:ningle #:split-sequence)
  (:import-from #:clack.response
                #:redirect)
  (:export #:start
           #:stop
           #:jump                       ; Both class and generic function
           #:add-jump
           #:redirecting-jump
           #:define-redirect

           ;; Re-exported symbols from NINGLE
           #:*request*
           #:*response*
           #:*context*))
