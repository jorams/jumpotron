(defpackage #:jumpotron
  (:use #:cl #:ningle #:split-sequence)
  (:import-from #:alexandria
                #:hash-table-keys)
  (:export #:start
           #:stop
           #:jump                       ; Both class and generic function
           #:suggest
           #:add-jump
           #:redirecting-jump
           #:define-redirect

           ;; Re-exported symbols from NINGLE
           #:*request*
           #:*response*
           #:*context*))
