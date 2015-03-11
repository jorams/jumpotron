(defpackage #:jumpotron
  (:use #:cl #:ningle #:split-sequence)
  (:import-from #:clack.response
                #:redirect)
  (:import-from #:alexandria
                #:hash-table-keys)
  (:import-from #:quri
                #:url-encode)
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
