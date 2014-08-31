(defpackage #:jumpotron
  (:use #:cl #:ningle #:split-sequence)
  (:import-from #:clack.response
                #:redirect)
  (:export #:start
           #:stop
           #:defjump))
