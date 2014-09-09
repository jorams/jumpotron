(defpackage #:jumpotron.bookmarks
  (:use #:cl #:jumpotron)
  (:import-from #:clack.response
                #:redirect)
  (:import-from #:alexandria
                #:hash-table-keys)
  (:import-from #:ningle
                #:*response*)
  (:export #:bookmarking-jump
           #:bookmark-jump))
(in-package #:jumpotron.bookmarks)

(defvar *bookmarks* (make-hash-table :test #'equal))

(defclass bookmarking-jump (jump) ())
(defclass bookmark-jump (jump) ())

(defmethod jump ((jump bookmarking-jump) query-parts)
  (if (= 2 (length query-parts))
      (setf (gethash (first query-parts) *bookmarks*)
            (second query-parts))
      "Can't add bookmark, wrong number of arguments"))

(defmethod jump ((jump bookmark-jump) query-parts)
  (if (gethash (first query-parts) *bookmarks*)
      (redirect *response* (gethash (first query-parts) *bookmarks*))
      "Bookmark doesn't exist."))

(defmethod suggest ((jump bookmark-jump) query-parts)
  (hash-table-keys *bookmarks*))
