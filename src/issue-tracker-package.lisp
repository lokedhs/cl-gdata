(defpackage :cl-gdata-issue-tracker
  (:use :cl :cl-gdata :cl-gdata-misc)
  (:export #:issue
           #:issue-content
           #:issue-author-name
           #:issue-author-uri
           #:issue-label
           #:issue-owner-username
           #:issue-owner-uri
           #:issue-stars
           #:issue-state
           #:issue-status
           #:list-issues
           #:add-comment))
