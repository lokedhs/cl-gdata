(defpackage :cl-gdata
  (:use :cl)
  (:export #:gdata-authenticate
           #:authentication-failed
           #:*verbose-http-errors*
           #:*gdata-session*))

(defpackage :cl-gdata-user
  (:use :cl
        :cl-gdata
        :cl-gdata-misc
        :cl-gdata-contacts
        :cl-gdata-docs-list
        :cl-gdata-spreadsheets
        :cl-gdata-picasa
        :cl-gdata-issue-tracker))

(in-package :cl-gdata)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
