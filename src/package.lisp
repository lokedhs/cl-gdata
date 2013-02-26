(defpackage :cl-gdata
  (:use :cl)
  (:export #:gdata-authenticate
           #:authentication-failed
           #:*verbose-http-errors*
           #:*gdata-session*
           #:*gdata-api-key*)
  (:documentation "cl-gdata is a project aimed at implementing the Google GData API's in Common Lisp.
The actual implementations of the different services can be found in the packages:
CL-GDATA-SPREADSHEETS, CL-GDATA-CONTACTS, CL-GDATA-DOCS-LIST, CL-GDATA-ISSUE-TRACKER,
CL-GDATA, CL-GDATA-PICASA"))

(in-package :cl-gdata)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
