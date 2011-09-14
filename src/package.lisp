(defpackage :cl-gdata
  (:use :cl)
  (:export #:gdata-authenticate
           #:authentication-failed))

(in-package :cl-gdata)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *compile-decl* '(optimize (speed 0) (safety 3) (debug 3))))
