(in-package :cl-gdata)

(declaim #.*compile-decl*)

(defvar *verbose-http-errors* nil
  "If non-nil, report detailed HTTP error information to *DEBUG-IO*")

(defvar *gdata-session* nil
  "The last authenticated session. Used as a default for gdata functions.")
