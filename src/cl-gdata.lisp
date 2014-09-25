(in-package :cl-gdata)

(defvar *verbose-http-errors* nil
  "If non-nil, report detailed HTTP error information to *DEBUG-IO*")

(defvar *gdata-session* nil
  "The last authenticated session. Used as a default for gdata functions.")

(defvar *gdata-api-key* nil
  "The default API key that is used by some services \(mainly the Google Calendar API)")
