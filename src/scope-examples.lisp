;;;
;;; Device example
;;;
;;;
;;; Create the session object
;;;
(defparameter *session*
  (make-instance 'cl-gdata-device:device-session
		 :client-id "*****" ;; See Google Developer Console
		 :client-secret "*****"))
;;;
;;; The first time that it is used, it must be authorized. This will print
;;; an informative message and return the URL for that. This code can be
;;; reused at a later stage.
;;;
(defparameter *code*
  (cl-gdata-scope:scope-session-authorize *session* "https://www.googleapis.com/auth/calendar"))
;;;
;;;
;;;
(setf (cl-gdata-scope:scope-session-code *session* "https://www.googleapis.com/auth/calendar")
      *code*)
;;;
;;; With the code that is generated we can request a token
;;;
(defparameter *token*
  (cl-gdata-scope:scope-session-get-token *session* "https://www.googleapis.com/auth/calendar"))
;;;
;;; This token can be refreshed
;;;
