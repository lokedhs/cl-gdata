(in-package :cl-gdata-device)

;;;
;;; This file implements the Google OAuth2 For Devices protocol
;;;    https://developers.google.com/identity/protocols/OAuth2ForDevices
;;;

(defparameter *device-auth-endpoint* "https://accounts.google.com/o/oauth2/device/code")
(defparameter *device-token-endpoint* "https://www.googleapis.com/oauth2/v3/token")

(defclass device-session (scope-session)
  (;;; These values are obtained (for Google) from here: https://code.google.com/apis/console
   (client-id :type string :initform "" :initarg :client-id
	      :reader device-session-client-id)
   (client-secret :type (or string null) :initform nil :initarg :client-secret
		  :reader device-session-client-secret))
  (:documentation "Session instance for Device GData sessions"))

(defun get-device-uri (scope client-id)
  "Return an URL to authorise this application in the given scopes.

CLIENT-ID is information that determines the application and is provided
by Google Developers Console.

SCOPE is one or more services for which we request authentication. It
must be a string designator or a list of thoses."
  (when (listp scope)
    (setf scope (format nil "~{~A ~}" scope)))
  (multiple-value-bind (response status)
      (parsed-json-request "authorising application"
			   *device-auth-endpoint*
			   `(("client_id" . ,client-id)
			     ("scope" .  ,scope)))
    (if (= status 200)
	(let* ((device-code (cdr (assoc :DEVICE--CODE response)))
	       (code (cdr (assoc :USER--CODE response)))
	       (url (cdr (assoc :VERIFICATION--URL response)))
	       (seconds (cdr (assoc :EXPIRES--IN response))))
	  (format t "Please open the address
  ~A
in your browser and enter the code
  ~A
to authorize this application. This page expires in ~D seconds"
		  url code seconds)
	  device-code)
	(error 'oauth2-authentication-failed
	       :status status
	       :response response))))

(defmethod scope-session-authorize ((session device-session) scope)
  (get-device-uri scope (device-session-client-id session)))

(defmethod scope-session-get-token ((session device-session) scope)
  (request-oauth2-token
   *device-token-endpoint*
   `(("client_id" . ,(device-session-client-id session))
     ("client_secret" . ,(device-session-client-secret session))
     ("code" . ,(scope-session-code session scope))
     ("grant_type" . "http://oauth.net/grant_type/device/1.0"))))

(defmethod scope-session-refresh-token ((session device-session) scope token)
  (let ((new-token (request-oauth2-token
		    *device-token-endpoint*
		    `(("client_id" . ,(device-session-client-id session))
		      ("client_secret" . ,(device-session-client-secret session))
		      ("refresh_token" . ,(oauth2-token-refresh-code token))
		      ("grant_type" . "refresh_token"))
		    :errorp nil)))
    (and new-token
	 (oauth2-token-update-with token new-token))))

