(in-package :cl-gdata-app)

;;;
;;; This file implements the Google OAuth2 For Devices protocol
;;;    https://developers.google.com/identity/protocols/OAuth2InstalledApp
;;; with hints from the PHP file
;;;    https://github.com/googleads/googleads-php-lib/blob/master/examples/AdWords/Auth/GetRefreshToken.php
;;;

(defparameter *app-auth-endpoint* "https://accounts.google.com/o/oauth2/auth")
(defparameter *app-token-endpoint* "https://www.googleapis.com/oauth2/v3/token")

(defclass app-session (scope-session)
  (;;; These values are obtained (for Google) from here: https://code.google.com/apis/console
   (client-id :type string :initform "" :initarg :client-id
	      :reader app-session-client-id)
   (client-secret :type (or string null) :initform nil :initarg :client-secret
		  :reader app-session-client-secret))
  (:documentation "Session instance for Device GData sessions"))

(defun make-app-uri (scope client-id)
  "Return an URL to authorise this application in the given scopes.

CLIENT-ID is information that determines the application and is provided
by Google Developers Console.

SCOPE is one or more services for which we request authentication. It
must be a string designator or a list of thoses."
  (when (listp scope)
    (setf scope (format nil "~{~A ~}" scope)))
  (let ((endpoint (puri:uri *app-auth-endpoint*)))
    (setf (puri:uri-query endpoint)
	  (drakma::alist-to-url-encoded-string
	   `(("response_type" . "code")
	     ("client_id" . ,client-id)
	     ("redirect_uri" . "urn:ietf:wg:oauth:2.0:oob")
	     ("scope" .  ,scope))
	   :latin1  'drakma:url-encode))
    (puri:uri endpoint)))

(defmethod scope-session-authorize ((session app-session) scope)
  (format t "Please open the address
  ~A
in your browser and copy the code that it is offered in this application.
Enter code:~%" (make-app-uri scope (app-session-client-id session)))
  ;; With this code we have to immediately authorise the scope
  ;; because, while the code expires, the refresh token does not.
  (let ((code (string-trim '(#\Space #\NewLine) (read-line))))
    (setf (scope-session-code session scope)
	  code)
    (setf (scope-session-token session scope)
	  (scope-session-get-token session scope))))

(defmethod scope-session-get-token ((session app-session) scope)
  (request-oauth2-token
   *app-token-endpoint*
   `(("client_id" . ,(app-session-client-id session))
     ("client_secret" . ,(app-session-client-secret session))
     ("code" . ,(scope-session-code session scope))
     ("redirect_uri" . "urn:ietf:wg:oauth:2.0:oob")
     ("grant_type" . "authorization_code"))))

(defmethod scope-session-refresh-token ((session app-session) scope token)
  (let ((new-token (request-oauth2-token
		    *app-token-endpoint*
		    `(("client_id" . ,(app-session-client-id session))
		      ("client_secret" . ,(app-session-client-secret session))
		      ("refresh_token" . ,(oauth2-token-refresh-code token))
		      ("grant_type" . "refresh_token"))
		    :errorp nil)))
    (and new-token
	 (oauth2-token-update-with token new-token))))

