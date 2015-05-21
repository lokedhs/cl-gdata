(defpackage :cl-gdata-scope
  (:use :cl :cl-gdata-misc)
  (:export #:scope-session
	   #:scope-session-authorize
	   #:scope-session-get-token
	   #:scope-session-refresh-token
	   #:scope-session-code
	   #:scope-session-token

	   #:parsed-json-request

	   #:oauth2-authentication-failed
	   #:oauth2-token
	   #:oauth2-token-expired-p
	   #:oauth2-token-expiration-date
	   #:oauth2-token-update-with
	   #:oauth2-token-refresh-code
	   #:request-oauth2-token))
