(in-package :cl-gdata-scope)

(defparameter *oauth2-session-debug* t)

(define-condition oauth2-authentication-failed (authentication-failed)
  ((status :initarg :status
	   :reader oauth2-authentication-failed-status)
   (response :initarg :response
             :reader oauth2-authentication-failed-response))
  (:report (lambda (condition out)
             (format out "Authentication failed with code ~D"
                     (oauth2-authentication-failed-status condition))))
  (:documentation "Condition that is signalled when Google's OAuth2 request fails"))

(defun parsed-json-request (operation endpoint parameters)
  (when *oauth2-session-debug*
    (let ((*print-pretty* t))
      (format t "OAUTH2 JSON REQUEST~%Endpoint: ~A~%Parameters:~%~S"
	      endpoint parameters)))
  (multiple-value-bind (body status headers) ;...
	(drakma:http-request endpoint
			     :method :post
			     :parameters parameters)
    (declare (ignore headers))
    (defparameter *foo* body)
    (when (typep body '(vector (unsigned-byte 8)))
      (setf body (map 'string #'code-char body)))
    (when *oauth2-session-debug*
      (format t "~%Status: ~D~%Json string: ~S~%"
	      status body))
    (values (json::decode-json-from-string body)
	    status)))

(defclass oauth2-token ()
  ((string :type string :initarg :string
	   :reader oauth2-token-string)
   (expiration-date :type integer :initarg :expiration-date
		    :reader oauth2-token-expiration-date)
   (refresh-code :type (or null string) :initarg :refresh-code :initform nil
		 :reader oauth2-token-refresh-code)))

(defun oauth2-token-expired-p (token)
  (<= (oauth2-token-expiration-date token) (get-universal-time)))

(defun oauth2-token-update-with (token new-data)
  (setf (slot-value token 'string) (slot-value new-data 'string)
	(slot-value token 'expiration-date) (slot-value new-data 'expiration-date)
	(slot-value token 'refresh-code) (slot-value token 'refresh-code))
  token)

(defun request-oauth2-token (endpoint parameters &key (errorp t))
  (multiple-value-bind (response status)
      (parsed-json-request "token request" endpoint parameters)
    (cond ((eql status 200)
	   (make-instance 'cl-gdata-scope:oauth2-token
			  :string (cdr (assoc :access--token response))
			  :expiration-date (+ (get-universal-time)
					      (cdr (assoc :expires--in response)))
			  :refresh-code (cdr (assoc :refresh--token response))))
	  (errorp
	   (error 'oauth2-authentication-failed
		  :status status
		  :response response)))))

(defclass scope-session ()
  (;;; These values are obtained (for Google) from here: https://code.google.com/apis/console
   (code-hash :type hashtable :initform (make-hash-table :test 'equal)
	      :reader scope-session-code-hash
	      :documentation "A hashmap for the codes that are exchanged for tokens.")
   (token-hash :type hashtable
               :initform (make-hash-table :test 'equal)
               :reader scope-session-token-hash
               :documentation "A hashmap of authentication keys"))
  (:documentation "Base class for Google's OAuth2 sessions"))

(defgeneric scope-session-authorize (session scope)
  (:documentation "Generate a new code for a given scope. This
function returns and caches the code that validates this authorisation
and can be used to request token."))

(defgeneric scope-session-get-token (session scope)
  (:documentation "Generate a new token for this scope, using a previously obtained code."))

(defgeneric scope-session-refresh-token (session scope token)
  (:documentation "Use the refresh key of a token to get a new one, replacing the fields in the original token with new ones. Return NIL if not possible."))

(defmethod scope-session-refresh-token ((session scope-session) scope token)
  (declare (ignore session scope token))
  ;;; Default is not to do anything
  nil)

(defmethod scope-session-code ((session scope-session) scope)
  (or (gethash scope (scope-session-code-hash session))
      (setf (scope-session-code session scope)
	    (scope-session-authorize session scope))))

(defmethod (setf scope-session-code) (value (session scope-session) scope)
  (setf (gethash scope (scope-session-code-hash session))
	value))

(defmethod scope-session-token ((session scope-session) scope)
  (gethash scope (scope-session-token-hash session)))

(defmethod (setf scope-session-token) (value (session scope-session) scope)
  (setf (gethash scope (scope-session-token-hash session))
	value))

(defmethod scope-session-authenticate (session scope)
  (let ((token (scope-session-token session scope)))
    (cond ((null token)
	   (setf (scope-session-token session scope)
		 (scope-session-get-new-token session scope)))
	  ((not (oauth2-token-expired-p token))
	   (return-from scope-session-authenticate token))
	  ((scope-session-refresh-token session scope token))
	  (t
	   (setf (scope-session-token session scope)
		 (scope-session-get-new-token session scope))))))

(define-condition missing-scope (error)
  ((url :initarg :url
        :reader missing-scope-url))
  (:report (lambda (condition out)
             (format out "Can't map url ~s to a service name" (missing-scope-url condition)))))

(defparameter *scope-map*
  (let ((m '(("^https://www.googleapis.com/auth/calendar*"
	      "^https://www.googleapis.com/auth/calendar")
	     ("^https://docs.google.com/feeds/.*" "writely")
             ("^https://spreadsheets.google.com/.*"
	      "https://spreadsheets.google.com/feeds/")
             ("^https://www.google.com/m8/feeds/.*"
	      "https://www.google.com/m8/feeds/")
             ("^https://picasaweb.google.com/data/.*"
	      "https://picasaweb.google.com/data/")
	     ("^https://sites.google.com/feeds/.*"
	      "https://sites.google.com/feeds/")
	     ("^https://mail.google.com/.*"
	      "https://mail.google.com/"))))
    (mapcar #'(lambda (v) (list (cl-ppcre:create-scanner (car v)) (cadr v))) m)))

(defun resolve-scope-from-url (url)
  (loop
     for (regex key) in *scope-map*
     if (cl-ppcre:scan regex url)
     return key
     finally (error 'missing-scope :url url)))

(defmethod authenticated-request (url (session scope-session)
                                  &key
                                  (method :get) (parameters nil) (content nil) (want-stream nil)
                                  (content-type nil) (additional-headers nil) (user-agent "cl-gdata")
                                  (force-binary nil) (content-length nil))
  (let* ((token (scope-session-authenticate session (resolve-scope-from-url url)))
	 (auth-string (oauth2-token-string token)))
    (apply #'drakma:http-request url
           :method method
           :parameters parameters
           :additional-headers (append `(("Authorization" . ,(concatenate 'string
                                                                          "Bearer "
                                                                          auth-string)))
                                       additional-headers)
           :want-stream want-stream
           :user-agent user-agent
           :content-type (or content-type "application/x-www-form-urlencoded")
           :content content
           :force-binary force-binary
           :preserve-uri t
           (append (if content-length (list :content-length content-length) nil)))))
