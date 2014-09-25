(in-package :cl-gdata-oauth)

;;; insert your credentials and auxiliary information here.
(defparameter *key* "anonymous")
(defparameter *secret* "anonymous")
(defparameter *callback-uri* "oob")
(defparameter *callback-port* 8090
  "Port to listen on for the callback")


;;; go
(defparameter *get-request-token-endpoint* "https://www.google.com/accounts/OAuthGetRequestToken")
(defparameter *auth-request-token-endpoint* "https://www.google.com/accounts/OAuthAuthorizeToken")
(defparameter *get-access-token-endpoint* "https://www.google.com/accounts/OAuthGetAccessToken")
(defparameter *consumer-token* (oauth:make-consumer-token :key *key* :secret *secret*))
(defparameter *request-token* nil)
(defparameter *access-token* nil)

(defun get-access-token ()
  (oauth:obtain-access-token *get-access-token-endpoint* *request-token*))

;;; get a request token
(defun get-request-token (scope)
  ;; TODO: scope could be a list.
  (oauth:obtain-request-token
    *get-request-token-endpoint*
    *consumer-token*
    :callback-uri *callback-uri*
    :user-parameters `(("scope" . ,scope))))

;;; set up callback uri
(defun callback-dispatcher (request)
  (declare (ignorable request))
  (unless (cl-ppcre:scan "favicon\.ico$" (hunchentoot:script-name request))
    (lambda (&rest args)
      (declare (ignore args))
      (handler-case
          (oauth:authorize-request-token-from-request
            (lambda (rt-key)
              (assert *request-token*)
              (unless (equal (oauth:url-encode rt-key) (oauth:token-key *request-token*))
                (warn "Keys differ: ~S / ~S~%" (oauth:url-encode rt-key) (oauth:token-key *request-token*)))
              *request-token*))
        (error (c)
          (warn "Couldn't verify request token authorization: ~A" c)))
      (when (oauth:request-token-authorized-p *request-token*)
        (format t "Successfully verified request token with key ~S~%" (oauth:token-key *request-token*))
        (setf *access-token* (get-access-token))
        ;; test request:
        (let ((result (oauth:access-protected-resource
                        "http://www.google.com/calendar/feeds/default/allcalendars/full?orderby=starttime"
                        *access-token*)))
          (if (stringp result)
            result
            (babel:octets-to-string result)))))))

(defun oauth-test ()
  (setf *request-token* (get-request-token "https://docs.google.com/feeds/default/private/full"))
  (let ((auth-uri (oauth:make-authorization-uri *auth-request-token-endpoint* *request-token*)))
    (format t "Please authorize the request token at this URI: ~A~%" (puri:uri auth-uri))))

#+nil
(progn
  (pushnew 'callback-dispatcher hunchentoot:*dispatch-table*)

  (defvar *web-server* nil)

  (when *web-server*
    (hunchentoot:stop *web-server*)
    (setf *web-server* nil))

  (setf *web-server* (hunchentoot:start (make-instance 'hunchentoot:acceptor :port *callback-port*))))

(defclass oauth-session ()
  ((token-hash :type hashtable
               :initform (make-hash-table :test 'equal)
               :reader oauth-session-token-hash
               :documentation "A hashmap of authentication keys"))
  (:documentation "Session instance for OAuth GData sessions"))

(define-condition oauth-authentication-failed (authentication-failed)
  ()
  (:documentation "Condition that is signalled when an OAuth request fails"))

(defun authenticate-if-needed (session key)
  (let ((token (gethash key (oauth-session-token-hash session))))
    ))

(defmethod authenticated-request (url (session oauth-session)
                                  &key
                                  (method :get) (parameters nil) (content nil) (want-stream nil)
                                  (content-type nil) (additional-headers nil) (user-agent "cl-gdata")
                                  (force-binary nil) (content-length nil))
  (let ((token (authenticate-if-needed session :foo)))
    token))
