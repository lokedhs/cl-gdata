(in-package :cl-gdata-clientlogin)

(declaim #.cl-gdata::*compile-decl*)

(defclass clientlogin-session ()
  ((username  :type (or null string)
              :initarg :user
              :accessor clientlogin-session-username)
   (password  :type (or null string)
              :initarg :password)
   (auth-keys :type hash-table
              :initform (make-hash-table :test 'equal)
              :documentation "A hashtable that is keyed on the service name and
contains the authentication key as the value")))

(define-condition authentication-failed (error)
  ((username :initarg :username
             :reader authentication-failed-username)
   (response :initarg :response
             :reader authentication-failed-response))
  (:report (lambda (condition out)
             (format out "Authentication failed for username: ~a"
                     (authentication-failed-username condition))))
  (:documentation "Condition that is signalled when a ClientLogin request fails"))

(defun parse-auth-reply (s)
  (loop
     for row in (split-sequence:split-sequence #\Newline s)
     do (destructuring-bind (key value) (split-sequence:split-sequence #\= row :count 2)
          (when (string= key "Auth")
            (return value)))
     finally (error "Field \"Auth\" not found in ClientLogin response")))

(defun read-new-username-and-passwd ()
  (format t "Enter new username: ")
  (let ((username (read-line)))
    (format t "Enter new password: ")
    (let ((password (read-line)))
      (list username password))))

(defun clientlogin-authenticate (username password service &key (source "dhsDevelopments-lispApi-1"))
  (restart-case
      (multiple-value-bind (response code) (drakma:http-request "https://www.google.com/accounts/ClientLogin"
                                                                :method :post
                                                                :parameters `(("accountType" . "GOOGLE")
                                                                              ("Email" . ,username)
                                                                              ("Passwd" . ,password)
                                                                              ("service" . ,service)
                                                                              ("source" . ,source)))
        (case code
          ((403) (error 'authentication-failed :username username :response response))
          ((200) (parse-auth-reply response))
          (t     (error "Unsupported response code from ClientLogin: ~a" response))))
    ;; Restarts
    (use-new-login (new-username new-password)
      :report "Restart with different credentials"
      :interactive read-new-username-and-passwd
      (clientlogin-authenticate new-username new-password service :source source))))

(defun login-if-needed (session service-name)
  (with-slots (username password auth-keys) session
    (let ((auth (gethash service-name auth-keys)))
      (or auth
          (let ((new-auth (clientlogin-authenticate username password service-name)))
            (setf (gethash service-name auth-keys) new-auth)
            new-auth)))))

(define-condition missing-service-name (error)
  ((url :initarg :url
        :reader missing-service-name-url))
  (:report (lambda (condition out)
             (format out "Can't map url ~s to a service name" (missing-service-name-url condition)))))

(defun read-new-service-name ()
  (format t "Enter new service name: ")
  (list (read-line)))

(defparameter *service-name-map*
  (let ((m '(("^https://docs.google.com/feeds.*" "writely")
             ("^https://spreadsheets.google.com/feeds.*" "wise")
             ("^https://www.google.com/m8/feeds.*" "cp")
             ("^https://picasaweb.google.com/data/.*" "lh2")
             ("^https?://code.google.com/feeds.*" "code")
             ("^https://[^.]+.googleusercontent.com/docs/.*" "writely"))))
    (mapcar #'(lambda (v) (list (cl-ppcre:create-scanner (car v)) (cadr v))) m)))

(defun resolve-service-name-from-url (url)
  (restart-case
      (loop
         for (regex key) in *service-name-map*
         if (cl-ppcre:scan regex url)
         return key
         finally (error 'missing-service-name :url url))
    (specify-service-name (new-service-name)
      :report "Specify new service name"
      :interactive read-new-service-name
      new-service-name)))

(defmethod authenticated-request (url (session clientlogin-session)
                                  &key
                                  (method :get) (parameters nil) (content nil) (want-stream nil)
                                  (content-type nil) (additional-headers nil) (user-agent "cl-gdata")
                                  (force-binary nil) (content-length nil))
  (let ((auth-string (login-if-needed session (resolve-service-name-from-url url))))
    (apply #'drakma:http-request url
                         :method method
                         :parameters parameters
                         :additional-headers (append `(("Authorization" . ,(concatenate 'string
                                                                                        "GoogleLogin auth="
                                                                                        auth-string)))
                                                     additional-headers)
                         :want-stream want-stream
                         :user-agent user-agent
                         :content-type (or content-type "application/x-www-form-urlencoded")
                         :content content
                         :force-binary force-binary
                         (append (if content-length (list :content-length content-length) nil)))))
