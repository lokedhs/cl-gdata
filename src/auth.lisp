(in-package :cl-gdata)

(declaim #.*compile-decl*)

(defclass gdata-session ()
  ((auth :type string
         :initarg :auth
         :initform (error "~s argument required" :auth)
         :reader gdata-session-auth
         :documentation "Authentication string from ClientLogin")))

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
  (let ((username (eval (read))))
    (format t "Enter new password: ")
    (let ((password (eval (read))))
      (list username password))))

(defvar *gdata-session* nil
  "The last authenticated session. Used as a default for gdata functions.")

(defun gdata-authenticate (username password service &key (source "dhsDevelopments-lispApi-1"))
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
          ((200) (setq *gdata-session* (make-instance 'gdata-session :auth (parse-auth-reply response))))
          (t     (error "Unsupported response code from ClientLogin: ~a" response))))
    ;; Restarts
    (use-new-login (new-username new-password)
      :report "Restart with different credentials"
      :interactive read-new-username-and-passwd
      (gdata-authenticate new-username new-password service :source source))))

;;;
;;;  HTTP requests
;;;
(defun authenticated-request (url &key (session *gdata-session*) (method :get) (parameters nil) (content nil)
                              (want-stream nil) (content-type nil))
  (drakma:http-request url
                       :method method
                       :parameters parameters
                       :additional-headers `(("GData-Version" . "3.0")
                                             ("Authorization" . ,(concatenate 'string
                                                                              "GoogleLogin auth="
                                                                              (gdata-session-auth session))))
                       :want-stream want-stream
                       :content-type (or content-type "application/x-www-form-urlencoded")
                       :content content))
