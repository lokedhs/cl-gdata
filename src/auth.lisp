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
  (let ((username (read)))
    (format t "Enter new password: ")
    (let ((password (read)))
      (list username password))))

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
          ((200) (make-instance 'gdata-session :auth (parse-auth-reply response)))
          (t     (error "Unsupported response code from ClientLogin: ~a" response))))
    ;; Restarts
    (use-new-login (new-username new-password)
      :report "Use new username and password"
      :interactive read-new-username-and-passwd
      (setq username new-username)
      (setq password new-password))))
