(in-package :cl-gdata-misc)

(declaim #.cl-gdata::*compile-decl*)

(defclass json-instance ()
  ((data :type list
         :initarg :data
         :reader json-instance-data))
  (:documentation "Superclass for objects backed by a JSON structure."))

(defun init-json-fields (obj definitions)
  (let ((data (json-instance-data obj)))
    (mapcar #'(lambda (v)
                (destructuring-bind (slot data-value &optional parse-function) v
                  (let ((v (cdr (assoc data-value data))))
                    (setf (slot-value obj slot) (if parse-function (funcall parse-function v) v)))))
            definitions)))

(defun load-and-parse-json (url &key (session *gdata-session*))
  (http-request-with-stream url
                            #'(lambda (s receieved-headers code)
                                (declare (ignore receieved-headers code))
                                (let ((char-stream (flexi-streams:make-flexi-stream s :external-format :utf8)))
                                  (json:decode-json char-stream)))
                            :session session
                            :version "3.0"))
