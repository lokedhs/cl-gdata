(in-package :cl-gdata-misc)

(declaim #.cl-gdata::*compile-decl*)

(defclass json-instance ()
  ((data :type list
         :initarg :data
         :reader json-instance-data))
  (:documentation "Superclass for objects backed by a JSON structure."))

(defun init-json-fields (obj definitions)
  (format t "defs=~s~%" definitions)
  (let ((data (json-instance-data obj)))
    (mapcar #'(lambda (v)
                (destructuring-bind (slot data-value &optional parse-function) v
                  (let ((v (cdr (assoc data-value data))))
                    (setf (slot-value obj slot) (if parse-function (funcall parse-function v) v)))))
            definitions)))

(defun load-and-parse-json (url)
  (http-request-with-stream url
                            #'(lambda (s receieved-headers code)
                                (declare (ignore receieved-headers code))
                                (let ((char-stream (flexi-streams:make-flexi-stream s :external-format :utf8)))
                                  (json:decode-json char-stream)))
                            :version "3.0"))
