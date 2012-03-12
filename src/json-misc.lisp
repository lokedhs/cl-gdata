(in-package :cl-gdata-misc)

(declaim #.cl-gdata::*compile-decl*)

(defclass json-instance ()
  ((data :type list
         :initarg :data
         :reader json-instance-data))
  (:documentation "Superclass for objects backed by a JSON structure."))

(defun init-json-fields (obj definitions)
  (let ((data (json-instance-data obj)))
    (loop
       for (slot data-value) in definitions
       do (setf (slot-value obj slot) (cdr (assoc data-value data))))))

(defun load-and-parse-json (url)
  (http-request-with-stream url
                            #'(lambda (s receieved-headers code)
                                (declare (ignore receieved-headers code))
                                (let ((char-stream (flexi-streams:make-flexi-stream s :external-format :utf8)))
                                  (json:decode-json char-stream)))))
