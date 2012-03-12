(in-package :cl-gdata-misc)

(declaim #.cl-gdata::*compile-decl*)

(defun load-and-parse-json (url)
  (http-request-with-stream url
                            #'(lambda (s receieved-headers code)
                                (declare (ignore receieved-headers code))
                                (let ((char-stream (flexi-streams:make-flexi-stream s :external-format :utf8)))
                                  (json:decode-json char-stream)))))
