(in-package :cl-gdata)

(declaim #.*compile-decl*)

(defmacro with-gdata-namespaces (&body body)
  `(xpath:with-namespaces (("atom" "http://www.w3.org/2005/Atom")
                           ("gd" "http://schemas.google.com/g/2005")
                           ("docs" "http://schemas.google.com/docs/2007"))
     ,@body))

(defun load-and-parse (url &key (session *gdata-session*))
  (multiple-value-bind (stream code received-headers original-url reply-stream should-close reason)
      (authenticated-request url :want-stream t)
    (declare (ignore received-headers original-url reply-stream reason))
    (if (/= code 200)
        nil
        (unwind-protect
             (let ((result (cxml:parse-stream stream (cxml-dom:make-dom-builder))))
               result)
          (when should-close (close stream))))))

(defun list-documents (&key (session *gdata-session*))
  (load-and-parse "https://docs.google.com/feeds/default/private/full"))
