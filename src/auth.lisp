(in-package :cl-gdata)

(declaim #.*compile-decl*)

(defvar *verbose-http-errors* nil)

(defvar *gdata-session* nil
  "The last authenticated session. Used as a default for gdata functions.")

(defgeneric authenticated-request (url session &key method parameters content want-stream content-type additional-headers)
  (:documentation "Performs an authenticated request to the Google services"))

(defmethod authenticated-request (url session &key &allow-other-keys)
  (error "No handler available for session type ~s" (type-of session)))

(defun load-and-parse (url &key
                       (session *gdata-session*) (method :get) (content-type nil)
                       (content nil) (additional-headers nil))
  (multiple-value-bind (stream code received-headers original-url reply-stream should-close reason)
      (authenticated-request url session
                             :want-stream t
                             :session session
                             :method method
                             :content-type content-type
                             :content content
                             :additional-headers additional-headers)
    (declare (ignore received-headers original-url reply-stream))
    (when (/= code 200)
      (when *verbose-http-errors*
        (format *debug-io* "~&====== ERROR OUTPUT ======~%")
        (let ((input (flexi-streams:make-flexi-stream stream :external-format :UTF8 :element-type 'character)))
          (loop
             with s
             while (setq s (read-line input nil nil))
             do (format *debug-io* "~a" s)))
        (format *debug-io* "~&====== END OF ERROR OUTPUT ======~%"))
      (error "Failed to load document. code=~s reason=~s" code reason))
    (unwind-protect
         (let ((result (cxml:parse-stream stream (cxml-dom:make-dom-builder))))
           result)
      (when should-close (close stream)))))
