(in-package :cl-gdata-misc)

(declaim #.cl-gdata::*compile-decl*)

(define-constant +HTTP-GDATA-USER-AGENT+ "cl-gdata (gzip)")

(defgeneric authenticated-request (url session
                                       &key method parameters content want-stream
                                       content-type additional-headers user-agent)
  (:documentation "Performs an authenticated request to the Google services"))

(defmethod authenticated-request (url session &key &allow-other-keys)
  (error "No handler available for session type ~s" (type-of session)))

(defun http-request-with-stream (url callback &key
                                 (session *gdata-session*) (method :get) (content-type nil)
                                 (content nil) (additional-headers nil)
                                 (accepted-status '(200)))
  (multiple-value-bind (stream code received-headers original-url reply-stream should-close reason)
      (authenticated-request url session
                             :want-stream t
                             :session session
                             :method method
                             :user-agent +HTTP-GDATA-USER-AGENT+
                             :content-type content-type
                             :content content
                             :additional-headers (append '(("GData-Version" . "3.0")
                                                           ("Accept-Encoding" . "gzip"))
                                                         additional-headers))
    (declare (ignore original-url reply-stream))
    (let ((decoded-stream (if (equal (cdr (assoc :content-encoding received-headers)) "gzip")
                              (gzip-stream:make-gzip-input-stream stream)
                              stream)))
      (unless (find code accepted-status)
        (when *verbose-http-errors*
          (format *debug-io* "~&====== ERROR OUTPUT ======~%")
          (let ((input (flexi-streams:make-flexi-stream decoded-stream
                                                        :external-format :UTF8
                                                        :element-type 'character)))
            (loop
               with s
               while (setq s (read-line input nil nil))
               do (format *debug-io* "~a~%" s)))
          (format *debug-io* "~&====== END OF ERROR OUTPUT ======~%"))
        (error "Failed to load document. code=~s reason=~s" code reason))
      (unwind-protect
           (funcall callback decoded-stream)
        (when should-close (close stream))))))

(defun load-and-parse (url &key
                       (session *gdata-session*) (method :get) (content-type nil)
                       (content nil) (additional-headers nil) (accepted-status '(200)))
  (http-request-with-stream url
                            #'(lambda (s)
                                (let ((result (cxml:parse-stream s (cxml-dom:make-dom-builder))))
                                  result))
                            :session session
                            :method method
                            :content-type content-type
                            :content content
                            :additional-headers additional-headers
                            :accepted-status accepted-status))
