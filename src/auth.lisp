(in-package :cl-gdata-misc)

(declaim #.cl-gdata::*compile-decl*)

(alexandria:define-constant +HTTP-GDATA-USER-AGENT+ "cl-gdata (gzip)" :test 'equal)

(define-condition authentication-failed (error)
  ()
  (:documentation "Error that is raised when authentication fails."))

(defgeneric authenticated-request (url session
                                       &key method parameters content want-stream
                                       content-type additional-headers user-agent
                                       force-binary)
  (:documentation "Performs an authenticated request to the Google services"))

(defmethod authenticated-request (url session &key &allow-other-keys)
  (error "No handler available for session type ~s" (type-of session)))

(defmethod authenticated-request (url (session null) &key &allow-other-keys)
  (error "Session is NIL. Please set *GDATA-SESSION* to an instance of a session handler."))

(defun display-stream-if-debug (stream)
  (when *verbose-http-errors*
    (format *debug-io* "~&====== ERROR OUTPUT ======~%")
    (let ((input (flexi-streams:make-flexi-stream stream
                                                  :external-format :UTF8
                                                  :element-type 'character)))
      (loop
         for s = (read-line input nil nil)
         while s
         do (format *debug-io* "~a~%" s)))
    (format *debug-io* "~&====== END OF ERROR OUTPUT ======~%")))

(defun http-request-with-stream (url callback &key
                                                (session *gdata-session*) (method :get) (content-type nil)
                                                (content-length nil)
                                                (content nil) (additional-headers nil)
                                                (force-binary nil)
                                                (accepted-status '(200))
                                                (version "2.0"))
  (multiple-value-bind (stream code received-headers original-url reply-stream should-close reason)
      (authenticated-request url session
                             :want-stream t
                             :session session
                             :method method
                             :user-agent +HTTP-GDATA-USER-AGENT+
                             :content-type content-type
                             :content content
                             :content-length content-length
                             :force-binary force-binary
                             :additional-headers (append `(("GData-Version" . ,version)
                                                           ("Accept-Encoding" . "gzip"))
                                                         additional-headers))
    (declare (ignore original-url reply-stream))
    (unwind-protect
         (let ((decoded-stream (if (equal (cdr (assoc :content-encoding received-headers)) "gzip")
                                   (gzip-stream:make-gzip-input-stream stream)
                                   stream)))
           (unless (member code accepted-status)
             (display-stream-if-debug decoded-stream)
             (error "Failed to load document. code=~s reason=~s" code reason))
           (funcall callback decoded-stream received-headers code))
      (when should-close
        (close stream)))))

(defun debug-get-and-display-url (url)
  "Debug function that prints the content of the given URL. Only to be used for testing."
  (http-request-with-stream url
                            #'(lambda (s h c)
                                (format *debug-io* "h=~s~%c=~s~%" h c)
                                (let ((input (flexi-streams:make-flexi-stream s
                                                                              :external-format :UTF8
                                                                              :element-type 'character)))
                                  (loop
                                     for s = (read-line input nil nil)
                                     while s
                                     do (format *debug-io* "~a~%" s)))))  )

(defun load-and-parse (url &key
                       (session *gdata-session*) (method :get) (content-type nil)
                       (content nil) (additional-headers nil) (force-binary nil)
                       (accepted-status '(200)) (version "3.0"))
  (http-request-with-stream url
                            #'(lambda (s received-headers code)
                                (declare (ignore received-headers code))
                                (cxml:parse-stream s (cxml-dom:make-dom-builder)))
                            :session session
                            :method method
                            :content-type content-type
                            :content content
                            :additional-headers additional-headers
                            :force-binary force-binary
                            :accepted-status accepted-status
                            :version version))
