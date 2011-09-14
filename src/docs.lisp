(in-package :cl-gdata)

(declaim #.*compile-decl*)

(defmacro with-gdata-namespaces (&body body)
  `(xpath:with-namespaces (("atom" "http://www.w3.org/2005/Atom")
                           ("gd" "http://schemas.google.com/g/2005")
                           ("docs" "http://schemas.google.com/docs/2007"))
     ,@body))

(defun load-and-parse (url &key (session *gdata-session*))
  (multiple-value-bind (stream code received-headers original-url reply-stream should-close reason)
      (authenticated-request url :want-stream t :session session)
    (declare (ignore received-headers original-url reply-stream))
    (when (/= code 200)
      (error "Failed to load document. code=~s reason=~s" code reason))
    (unwind-protect
         (let ((result (cxml:parse-stream stream (cxml-dom:make-dom-builder))))
           result)
      (when should-close (close stream)))))

(defun get-text-from-node (node)
  (if (null node)
      nil
      (with-output-to-string (s)
        (loop
           for child across (dom:child-nodes node)
           when (dom:text-node-p child)
           do (princ (dom:node-value child) s)))))

(defun make-document-entry (node)
  (with-gdata-namespaces
    (mapcar #'(lambda (suffix)
                (get-text-from-node (xpath:first-node (xpath:evaluate (format nil "~a" suffix) node))))
            '("atom:id" "gd:resourceId" "atom:title" "docs:description" "docs:suggestedFilename" "atom:updated"))))

(defun list-documents (&key (session *gdata-session*))
  (let ((doc (load-and-parse "https://docs.google.com/feeds/default/private/full" :session session)))
    (xpath:map-node-set->list #'(lambda (node)
                                  (make-document-entry node))
                              (with-gdata-namespaces
                                (xpath:evaluate "/atom:feed/atom:entry" doc)))))
