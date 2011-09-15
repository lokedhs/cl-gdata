(in-package :cl-gdata)

(declaim #.*compile-decl*)

(defclass document ()
  ((id                 :type string
                       :initarg :id
                       :reader document-id)
   (resource-id        :type string
                       :initarg :resource-id
                       :reader document-resource-id)
   (title              :type string
                       :initarg :title
                       :reader document-title)
   (description        :type (or null string)
                       :initarg :description
                       :reader document-description)
   (suggested-filename :type (or null string)
                       :initarg :suggested-filename
                       :reader document-suggested-filename)
   (updated            :type string
                       :initarg :updated
                       :reader document-updated)
   (feeds              :type list
                       :initarg :feeds
                       :reader document-feeds
                       :documentation "A list of links from this document.
Each entry is a list of the three attributes in a \"link\"
node: \"rel\", \"type\", \"href\".")))

(defmethod print-object ((obj document) out)
  (print-unreadable-safely (title updated) obj out
    (format out "~s UPDATED ~a" title updated)))

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
    (labels ((text (path)
               (get-text-from-node (xpath:first-node (xpath:evaluate path node)))))
      (let ((feeds (xpath:map-node-set->list #'(lambda (n)
                                                 (list (dom:get-attribute n "rel")
                                                       (dom:get-attribute n "type")
                                                       (dom:get-attribute n "href")))
                                             (xpath:evaluate "atom:link" node))))
        (make-instance 'document
                       :id (text "atom:id")
                       :resource-id (text "gd:resourceId")
                       :title (text "atom:title")
                       :description (text "docs:description")
                       :suggested-filename (text "docs:suggestedFilename")
                       :updated (text "atom:updated")
                       :feeds feeds)))))

(defun list-documents (&key (session *gdata-session*))
  (let ((doc (load-and-parse "https://docs.google.com/feeds/default/private/full" :session session)))
    (xpath:map-node-set->list #'(lambda (node)
                                  (make-document-entry node))
                              (with-gdata-namespaces
                                (xpath:evaluate "/atom:feed/atom:entry" doc)))))
