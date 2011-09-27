(in-package :cl-gdata)

(declaim #.*compile-decl*)

(defclass album (atom-feed-entry)
  ((summary :type string
            :reader album-summary
            :documentation "The content of the <atom:summary> node"))
  (:documentation "Class that represents a single album"))

(defmethod initialize-instance :after ((obj album) &key node-dom &allow-other-keys)
  (with-slots (title summary) obj
    (with-gdata-namespaces
      (setf summary (text-from-xpath node-dom "atom:summary")))))

(defmethod print-object ((obj album) out)
  (print-unreadable-safely (title) obj out
    (format out "~s" title)))

(defun list-all-albums (&key (session *gdata-session*) username)
  (let ((doc (load-and-parse (format nil "https://picasaweb.google.com/data/feed/api/user/~a"
                                     (if username (url-rewrite:url-encode username) "default"))
                             :session session)))
    (with-gdata-namespaces
      (xpath:map-node-set->list #'(lambda (n)
                                    (make-instance 'album :node-dom n))
                                (xpath:evaluate "/atom:feed/atom:entry" doc)))))
