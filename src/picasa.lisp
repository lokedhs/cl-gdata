(in-package :cl-gdata)

(declaim #.*compile-decl*)

(defclass album (atom-feed-entry)
  ((summary :type (or null string)
            :reader album-summary
            :node "atom:summary/text()"
            :documentation "The content of the <atom:summary> node"))
  (:documentation "Class that represents a single album")
  (:metaclass atom-feed-entry-class))

(defun list-all-albums (&key (session *gdata-session*) username)
  (load-atom-feed-url (format nil "https://picasaweb.google.com/data/feed/api/user/~a"
                              (if username (url-rewrite:url-encode username) "default"))
                      'album
                      :session session))
