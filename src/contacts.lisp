(in-package :cl-gdata)

(declaim #.*compile-decl*)

(defun list-all-contacts (&key (session *gdata-session*) username)
  (let ((doc (load-and-parse (format nil
                                     "https://www.google.com/m8/feeds/contacts/~a/full"
                                     (if username (url-rewrite:url-encode username) "default"))
                             :session session)))
    (dom:map-document (cxml:make-character-stream-sink *standard-output*) doc)))
