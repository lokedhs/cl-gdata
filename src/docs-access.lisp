(in-package :cl-gdata-docs-list)

(declaim #.cl-gdata::*compile-decl*)

(defclass access (atom-feed-entry)
  ()
  (:metaclass atom-feed-entry-class))

(defun list-doc-acls (document)
  (with-gdata-namespaces
    (let ((url (value-by-xpath (format nil "gd:feedLink[@rel='~a']/@href" +DOCS-ACCESS-CONTROL-FEED+)
                               (feed-entry-node-dom document))))
      (load-feed url 'access))))
