(in-package :cl-gdata-docs-list)

(declaim #.cl-gdata::*compile-decl*)

(defclass access (atom-feed-entry)
  ((role  :type string
          :reader access-role
          :node "gAcl:role/@value"
          :documentation "The access role.")
   (scope :type list
          :reader access-scope
          :node ("gAcl:scope" "@type" "@value" "@name")
          :documentation "A list of three elements representing the values of the gAcl:scope node.
The elements in the list are the content of the type, value and name attributes."))
  (:metaclass atom-feed-entry-class))

(defmethod print-object ((obj access) out)
  (print-unreadable-safely (role) obj out
    (format out "~s ~s" (feed-entry-title obj) role)))

(defun list-doc-acls (document)
  (with-gdata-namespaces
    (let ((url (value-by-xpath (format nil "gd:feedLink[@rel='~a']/@href" +DOCS-ACCESS-CONTROL-FEED+)
                               (node-dom document))))
      (load-atom-feed-url url 'access))))
