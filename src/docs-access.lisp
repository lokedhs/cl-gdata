(in-package :cl-gdata-docs-list)

(declaim #.cl-gdata::*compile-decl*)

(defclass access (atom-feed-entry)
  ((role :type (member :reader :writer :owner)
         :reader access-role
         :documentation "The access role. Either :READER which means read-only access,
:WRITER which means full read and write access, or :OWNER which indicates
that the user is the owner of the document."))
  (:metaclass atom-feed-entry-class))

(defmethod initialize-instance :after ((obj access) &key &allow-other-keys)
  (let ((role (value-by-xpath "gAcl:role/@value" (feed-entry-node-dom obj))))
    (setf (slot-value obj 'role) (string-case:string-case (role)
                                   ("reader" :reader)
                                   ("writer" :writer)
                                   ("owner" :owner)
                                   (t (error "Unknown access type: ~s" role))))))

(defmethod print-object ((obj access) out)
  (print-unreadable-safely (role) obj out
    (format out "~s ~s" (feed-entry-title obj) role)))

(defun list-doc-acls (document)
  (with-gdata-namespaces
    (let ((url (value-by-xpath (format nil "gd:feedLink[@rel='~a']/@href" +DOCS-ACCESS-CONTROL-FEED+)
                               (feed-entry-node-dom document))))
      (load-atom-feed-url url 'access))))
