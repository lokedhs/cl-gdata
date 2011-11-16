(in-package :cl-gdata-contacts)

(declaim #.cl-gdata::*compile-decl*)

(define-constant +EMAIL-TAG-HOME+ "http://schemas.google.com/g/2005#home")
(define-constant +EMAIL-TAG-WORK+ "http://schemas.google.com/g/2005#work")
(define-constant +EMAIL-TAG-OTHER+ "http://schemas.google.com/g/2005#other")

(define-constant +PHONE-TAG-MOBILE+ "http://schemas.google.com/g/2005#mobile")
(define-constant +PHONE-TAG-HOME+ "http://schemas.google.com/g/2005#home")

(defun clear-email (node)
  (with-gdata-namespaces
    (mapc #'(lambda (child-node)
              (dom:remove-child (dom:parent-node child-node) child-node))
          (xpath:map-node-set->list #'identity (xpath:evaluate "gd:email" node)))))

(defun update-email (node entry slot-descriptor)
  (declare (ignore slot-descriptor))
  (let ((email-node (dom:create-element-ns (dom:owner-document node) (find-namespace-url "gd") "email")))
    (destructuring-bind (rel address primary) entry
      (dom:set-attribute email-node "rel" rel)
      (dom:set-attribute email-node "address" address)
      (when primary
        (dom:set-attribute email-node "primary" "true"))
      (dom:append-child node email-node))))

(defclass contact (atom-feed-entry)
  ((full-name    :type (or null string)
		 :accessor full-name
                 :node "gd:name/gd:fullName/text()"
                 :node-updater-function update-from-xpath
		 :documentation "Content of the <gd:name><gd:fullName> node")
   (given-name   :type (or null string)
		 :accessor given-name
                 :node "gd:name/gd:givenName/text()"
                 :node-updater-function update-from-xpath
		 :documentation "Content of the <gd:name><gd:givenName> node")
   (family-name  :type (or null string)
		 :accessor family-name
                 :node "gd:name/gd:familyName/text()"
                 :node-updater-function update-from-xpath
		 :documentation "Content of the <gd:name><gd:familyName> node")
   (email        :type list
		 :accessor email
                 :node ("gd:email" "@rel" "@address" ("@primary" :true-false))
                 :node-collectionp t
                 :node-clear-function clear-email
                 :node-updater-function update-email
		 :documentation "Alist of email addresses")
   (phone-number :type list
		 :reader phone-number
                 :node ("gd:phoneNumber" "@rel" "text()")
                 :node-collectionp t
		 :documentation "Alist of phone numbers"))
  (:documentation "Class that represents a contact element")
  (:metaclass atom-feed-entry-class))

(defun list-all-contacts (&key (session *gdata-session*) username)
  "Return a list of all contacts for the specified user"
  (load-atom-feed-url (format nil "https://www.google.com/m8/feeds/contacts/~a/full"
                              (if username (url-rewrite:url-encode username) "default"))
                      'contact
                      :session session))

(defun %write-doc-to-stream (doc stream)
  (dom:map-document (cxml:make-namespace-normalizer (cxml:make-character-stream-sink stream)) doc))

(defun update-contact (contact &key (session *gdata-session*))
  "Update the remove contact list to reflect any local changes to the contact"
  (let ((doc (cxml-dom:create-document)))
    (dom:append-child doc (cl-gdata-misc::update-feed-entry-node contact doc))
    (let ((result (load-and-parse (find-feed-from-atom-feed-entry contact "edit")
                                  :session session
                                  :method :put
                                  :additional-headers `(("If-Match" . "*")
                                                        ("Content-Type" . ,+ATOM-XML-MIME-TYPE+))
                                  :content #'(lambda (os) (%write-doc-to-stream doc os)))))
      (with-gdata-namespaces
        (make-instance 'contact :node-dom (xpath:first-node (xpath:evaluate "/atom:entry" result)))))))
