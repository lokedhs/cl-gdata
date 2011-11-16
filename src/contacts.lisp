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

(defun update-email (node entry)
  (let ((email-node (dom:create-element-ns (dom:owner-document node) (find-namespace-url "gd") "email")))
    (destructuring-bind (rel address primary) entry
      (dom:set-attribute email-node "rel" rel)
      (dom:set-attribute email-node "address" address)
      (when primary
        (dom:set-attribute email-node "primary" "true"))
      (dom:append-child node email-node))))

(defclass contact (atom-feed-entry)
  ((full-name    :type (or null string)
		 :reader contact-full-name
                 :node "gd:name/gd:fullName/text()"
		 :documentation "Content of the <gd:name><gd:fullName> node")
   (given-name   :type (or null string)
		 :accessor contact-given-name
                 :node "gd:name/gd:givenName/text()"
		 :documentation "Content of the <gd:name><gd:givenName> node")
   (family-name  :type (or null string)
		 :reader contact-family-name
                 :node "gd:name/gd:familyName/text()"
		 :documentation "Content of the <gd:name><gd:familyName> node")
   (email        :type list
		 :accessor contact-email
                 :node ("gd:email" "@rel" "@address" ("@primary" :true-false))
                 :node-collectionp t
                 :node-clear-function clear-email
                 :node-updater-function update-email
		 :documentation "Alist of email addresses")
   (phone-number :type list
		 :reader contact-phone-number
                 :node ("gd:phoneNumber" "@rel" "text()")
                 :node-collectionp t
		 :documentation "Alist of phone numbers"))
  (:metaclass atom-feed-entry-class))

(defun list-all-contacts (&key (session *gdata-session*) username)
  (load-atom-feed-url (format nil "https://www.google.com/m8/feeds/contacts/~a/full"
                              (if username (url-rewrite:url-encode username) "default"))
                      'contact
                      :session session))

#|
Test code:

(with-gdata-namespaces
                 (let ((doc (cxml-dom:create-document)))
                   (let ((root-node (dom:create-element-ns doc (find-namespace-url "atom") "root-node"))
                         (entry-node (dom:import-node doc (feed-entry-node-dom *contact*) t)))
                     (dom:append-child root-node entry-node)
                     (dom:append-child doc root-node)
                     (dom:map-document (cxml:make-character-stream-sink *standard-output*) doc))))

With header copy:

(with-gdata-namespaces
                 (let ((doc (cxml-dom:create-document)))
                   (let ((root-node (dom:create-element-ns doc (find-namespace-url "atom") "root-node"))
                         (entry-node (dom:import-node doc (feed-entry-node-dom *contact*) t)))
                     (dom:append-child root-node entry-node)
                     (dom:append-child doc root-node)
                     (dom:map-node-map #'(lambda (a) (dom:set-attribute (dom:document-element doc)
                                                                      (dom:node-name a)
                                                                      (dom:node-value a)))
                                     (dom:attributes (dom:document-element (dom:owner-document *n*))))
                     (dom:map-document (cxml:make-character-stream-sink *standard-output*) doc))))
|#
