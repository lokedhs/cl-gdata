(in-package :cl-gdata-contacts)

(declaim #.cl-gdata::*compile-decl*)

(define-constant +EMAIL-TAG-HOME+ "http://schemas.google.com/g/2005#home")
(define-constant +EMAIL-TAG-WORK+ "http://schemas.google.com/g/2005#work")
(define-constant +EMAIL-TAG-OTHER+ "http://schemas.google.com/g/2005#other")

(define-constant +PHONE-TAG-MOBILE+ "http://schemas.google.com/g/2005#mobile")
(define-constant +PHONE-TAG-HOME+ "http://schemas.google.com/g/2005#home")

(defclass contact (atom-feed-entry)
  ((full-name    :type (or null string)
		 :reader contact-full-name
                 :node "gd:name/gd:fullName/text()"
                 :node-updatable t
		 :documentation "Content of the <gd:name><gd:fullName> node")
   (given-name   :type (or null string)
		 :accessor contact-given-name
                 :node "gd:name/gd:givenName/text()"
                 :node-updatable t
		 :documentation "Content of the <gd:name><gd:givenName> node")
   (family-name  :type (or null string)
		 :reader contact-family-name
                 :node "gd:name/gd:familyName/text()"
                 :node-updatable t
		 :documentation "Content of the <gd:name><gd:familyName> node")
   (email        :type list
		 :reader contact-email
                 :node ("gd:email" "@rel" "@address")
                 :node-collectionp t
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
