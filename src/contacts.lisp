(in-package :cl-gdata)

(declaim #.*compile-decl*)

(define-constant +EMAIL-TAG-HOME+ "http://schemas.google.com/g/2005#home")
(define-constant +EMAIL-TAG-WORK+ "http://schemas.google.com/g/2005#work")
(define-constant +EMAIL-TAG-OTHER+ "http://schemas.google.com/g/2005#other")

(define-constant +PHONE-TAG-MOBILE+ "http://schemas.google.com/g/2005#mobile")
(define-constant +PHONE-TAG-HOME+ "http://schemas.google.com/g/2005#home")

(defclass contact (atom-feed-entry)
  ((full-name    :type (or null string)
		 :initform nil
		 :reader contact-full-name
                 :node "gd:name/gd:fullName/text()"
		 :documentation "Content of the <gd:name><gd:fullName> node")
   (given-name   :type (or null string)
		 :initform nil
		 :reader contact-given-name
                 :node "gd:name/gd:givenName/text()"
		 :documentation "Content of the <gd:name><gd:givenName> node")
   (family-name  :type (or null string)
		 :initform nil
		 :reader contact-family-name
                 :node "gd:name/gd:familyName/text()"
		 :documentation "Content of the <gd:name><gd:familyName> node")
   (email        :type list
		 :initform nil
		 :reader contact-email
                 :node ("gd:email" "@rel" "@address")
		 :documentation "Alist of email addresses")
   (phone-number :type list
		 :initform nil
		 :reader contact-phone-number
		 :documentation "Alist of phone numbers"))
  (:metaclass atom-feed-entry-class))

(defun %collect-rel (node path reader)
  (xpath:map-node-set->list #'(lambda (n)
                                (list (dom:get-attribute n "rel")
                                      (funcall reader n)))
                            (xpath:evaluate path node)))

(defmethod initialize-instance :after ((obj contact) &key node-dom &allow-other-keys)
  (with-slots (full-name given-name family-name email phone-number) obj
    (with-gdata-namespaces
      ;;      (setf email (%collect-rel node-dom "gd:email" #'(lambda (n) (dom:get-attribute n "address"))))
      (setf phone-number (%collect-rel node-dom "gd:phoneNumber" #'get-text-from-node)))))

(defun list-all-contacts (&key (session *gdata-session*) username)
  (load-atom-feed-url (format nil "https://www.google.com/m8/feeds/contacts/~a/full"
                              (if username (url-rewrite:url-encode username) "default"))
                      'contact
                      :session session))
