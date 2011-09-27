(in-package :cl-gdata)

(declaim #.*compile-decl*)

(define-constant +EMAIL-TAG-HOME+ "http://schemas.google.com/g/2005#home")
(define-constant +EMAIL-TAG-WORK+ "http://schemas.google.com/g/2005#work")
(define-constant +EMAIL-TAG-OTHER+ "http://schemas.google.com/g/2005#other")

(define-constant +PHONE-TAG-MOBILE+ "http://schemas.google.com/g/2005#mobile")
(define-constant +PHONE-TAG-HOME+ "http://schemas.google.com/g/2005#home")

(defclass contact (node-dom-mixin)
  ((title        :type string
		 :reader contact-title
		 :documentation "Content of the <title> node")
   (full-name    :type (or null string)
		 :initform nil
		 :reader contact-full-name
		 :documentation "Content of the <gd:name><gd:fullName> node")
   (given-name   :type (or null string)
		 :initform nil
		 :reader contact-given-name
		 :documentation "Content of the <gd:name><gd:givenName> node")
   (family-name  :type (or null string)
		 :initform nil
		 :reader contact-family-name
		 :documentation "Content of the <gd:name><gd:familyName> node")
   (email        :type list
		 :initform nil
		 :reader contact-email
		 :documentation "Alist of email addresses")
   (phone-number :type list
		 :initform nil
		 :reader contact-phone-number
		 :documentation "Alist of phone numbers")))

(defmethod print-object ((obj contact) out)
  (print-unreadable-safely (title) obj out
    (format out "~s" title)))

(defun %collect-rel (node path reader)
  (xpath:map-node-set->list #'(lambda (n)
                                (list (dom:get-attribute n "rel")
                                      (funcall reader n)))
                            (xpath:evaluate path node)))

(defmethod initialize-instance :after ((obj contact) &key node-dom &allow-other-keys)
  (with-slots (title full-name given-name family-name email phone-number) obj
    (with-gdata-namespaces
      (setf title (text-from-xpath node-dom "atom:title"))
      (setf full-name (text-from-xpath node-dom "gd:name/gd:fullName"))
      (setf given-name (text-from-xpath node-dom "gd:name/gd:givenName"))
      (setf family-name (text-from-xpath node-dom "gd:name/gd:familyName"))
      (setf email (%collect-rel node-dom "gd:email" #'(lambda (n) (dom:get-attribute n "address"))))
      (setf phone-number (%collect-rel node-dom "gd:phoneNumber" #'get-text-from-node)))))

(defun list-all-contacts (&key (session *gdata-session*) username)
  (let ((doc (load-and-parse (format nil "https://www.google.com/m8/feeds/contacts/~a/full"
                                     (or username (url-rewrite:url-encode username) "default"))
                             :session session)))
    ;;    (dom:map-document (cxml:make-character-stream-sink *standard-output*) doc)
    (with-gdata-namespaces
      (xpath:map-node-set->list #'(lambda (n)
                                    (make-instance 'contact :node-dom n))
                                (xpath:evaluate "/atom:feed/atom:entry" doc)))))

