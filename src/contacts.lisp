(in-package :cl-gdata)

(declaim #.*compile-decl*)

(defclass contact (node-dom-mixin)
  ((title       :type string
                :reader contact-title
                :documentation "Content of the <title> node")
   (full-name   :type (or null string)
                :initform nil
                :reader contact-full-name
                :documentation "Content of the <gd:name><gd:fullName> node")
   (given-name  :type (or null string)
                :initform nil
                :reader contact-full-name
                :documentation "Content of the <gd:name><gd:givenName> node")
   (family-name :type (or null string)
                :initform nil
                :reader contact-full-name
                :documentation "Content of the <gd:name><gd:familyName> node")
   (email       :type list
                :initform nil
                :reader contact-email
                :documentation "Alist of email addresses")))

(defmethod print-object ((obj contact) out)
  (print-unreadable-safely (title) obj out
    (format out "~s" title)))

(defun %collect-rel (node path attribute)
  (xpath:map-node-set->list #'(lambda (n)
                                (list (dom:get-attribute n "rel")
                                      (dom:get-attribute n attribute)))
                            (xpath:evaluate path node)))

(defmethod initialize-instance :after ((obj contact) &key node-dom &allow-other-keys)
  (with-slots (title email) obj
    (with-gdata-namespaces
      (setf title (text-from-xpath node-dom "atom:title"))
      (setf email (%collect-rel node-dom "gd:email" "address")))))

(defun list-all-contacts (&key (session *gdata-session*) username)
  (let ((doc (load-and-parse (format nil
                                     "https://www.google.com/m8/feeds/contacts/~a/full"
                                     (if username (url-rewrite:url-encode username) "default"))
                             :session session)))
    (with-gdata-namespaces
      (xpath:map-node-set->list #'(lambda (n)
                                    (make-instance 'contact :node-dom n))
                                (xpath:evaluate "/atom:feed/atom:entry" doc)))))

