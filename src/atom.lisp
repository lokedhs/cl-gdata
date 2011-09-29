(in-package :cl-gdata)

(declaim #.*compile-decl*)

(define-constant +ATOM-XML-MIME-TYPE+ "application/atom+xml")

(defclass node-dom-mixin ()
  ((feeds    :type list
             :initarg :feeds
             :reader document-feeds
             :documentation "A list of links from this document.
Each entry is a list of the three attributes in a \"link\"
node: \"rel\", \"type\", \"href\".")
   (node-dom :initarg :node-dom
             :reader document-node-dom
             :documentation "The DOM node that was used to initialise this document")))

(defmethod initialize-instance :after ((node node-dom-mixin) &key node-dom &allow-other-keys)
  (when node-dom
    (with-slots (feeds) node
      (with-gdata-namespaces
        (setf feeds (xpath:map-node-set->list #'(lambda (n)
                                                  (list (dom:get-attribute n "rel")
                                                        (dom:get-attribute n "type")
                                                        (dom:get-attribute n "href")))
                                              (xpath:evaluate "atom:link" node-dom)))))))

;;;
;;; MOP stuff
;;;

(defclass atom-feed-entry-class (standard-class)
  ()
  (:documentation "Metaclass for atom feed entry classes."))

(defmethod closer-mop:validate-superclass ((class atom-feed-entry-class) (superclass standard-object))
  t)

(defclass atom-feed-entry-slot-definition-mixin ()
  ((field-node             :initarg :node
                           :accessor field-node)
   (field-node-collectionp :initarg :node-collectionp
                           :accessor node-collectionp)
   (field-node-default     :initarg :node-default
                           :accessor node-default)))

(defclass atom-feed-entry-direct-slot-definition (atom-feed-entry-slot-definition-mixin
                                                  closer-mop:standard-direct-slot-definition)
  ())

(defclass atom-feed-entry-effective-slot-definition (atom-feed-entry-slot-definition-mixin
                                                     closer-mop:standard-effective-slot-definition)
  ())

(defmethod closer-mop:direct-slot-definition-class ((class atom-feed-entry-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'atom-feed-entry-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class atom-feed-entry-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'atom-feed-entry-effective-slot-definition))

(defun ensure-slot-value (instance field-name &optional default-value)
  "Returns the value of slot FIELD-NAME in INSTANCE. If the slot is unbound, return DEFAULT-VALUE."
  (if (and (slot-exists-p instance field-name)
           (slot-boundp instance field-name))
      (slot-value instance field-name)
      default-value))

(defmethod closer-mop:compute-effective-slot-definition ((class atom-feed-entry-class) slot-name direct-slots)
  (let ((result (call-next-method)))
    (setf (field-node result) (ensure-slot-value (car direct-slots) 'field-node))
    (setf (node-collectionp result) (ensure-slot-value (car direct-slots) 'field-node-collectionp))
    (setf (node-default result) (ensure-slot-value (car direct-slots) 'field-node-default))
    result))

(defclass atom-feed-entry ()
  ((title        :type string
		 :reader feed-entry-title
                 :node "atom:title/text()"
                 :node-default ""
		 :documentation "Content of the <title> node"))
  (:documentation "Common superclass for all Atom feed entries")
  (:metaclass atom-feed-entry-class))

;(defun destructure-paths (descriptor node)
;  (

(defmethod initialize-instance :after ((obj atom-feed-entry) &key node-dom &allow-other-keys)
  (with-gdata-namespaces
    (let ((class (class-of obj)))
      (dolist (slot (closer-mop:class-slots class))
        (let* ((node-descriptor (field-node slot))
               (collectionp (node-collectionp slot))
               (node-name (etypecase node-descriptor
                              (list (car node-descriptor))
                              (string node-descriptor))))
          (when node-name
            (let ((nodes (xpath:evaluate node-name node-dom)))
              (setf (closer-mop:slot-value-using-class class obj slot)
                    (if collectionp
                        (xpath:map-node-set->list #'(lambda (n) (dom:node-value n)) nodes)
                        (if (xpath:node-set-empty-p nodes)
                            (node-default slot)
                            (dom:node-value (xpath:first-node nodes))))))))))))

(defmethod print-object ((obj atom-feed-entry) out)
  (print-unreadable-safely (title) obj out
    (format out "~s" title)))

(defun find-document-feed (document rel type)
  (check-type document node-dom-mixin)
  (let ((found-feed (find-if #'(lambda (feed)
                                 (and (equal (car feed) rel) (equal (cadr feed) type)))
                             (document-feeds document))))
    (unless found-feed
      (error "Feed not found. rel=~s type=~s" rel type))
    (caddr found-feed)))

(defgeneric load-atom-feed (document class-name)
  (:documentation "Loads an atom feed into a list of atom-feed-entry instances"))

(defmethod load-atom-feed (document (class symbol))
  (load-atom-feed document (find-class class)))

(defmethod load-atom-feed (document (class atom-feed-entry-class))
  (with-gdata-namespaces
    (xpath:map-node-set->list #'(lambda (n)
                                  (make-instance class :node-dom n))
                              (xpath:evaluate "/atom:feed/atom:entry" document))))

(defun load-atom-feed-url (url class &key (session *gdata-session*))
  (load-atom-feed (load-and-parse url :session session) class))
