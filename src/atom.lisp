(in-package :cl-gdata-misc)

(declaim #.cl-gdata::*compile-decl*)

(define-constant +ATOM-TAG-FEED+ "http://schemas.google.com/g/2005#feed")
(define-constant +ATOM-TAG-EDIT+ "edit")

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

(defun find-document-feed (document rel type)
  "This version should be eliminated once all existing code has been moved to atom-feed-entry instances"
  (check-type document node-dom-mixin)
  (let ((found-feed (find-if #'(lambda (feed)
                                 (and (equal (car feed) rel) (equal (cadr feed) type)))
                             (document-feeds document))))
    (unless found-feed
      (error "Feed not found. rel=~s type=~s" rel type))
    (caddr found-feed)))

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
   (field-node-type        :initarg :node-type
                           :accessor field-node-type)
   (field-node-default     :initarg :node-default
                           :accessor node-default)
   (field-node-updatable   :initarg :node-updatable
                           :accessor node-updatable)))

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
    (setf (field-node-type result) (ensure-slot-value (car direct-slots) 'field-node-type))
    (setf (node-updatable result) (ensure-slot-value (car direct-slots) 'field-node-updatable))
    result))

(defclass atom-feed-entry ()
  ((feeds        :type list
                 :reader feed-entry-feeds
                 :node ("atom:link" "@rel" "@type" "@href")
                 :node-collectionp t
                 :documentation "List of all link elements")
   (title        :type string
		 :reader feed-entry-title
                 :node "atom:title/text()"
                 :node-default ""
		 :documentation "Content of the <title> node")
   (node-dom     :initarg :node-dom
                 :reader feed-entry-node-dom
                 :documentation "The underlying dom for this node"))
  (:documentation "Common superclass for all Atom feed entries")
  (:metaclass atom-feed-entry-class))

(defgeneric parse-text-value (value typename)
  (:documentation "Converts VALUE to the type TYPE.")
  (:method (value (typename (eql nil)))         value)
  (:method (value (typename (eql :string)))     value)
  (:method (value (typename (eql :number)))     (parse-number:parse-number value))
  (:method (value (typename (eql :true-false))) (cond ((equal value "true") t)
                                                      ((equal value "false") nil)
                                                      (t (error "Unexpected value: ~s" value))))
  (:method (value (typename t)) (error "Illegal type name: ~s" typename)))

(defgeneric update-feed-entry-node (element)
  (:documentation "Update the undelying DOM node to reflect any changes to the entry."))

(defmethod update-feed-entry-node ((element atom-feed-entry))
  (with-gdata-namespaces
    (let ((class (class-of element))
          (node (feed-entry-node-dom element)))
      (dolist (slot (closer-mop:class-slots class))
        (let* ((node-descriptor (field-node slot))
               (collectionp (node-collectionp slot)))
          (cond ((null node-descriptor)
                 nil)
                ((and (stringp node-descriptor) (not collectionp))
                 (setf (dom:node-value (xpath:first-node (xpath:evaluate node-descriptor node)))
                       (closer-mop:slot-value-using-class class element slot)))
                (t
                 (format *debug-io* "~&Unsupported slot format: ~s~%" slot)))))
      node)))
                 

(defun %read-subpaths (pathlist node)
  (mapcar #'(lambda (descriptor)
              ;; The subpath can either be a string designating an xpath that specified string data,
              ;; or a list with two elements, an xpath and a type designator
              (let* ((path (if (listp descriptor) (car descriptor) descriptor))
                     (type (if (listp descriptor) (cadr descriptor) nil))
                     (n (xpath:first-node (xpath:evaluate path node))))
                (if n
                    (parse-text-value (dom:node-value n) type)
                    nil)))
          pathlist))

(defmethod initialize-instance :after ((obj atom-feed-entry) &key node-dom &allow-other-keys)
  (with-gdata-namespaces
    (let ((class (class-of obj)))
      (dolist (slot (closer-mop:class-slots class))
        (let* ((node-descriptor (field-node slot))
               (collectionp (node-collectionp slot))
               (type (field-node-type slot)))
          (cond ((null node-descriptor)
                 nil)
                ((typep node-descriptor 'string)
                 (let ((nodes (xpath:evaluate node-descriptor node-dom)))
                   (setf (closer-mop:slot-value-using-class class obj slot)
                         (if collectionp
                             (xpath:map-node-set->list #'(lambda (n)
                                                           (parse-text-value (dom:node-value n) type)) nodes)
                             (if (xpath:node-set-empty-p nodes)
                                 (node-default slot)
                                 (parse-text-value (dom:node-value (xpath:first-node nodes)) type))))))
                ((typep node-descriptor 'list)
                 (let ((nodes (xpath:evaluate (car node-descriptor) node-dom)))
                   (setf (closer-mop:slot-value-using-class class obj slot)
                         (if collectionp
                             (xpath:map-node-set->list #'(lambda (n) (%read-subpaths (cdr node-descriptor) n)) nodes)
                             (if (xpath:node-set-empty-p nodes)
                                 (node-default slot)
                                 (%read-subpaths (cdr node-descriptor) (xpath:first-node nodes)))))))
                (t
                 (error "Illegal node format: ~s" node-descriptor))))))))

(defmethod print-object ((obj atom-feed-entry) out)
  (print-unreadable-safely (title) obj out
    (format out "~s" title)))

(defun find-feed-from-atom-feed-entry (entry rel &optional (type +ATOM-XML-MIME-TYPE+))
  (check-type entry atom-feed-entry)
  (let ((found-feed (find-if #'(lambda (feed)
                                 (and (equal (car feed) rel) (equal (cadr feed) type)))
                             (feed-entry-feeds entry))))
    (unless found-feed
      (error "Feed not found. rel=~s type=~s" rel type))
    (caddr found-feed)))

;;;
;;; Loading of feeds
;;;

(defgeneric load-atom-feed (document class-name)
  (:documentation "Loads an atom feed into a list of atom-feed-entry instances"))

(defmethod load-atom-feed (document (class symbol))
  (load-atom-feed document (find-class class)))

(defmethod load-atom-feed (document (class atom-feed-entry-class))
  ;;  (dom:map-document (cxml:make-character-stream-sink *standard-output*) document)
  (with-gdata-namespaces
    (xpath:map-node-set->list #'(lambda (n)
                                  (make-instance class :node-dom n))
                              (xpath:evaluate "/atom:feed/atom:entry" document))))

(defun load-atom-feed-url (url class &key (session *gdata-session*) (version "3.0"))
  (load-atom-feed (load-and-parse url :session session :version version) class))
