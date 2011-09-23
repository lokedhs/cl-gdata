(in-package :cl-gdata)

(declaim #.*compile-decl*)

(define-constant +ATOM-XML-MIME-TYPE+ "application/atom+xml")
(define-constant +DOCS-THUMBNAIL+ "http://schemas.google.com/docs/2007/thumbnail")

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

(defmethod initialize-instance :after ((node node-dom-mixin) &rest initargs &key node-dom &allow-other-keys)
  (declare (ignore initargs))
  (when node-dom
    (with-slots (feeds) node
      (with-gdata-namespaces
        (setf feeds (xpath:map-node-set->list #'(lambda (n)
                                                  (list (dom:get-attribute n "rel")
                                                        (dom:get-attribute n "type")
                                                        (dom:get-attribute n "href")))
                                              (xpath:evaluate "atom:link" node-dom)))))))

(defclass document (node-dom-mixin)
  ((id-url             :type string
                       :initarg :id
                       :reader document-id-url)
   (resource-id        :type string
                       :initarg :resource-id
                       :reader document-resource-id
                       :documentation "The resource ID of the document, from the <resource-id> node in the XML document.")
   (title              :type string
                       :initarg :title
                       :reader document-title
                       :documentation "The title of the document, from the <title> node in the XML document.")
   (description        :type (or null string)
                       :initarg :description
                       :reader document-description)
   (suggested-filename :type (or null string)
                       :initarg :suggested-filename
                       :reader document-suggested-filename)
   (updated            :type string
                       :initarg :updated
                       :reader document-updated)))

(defmethod print-object ((obj document) out)
  (print-unreadable-safely (title resource-id updated) obj out
    (format out "~s RESOURCE-ID ~s UPDATED ~a" title resource-id updated)))

(defgeneric make-document-from-resource (node resource-type)
  (:documentation "Create a document instance based on a specific resource type")
  (:method (node resource-type)
    (error "Initialisation method for resource type ~s not available" resource-type))
  (:method (node (resource-type (eql :document)))
    (make-instance 'document :node-dom node)))

(defun parse-resource-id (resource-id)
  "Given a document, return the id to be used in document URL's. The second
return value is the document type."
  (values-list (reverse (split-sequence:split-sequence #\: resource-id :count 2))))

(defun find-document-feed (document rel type)
  (check-type document node-dom-mixin)
  (let ((found-feed (find-if #'(lambda (feed)
                                 (and (equal (car feed) rel) (equal (cadr feed) type)))
                             (document-feeds document))))
    (unless found-feed (error "Feed not found. rel=~s type=~s" rel type))
    (caddr found-feed)))

(defmethod initialize-instance :after ((doc document) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (with-gdata-namespaces
    (labels ((text (path)
               (get-text-from-node (xpath:first-node (xpath:evaluate path (document-node-dom doc))))))
      (with-slots (id-url resource-id title description suggested-filename updated feeds node-dom) doc
        (setf id-url             (text "atom:id"))
        (setf resource-id        (text "gd:resourceId"))
        (setf title              (text "atom:title"))
        (setf description        (text "docs:description"))
        (setf suggested-filename (text "docs:suggestedFilename"))
        (setf updated            (text "atom:updated"))))))

(defun document-type-name-to-identifier (name)
  "Converts the type name from the resource id to an identifier.
Currently, this is done by simply upcasing the name and interning
it into the KEYWORD package."
  (intern (string-upcase name) "KEYWORD"))

(defun make-document-entry (node)
  (with-gdata-namespaces
    (let* ((resource-id (get-text-from-node (xpath:first-node (xpath:evaluate "gd:resourceId" node))))
           (type (nth-value 1 (parse-resource-id resource-id))))
      (make-document-from-resource node (document-type-name-to-identifier type)))))

(defun list-documents (&key (session *gdata-session*))
  (let ((doc (load-and-parse "https://docs.google.com/feeds/default/private/full" :session session)))
    ;;    (dom:map-document (cxml:make-character-stream-sink *standard-output*) doc)
    (with-gdata-namespaces
      (xpath:map-node-set->list #'(lambda (node)
                                    (make-document-entry node))
                                (xpath:evaluate "/atom:feed/atom:entry" doc)))))
