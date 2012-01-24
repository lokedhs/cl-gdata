(in-package :cl-gdata-docs-list)

(declaim #.cl-gdata::*compile-decl*)

(define-constant +DOCS-THUMBNAIL+ "http://schemas.google.com/docs/2007/thumbnail")

(defclass document (atom-feed-entry)
  ((id-url             :type string
                       :reader document-id-url
                       :node "atom:id/text()")
   (resource-id        :type string
                       :reader document-resource-id
                       :documentation "The resource ID of the document, from the <resource-id> node in the XML document."
                       :node "gd:resourceId/text()")
   (description        :type (or null string)
                       :reader document-description
                       :node "docs:description/text()")
   (suggested-filename :type (or null string)
                       :reader document-suggested-filename
                       :node "docs:suggestedFilename/text()")
   (updated            :type string
                       :reader document-updated
                       :node "atom:updated/text()")
   (content            :type list
                       :reader document-content
                       :node ("atom:content" "@type" "@src")
                       :node-collectionp t))
  (:metaclass atom-feed-entry-class))

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
  "List all the documents that belongs to the authenticated user"
  (let ((doc (load-and-parse "https://docs.google.com/feeds/default/private/full" :session session)))
    (with-gdata-namespaces
      (xpath:map-node-set->list #'make-document-entry (xpath:evaluate "/atom:feed/atom:entry" doc)))))
