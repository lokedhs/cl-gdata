(defpackage :cl-gdata-docs-list
  (:use :cl :cl-gdata :cl-gdata-misc)
  (:export #:+DOCS-THUMBNAIL+
           #:document
           #:document-id-url
           #:document-resource-id
           #:document-title
           #:document-description
           #:document-suggested-filename
           #:document-updated
           #:list-documents
           #:upload-document
           #:delete-document
           #:download-document
           #:list-doc-acls
           #:folder
           #:drawing))
