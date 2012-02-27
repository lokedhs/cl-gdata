(in-package :cl-gdata-docs-list)

(declaim #.cl-gdata::*compile-decl*)

(alexandria:define-constant +DOCS-THUMBNAIL+ "http://schemas.google.com/docs/2007/thumbnail" :test 'equal)
(alexandria:define-constant +RESUMABLE-CREATE-MEDIA-REF+ "http://schemas.google.com/g/2005#resumable-create-media" :test 'equal)
(alexandria:define-constant +DOCS-ACCESS-CONTROL-FEED+ "http://schemas.google.com/acl/2007#accessControlList" :test 'equal)

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

(defclass folder (atom-feed-entry)
  ()
  (:metaclass atom-feed-entry-class))

(defclass drawing (atom-feed-entry)
  ()
  (:metaclass atom-feed-entry-class))

(defgeneric make-document-from-resource (node resource-type)
  (:documentation "Create a document instance based on a specific resource type")
  (:method (node resource-type)
    (warn "Initialisation method for resource type ~s not available" resource-type)
    nil)
  (:method (node (resource-type (eql :document)))
    (make-instance 'document :node-dom node))
  (:method (node (resource-type (eql :file)))
    (make-instance 'document :node-dom node))
  (:method (node (resource-type (eql :folder)))
    (make-instance 'folder :node-dom node))
  (:method (node (resource-type (eql :drawing)))
    (make-instance 'drawing :node-dom node)))

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
    (let* ((resource-id (value-by-xpath "gd:resourceId/text()" node))
           (type (nth-value 1 (parse-resource-id resource-id))))
      (make-document-from-resource node (document-type-name-to-identifier type)))))

(defun type-string-for-type (type)
  (ecase type
    (:document "document")
    (:spreadsheet "spreadsheet")
    (:presentation "presentation")
    (:drawing "drawing")
    (:folder "folder")))

(defun list-documents (&key (session *gdata-session*) max-results showfolders type query-string updated-min)
  "List all the documents that belongs to the authenticated user.

:MAX-RESULTS can be set to an integer (up to a maximum of 1000) that limits the number of
returned objects.

If :SHOWFOLDERS is non-NIL, the resulting list will also contain folder objects.
:TYPE can be used to limit the output to a specific type of documents (one of :DOCUMENT,
:SPREADSHEET, :PRESENTATION, :DRAWING or :FOLDER).

If :QUERY-STRING is non-NIL, it is used as a search term.

If given, :UPDATED-MIN indicates the oldest documents that should be included in the
output. The value can be either a universal time value, an a local-time instance,
or a string in standard ISO format."
  (check-type max-results (or null alexandria:non-negative-integer))
  (check-type query-string (or null string))
  (check-type updated-min (or null cl-gdata-date-value))
  (let ((doc (load-and-parse (make-url-search-params (format nil "https://docs.google.com/feeds/default/private/full~a"
                                                             (if type 
                                                                 (format nil "/-/~a" (type-string-for-type type))
                                                                 ""))
                                                     "max-results" max-results
                                                     "showfolders" (when showfolders "true")
                                                     "q" query-string
                                                     "updated-min" (when updated-min (parse-date-string updated-min)))
                             :session session)))
    (with-gdata-namespaces
      (remove nil (xpath:map-node-set->list #'make-document-entry (xpath:evaluate "/atom:feed/atom:entry" doc))))))

(defun copy-stream-with-limit (from to limit)
  "Copies a maximum of LIMIT elements into TO \(a stream) from FROM
\(also a stream) until the end of FROM is reached, in blocks of
8192 elements. The streams should have the same element type."
  (let ((buf (make-array 8192
                         :element-type (stream-element-type from))))
    (loop
       (let* ((n (min (length buf) limit))
              (pos (read-sequence buf from :end n)))
         (when (zerop pos) (return))
         (write-sequence buf to :end pos)
         (decf limit pos)
         (when (zerop limit) (return)))))
  (values))

(defun parse-result-stream (stream)
  (let ((doc (cxml:parse-stream stream (cxml-dom:make-dom-builder))))
    (make-document-entry (xpath:first-node (xpath:evaluate "/atom:entry" doc)))))

(defun %upload-document-send-metadata (stream title description)
  (build-atom-xml-stream `(("atom" "entry")
                           (("atom" "title") ,title)
                           ,@(when description `((("docs" "description") ,description))))
                         stream))

(defun upload-document (file &key title description
                               (session *gdata-session*) (chunk-size (* 512 1024)) (convert nil)
                               (content-type "application/octet-stream") (progress-update nil))
  "Upload a document to Google. TITLE indicates the document name under which the file will
be stored. DESCRIPTION is the description of the file. CHUNK-SIZE indicates the size of
each upload chunk. This value must be a multiple of 512 kB. In non-NIL, CONVERT indicates
that the file should be converted to the apropriate document format. For example, word
processing documents will be converted to an editable Google Docs document.
CONTENT-TYPE specifies the format of the data. If given, PROGRESS-UPDATE will be called
after each chunk has been uploaded. It will be called with one argument, the number of bytes
uploaded."
  (unless (and (plusp chunk-size)
               (zerop (mod chunk-size (* 512 1024))))
    (error "CHUNK-SIZE must be greater than zero and a multiple of 512 kB"))

  (let ((doc (load-and-parse "https://docs.google.com/feeds/default/private/full" :session session)))
    (with-gdata-namespaces                                             

      (let ((upload-url (value-by-xpath (format nil "/atom:feed/atom:link[@rel='~a']/@href" +RESUMABLE-CREATE-MEDIA-REF+) doc)))
        (with-open-file (input-stream file :element-type '(unsigned-byte 8))
          (let ((length (file-length input-stream)))
            (labels ((upload-next-chunk (headers start-offset previous-location)
                       (let ((location (or (cdr (assoc :location headers)) previous-location))
                             (content-length (min (- length start-offset) chunk-size)))
                         (let ((upload-result
                                (http-request-with-stream location
                                                          #'(lambda (result-stream headers code)
                                                              (case code
                                                                (308 (list :upload-next headers))
                                                                (201 (list :upload-done (parse-result-stream result-stream)))))
                                                          :session session
                                                          :method :put
                                                          :version "3.0"
                                                          :content-type content-type
                                                          :content-length content-length
                                                          :content #'(lambda (s) (copy-stream-with-limit input-stream s chunk-size))
                                                          :accepted-status '(201 308)
                                                          :additional-headers `(("Content-Range" . ,(format nil "bytes ~a-~a/~a"
                                                                                                            start-offset
                                                                                                            (1- (+ start-offset content-length))
                                                                                                            length))))))
                           (ecase (car upload-result)
                             (:upload-next
                              (let ((next-offset (+ start-offset chunk-size)))
                                (when progress-update
                                  (funcall progress-update next-offset))
                                (upload-next-chunk (cadr upload-result) next-offset location)) ())
                             (:upload-done
                              (cadr upload-result)))))))

              (let ((upload-result (http-request-with-stream (format nil "~a~a" upload-url (if convert "" "?convert=false"))
                                                             #'(lambda (result-stream headers code)
                                                                 (declare (ignore result-stream code))
                                                                 (list :upload-next headers))
                                                             :session session
                                                             :method :post
                                                             :version "3.0"
                                                             :content-type "application/atom+xml"
                                                             :content #'(lambda (s)
                                                                          (%upload-document-send-metadata s
                                                                                                          (or title
                                                                                                              (name-from-filename file))
                                                                                             description))
                                                             :additional-headers `(("X-Upload-Content-Type" . ,content-type)
                                                                                   ("X-Upload-Content-Length" . ,(princ-to-string length))))))
                (ecase (car upload-result)
                  (:upload-next (upload-next-chunk (cadr upload-result) 0 nil)))))))))))

(defun delete-document (document &key (session *gdata-session*) (delete nil))
    (http-request-with-stream (format nil "~a~a"
                                      (find-feed-from-atom-feed-entry document +ATOM-TAG-EDIT+)
                                      (if delete "?delete=true" ""))
                              #'(lambda (s received code) (declare (ignore s received code)) nil)
                              :session session
                              :method :delete
                              :additional-headers '(("If-Match" . "*"))
                              :version "3.0"))

(defun download-document (document destination &key (session *gdata-session*))
  "Downloads DOCUMENT. DESTINATION is a function which will be called with
an input stream as an argument."
  (with-gdata-namespaces
    (let ((url (dom:get-attribute (xpath:first-node (xpath:evaluate "atom:content[@type='application/atom+xml']"
                                                                    (node-dom document)))
                                  "src")))
      (http-request-with-stream url #'(lambda (s receieved code)
                                        (declare (ignore receieved code))
                                        (funcall destination s))
                                :session session
                                :force-binary t
                                :version "3.0"))))
