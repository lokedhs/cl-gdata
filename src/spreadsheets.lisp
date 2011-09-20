(in-package :cl-gdata)

(declaim #.*compile-decl*)

(define-constant +SPREADSHEETS-WORKSHEETSFEED+ "http://schemas.google.com/spreadsheets/2006#worksheetsfeed")
(define-constant +SPREADSHEETS-TABLESFEED+ "http://schemas.google.com/spreadsheets/2006#tablesfeed")

(defclass spreadsheet (document)
  ((worksheets :type (or list (member :unset))
               :initform :unset
               :reader spreadsheet-worksheets
               :documentation "A list of the worksheets in this document, or :unset if the worksheets
has not yet been loaded."))
  (:documentation "Class that manages the content and pending updates to a spreadsheet document."))

(defmethod make-document-from-resource (node (type (eql :spreadsheet)))
  (make-instance 'spreadsheet :node-dom node))

(defclass worksheet ()
  ((spreadsheet  :type spreadsheet
                 :initarg :spreadsheet
                 :initform (error "Can't create a ~s instance without a ~s argument" 'worksheet :spreadsheet)
                 :reader worksheet-spreadsheet
                 :documentation "The spreadsheet this worksheet belongs to")
   (title        :type string
                 :initarg :title
                 :reader worksheet-title
                 :documentation "The worksheet title")
   (row-count    :type (integer 0)
                 :initarg :row-count
                 :reader worksheet-row-count
                 :documentation "The number of rows in the worksheet")
   (column-count :type (integer 0)
                 :initarg :column-count
                 :reader worksheet-column-count
                 :documentation "The number of columns in the worksheet"))
  (:documentation "Class the manages a single worksheet in a spreadsheet document"))

(defmethod print-object ((obj worksheet) out)
  (print-unreadable-safely (title row-count column-count) obj out
    (format out "~s ROWS ~a COLUMNS ~a" title row-count column-count)))

(defun create-worksheet (document-id title rows cols &key (session *gdata-session*))
  (with-gdata-namespaces
    (let ((content (with-output-to-string (s)
                     (build-atom-xml-stream `(("atom" "entry")
                                              (("atom" "title") ,title)
                                              (("gs" "rowCount") ,rows)
                                              (("gs" "colCount") ,cols))
                                            s))))
      (authenticated-request (format nil "https://spreadsheets.google.com/feeds/worksheets/~a/private/full"
                                     (url-rewrite:url-encode document-id))
                             session
                             :method :post
                             :content-type "application/atom+xml"
                             :content content))))

(defun load-worksheets (doc)
  "Loads the worksheets into the document instance. Returns the new worksheets."
  (check-type doc spreadsheet)
  (let ((doc-node (load-and-parse (find-document-feed doc +SPREADSHEETS-WORKSHEETSFEED+ "application/atom+xml"))))
    (with-gdata-namespaces
      (let ((ws-list (xpath:map-node-set->list #'(lambda (node)
                                                   (make-instance 'worksheet
                                                                  :spreadsheet doc
                                                                  :title (text-from-xpath node "atom:title")
                                                                  :row-count (parse-integer (text-from-xpath node "gs:rowCount"))
                                                                  :column-count (parse-integer (text-from-xpath node "gs:colCount"))))
                                               (xpath:evaluate "/atom:feed/atom:entry" doc-node))))
        (setf (slot-value doc 'worksheets) ws-list)
        ws-list))))
