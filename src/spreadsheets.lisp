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
   (cells        :type array
                 :documentation "The content of the worksheet"))
  (:documentation "Class the manages a single worksheet in a spreadsheet document"))

(defmethod print-object ((obj worksheet) out)
  (print-unreadable-safely (title) obj out
    (format out "~s" title)))

(defmethod initialize-instance :after ((sheet worksheet) &rest initargs &key row-count column-count &allow-other-keys)
  (declare (ignore initargs))
  (setf (slot-value sheet 'cells) (make-array (list row-count column-count) :adjustable t :initial-element :unset)))

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

;;;
;;; The repeat of the max dimension calculation below is a bit ugly. I suppose
;;; it would work if I simply replaced the expression in declaration below
;;; with ARRAY-DIMENSION-LIMIT, as it would be adjusted in the LET form below.
;;;
(defun load-cell-range (worksheet &key 
                        (min-col 0) (max-col (1- (array-dimension (slot-value worksheet 'cells) 0)))
                        (min-row 0) (max-row (1- (array-dimension (slot-value worksheet 'cells) 1))))
  "Loads the specified cell range into the worksheet."
  (check-type worksheet worksheet)
  (let ((x1 (max min-col 0))
        (x2 (min max-col (1- (array-dimension (slot-value worksheet 'cells) 0))))
        (y1 (max min-row 0))
        (y2 (min max-row (1- (array-dimension (slot-value worksheet 'cells) 1)))))
    (format t "will load (~a,~a) (~a,~a)~%" x1 x2 y1 y2)))
