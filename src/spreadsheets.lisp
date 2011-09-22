(in-package :cl-gdata)

(declaim #.*compile-decl*)

(define-constant +SPREADSHEETS-WORKSHEETSFEED+ "http://schemas.google.com/spreadsheets/2006#worksheetsfeed")
(define-constant +SPREADSHEETS-TABLESFEED+ "http://schemas.google.com/spreadsheets/2006#tablesfeed")
(define-constant +SPREADSHEETS-CELLSFEED+ "http://schemas.google.com/spreadsheets/2006#cellsfeed")

;;;
;;; SPREADSHEET
;;;
(defclass spreadsheet (document)
  ((worksheets :type (or list (member :unset))
               :initform :unset
               :reader spreadsheet-worksheets
               :documentation "A list of the worksheets in this document, or :unset if the worksheets
has not yet been loaded."))
  (:documentation "Class that manages the content and pending updates to a spreadsheet document."))

(defmethod make-document-from-resource (node (type (eql :spreadsheet)))
  (make-instance 'spreadsheet :node-dom node))

;;;
;;; WORKSHEET
;;;
(defclass worksheet (node-dom-mixin)
  ((spreadsheet  :type spreadsheet
                 :initarg :spreadsheet
                 :initform (error "Can't create a ~s instance without a ~s argument" 'worksheet :spreadsheet)
                 :reader worksheet-spreadsheet
                 :documentation "The spreadsheet this worksheet belongs to")
   (title        :type string
                 :initarg :title
                 :reader worksheet-title
                 :documentation "The worksheet title")
   (cells        :type (array (or spreadsheet-cell (member :unset :empty)))
                 :documentation "The content of the worksheet"))
  (:documentation "Class the manages a single worksheet in a spreadsheet document"))

(defmethod print-object ((obj worksheet) out)
  (print-unreadable-safely (title) obj out
    (format out "~s" title)))

(defmethod initialize-instance :after ((sheet worksheet)
                                       &rest initargs
                                       &key node-dom &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (title cells) sheet
    (with-gdata-namespaces
      (setf title (text-from-xpath node-dom "atom:title"))
      (setf cells (make-array (list (parse-integer (text-from-xpath node-dom "gs:rowCount"))
                                    (parse-integer (text-from-xpath node-dom "gs:colCount")))
                              :adjustable t :initial-element :unset)))))

;;;(defun create-worksheet (document-id title rows cols &key (session *gdata-session*))
;;;  (with-gdata-namespaces
;;;    (let ((content (with-output-to-string (s)
;;;                     (build-atom-xml-stream `(("atom" "entry")
;;;                                              (("atom" "title") ,title)
;;;                                              (("gs" "rowCount") ,rows)
;;;                                              (("gs" "colCount") ,cols))
;;;                                            s))))
;;;      (authenticated-request (format nil "https://spreadsheets.google.com/feeds/worksheets/~a/private/full"
;;;                                     (url-rewrite:url-encode document-id))
;;;                             session
;;;                             :method :post
;;;                             :content-type "application/atom+xml"
;;;                             :content content))))

(defun load-worksheets (doc)
  "Loads the worksheets into the document instance. Returns the new worksheets."
  (check-type doc spreadsheet)
  (let ((doc-node (load-and-parse (find-document-feed doc +SPREADSHEETS-WORKSHEETSFEED+ +ATOM-XML-MIME-TYPE+))))
    (with-gdata-namespaces
      (let ((ws-list (xpath:map-node-set->list #'(lambda (node)
                                                   (make-instance 'worksheet
                                                                  :spreadsheet doc
                                                                  :node-dom node))
                                               (xpath:evaluate "/atom:feed/atom:entry" doc-node))))
        (setf (slot-value doc 'worksheets) ws-list)
        ws-list))))

(defun fill-array-slice (array element-value x1 x2 y1 y2)
  (loop
     for y from y1 to y2
     do (loop
           for x from x1 to x2
           do (setf (aref array x y) element-value))))

;;;
;;; The repeat of the max dimension calculation below is a bit ugly. I suppose
;;; it would work if I simply replaced the expression in declaration below
;;; with ARRAY-DIMENSION-LIMIT, as it would be adjusted in the LET form below.
;;;
(defun load-cell-range (worksheet &key
                        (session *gdata-session*)
                        (min-row 0) (max-row (1- (array-dimension (slot-value worksheet 'cells) 0)))
                        (min-col 0) (max-col (1- (array-dimension (slot-value worksheet 'cells) 1))))
  "Loads the specified cell range into the worksheet."
  (check-type worksheet worksheet)
  (with-slots (cells) worksheet
    (let ((x1 (max min-col 0))
          (x2 (min max-col (1- (array-dimension cells 0))))
          (y1 (max min-row 0))
          (y2 (min max-row (1- (array-dimension cells 1)))))
      ;; The gdata feed will only return the cells that actually contain data,
      ;; so we need to mark all the candidate cells as :EMPTY before filling
      ;; in the results
      (fill-array-slice cells :empty x1 x2 y1 y2)
      (let ((node-doc (load-and-parse (format nil "~a?min-row=~a&max-row=~a&min-col=~a&max-col=~a"
                                              (find-document-feed worksheet +SPREADSHEETS-CELLSFEED+ +ATOM-XML-MIME-TYPE+)
                                              (1+ y1) (1+ y2) (1+ x1) (1+ x2))
                                      :session session)))
        (with-gdata-namespaces
          (xpath:map-node-set #'(lambda (n)
                                  (let* ((cell-node (xpath:first-node (xpath:evaluate "gs:cell" n)))
                                         (row (parse-integer (dom:get-attribute cell-node "row")))
                                         (col (parse-integer (dom:get-attribute cell-node "col"))))
                                    (setf (aref cells (1- row) (1- col)) (make-instance 'spreadsheet-cell :node-dom n))))
                              (xpath:evaluate "/atom:feed/atom:entry" node-doc)))))))
