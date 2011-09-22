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
;;; SPREADSHEET-CELL
;;;
(defclass spreadsheet-cell (node-dom-mixin)
  ((id-url          :type string
                    :initarg :id-url
                    :reader cell-id-url
                    :documentation "The ID URL of this cell")
   (input-value     :type string
                    :initarg :input-value
                    :reader cell-input-value
                    :documentation "The value of the <gs:cell inputValue=...> attribute")
   (value           :type string
                    :initarg :value
                    :reader cell-loaded-value
                    :documentation "The content of the <gc:cell> node")
   (numeric-value   :type (or number null)
                    :initarg :numeric-value
                    :reader cell-numeric-value
                    :documentation "The content of the <gs:cell numericValue=...> attribute, or NIL
if the cell does not contain a number")
   (new-input-value :type (or null string)
                    :initform nil
                    :initarg :new-input-value
                    :documentation "The updated value prior to uploading, or NIL if the value is unchanged."))
  (:documentation "Class that describes the content of a single cell"))

(defmethod print-object ((obj spreadsheet-cell) out)
  (print-unreadable-safely (input-value value new-input-value) obj out
    (format out "~s~a~a"
            value
            (if (string= value input-value)
                ""
                (format nil " INPUT-VALUE ~s" input-value))
            (if new-input-value
                (format nil " NEW-INPUT-VALUE ~s" new-input-value)
                ""))))

;;;
;;; WORKSHEET
;;;
(defclass worksheet (node-dom-mixin)
  ((spreadsheet  :type spreadsheet
                 :initarg :spreadsheet
                 :initform (error "Can't create a ~s instance without a ~s argument" 'worksheet :spreadsheet)
                 :reader worksheet-spreadsheet
                 :documentation "The spreadsheet this worksheet belongs to")
   (id-url       :type string
                 :reader worksheet-id-url
                 :documentation "URL to this worksheet")
   (title        :type string
                 :initarg :title
                 :reader worksheet-title
                 :documentation "The worksheet title")
   (cells        :type (array (or spreadsheet-cell (member :unset :empty)))
                 :documentation "The content of the worksheet"))
  (:documentation "Class that manages a single worksheet in a spreadsheet document"))

(defmethod print-object ((obj worksheet) out)
  (print-unreadable-safely (title) obj out
    (format out "~s" title)))

(defmethod initialize-instance :after ((sheet worksheet)
                                       &rest initargs
                                       &key node-dom &allow-other-keys)
  (declare (ignore initargs))
  (with-slots (id-url title cells) sheet
    (with-gdata-namespaces
      (setf id-url (text-from-xpath node-dom "atom:id"))
      (setf title (text-from-xpath node-dom "atom:title"))
      (setf cells (make-array (list (parse-integer (text-from-xpath node-dom "gs:rowCount"))
                                    (parse-integer (text-from-xpath node-dom "gs:colCount")))
                              :adjustable t :initial-element :unset)))))

(defun worksheet-cell-input-value (worksheet row col)
  (check-type worksheet worksheet)
  (let ((cell (aref (slot-value worksheet 'cells) row col)))
    (with-slots (input-value new-input-value) cell
      (or new-input-value input-value))))

(defun (setf worksheet-cell-input-value) (value worksheet row col)
  (check-type worksheet worksheet)
  (with-slots (cells) worksheet
    (let ((cell (aref cells row col)))
      (if (typep cell 'spreadsheet-cell)
          (setf (slot-value cell 'new-input-value) value)
          (setf (aref cells row col) (make-instance 'spreadsheet-cell
                                                    :input-value nil
                                                    :value nil
                                                    :numeric-value nil
                                                    :new-input-value value))))))

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

(defun map-cell-range (worksheet function &key
                       (session *gdata-session*)
                       (min-row 0) (max-row (1- (array-dimension (slot-value worksheet 'cells) 0)))
                       (min-col 0) (max-col (1- (array-dimension (slot-value worksheet 'cells) 1))))
  "Call FUNCTION for each cell for the given WORKSHEET with the following arguments:
DOM-NODE - the <entry> node in the XML result
ID - the URL to use when updating a cell
ROW - the row number for the node (0-based)
COLUMN - the column number for the node (0-based)
VALUE - the content of the <gs:cell>
INPUT-VALUE - the content of the <gs:cell inputValue=...> attribute
NUMERIC-VALUE - the numeric content of the cell, or NIL if the cell is not numeric"
  (check-type worksheet worksheet)
  (check-type function function)
  (with-slots (cells) worksheet
    (check-range max-row 0 (1- (array-dimension cells 0)))
    (check-range min-row 0 max-row)
    (check-range max-col 0 (1- (array-dimension cells 1)))
    (check-range min-col 0 max-col)
    (let ((node-doc (load-and-parse (format nil "~a?min-row=~a&max-row=~a&min-col=~a&max-col=~a"
                                            (find-document-feed worksheet +SPREADSHEETS-CELLSFEED+ +ATOM-XML-MIME-TYPE+)
                                            (1+ min-row) (1+ max-row) (1+ min-col) (1+ max-col))
                                    :session session)))
        (with-gdata-namespaces
          (xpath:map-node-set #'(lambda (n)
                                  (let* ((cell-node (xpath:first-node (xpath:evaluate "gs:cell" n)))
                                         (id-url (text-from-xpath n "atom:id"))
                                         (row (parse-integer (dom:get-attribute cell-node "row")))
                                         (col (parse-integer (dom:get-attribute cell-node "col")))
                                         (value (get-text-from-node cell-node))
                                         (input-value (dom:get-attribute cell-node "inputValue"))
                                         (numeric-value-as-string (dom:get-attribute cell-node "numericValue")))
                                    (funcall function
                                             n id-url (1- row) (1- col) value input-value
                                             (if (and numeric-value-as-string
                                                      (/= (length numeric-value-as-string) 0))
                                                 (parse-number:parse-number numeric-value-as-string)
                                                 nil))))
                              (xpath:evaluate "/atom:feed/atom:entry" node-doc))))))

(defun load-cell-range (worksheet &key
                        (session *gdata-session*)
                        (min-row 0) (max-row (1- (array-dimension (slot-value worksheet 'cells) 0)))
                        (min-col 0) (max-col (1- (array-dimension (slot-value worksheet 'cells) 1))))
  "Loads the specified cell range into the worksheet."
  (with-slots (cells) worksheet
    (fill-array-slice cells :empty min-row max-row min-col max-col)
    (map-cell-range worksheet #'(lambda (dom-node id-url row column value input-value numeric-value)
                                  (setf (aref cells row column) (make-instance 'spreadsheet-cell
                                                                               :node-dom dom-node
                                                                               :id-url id-url
                                                                               :input-value input-value
                                                                               :value value
                                                                               :numeric-value numeric-value)))
                    :session session
                    :min-row min-row
                    :max-row max-row
                    :min-col min-col
                    :max-col max-col)))

;;;
;;; Uploading spreadsheet updates
;;;
(defun find-updated-cells (worksheet)
  (with-slots (cells) worksheet
    (loop
       for y from 0 below (array-dimension cells 0)
       nconc (loop
                for x from 0 below (array-dimension cells 1)
                nconc (let ((cell (aref cells y x)))
                        (if (and (typep cell 'spreadsheet-cell)
                                 (slot-value cell 'new-input-value))
                            (list (list y x cell))
                            nil))))))

(defun save-updated-cells (worksheet &key (session *gdata-session*))
  (let ((updated (find-updated-cells worksheet))
        (cellsfeed-name (find-document-feed worksheet +SPREADSHEETS-CELLSFEED+ +ATOM-XML-MIME-TYPE+))
        (batchid 0))
    (let ((content (with-output-to-string (s)
                     (build-atom-xml-stream `(("atom" "feed")
                                              (("atom" "id") ,cellsfeed-name)
                                              ,@(mapcar #'(lambda (v)
                                                            (destructuring-bind (row col cell) v
                                                              (let ((cell-feed (find-document-feed cell "self" "application/atom+xml")))
                                                              `(("atom" "entry")
                                                                (("batch" "id") ,(princ-to-string (incf batchid)))
                                                                (("batch" "operation" "type" "update"))
                                                                (("atom" "id") ,cell-feed)
                                                                (("atom" "link"
                                                                         "rel" "edit"
                                                                         "type" "application/atom+xml"
                                                                         "href" ,(format nil "~a/2" cell-feed)))
                                                                (("gs" "cell"
                                                                       "row" ,(princ-to-string (1+ row))
                                                                       "col" ,(princ-to-string (1+ col))
                                                                       "inputValue" ,(slot-value cell 'new-input-value)))))))
                                                        updated))
                                            s))))
      ;; The proper way to find the batch feed is to extract it from the header
      ;; of the cells feed. However, there isn't any good place to store it,
      ;; and we might not even have retrieved the cell feed at this time.
      ;; Fortunately, the format of the batch feed is the same as the cells
      ;; feed with "/batch" appended. This is documented in the gdata documentation
      ;; so it really should be stable.
      (format t "~a~%" content)
      (let ((result (load-and-parse (format nil "~a/batch" cellsfeed-name)
                                    :session session
                                    :method :post
                                    :content-type "application/atom+xml"
                                    :content content
                                    :additional-headers '(("If-Match" . "*")))))
        (dom:map-document (cxml:make-character-stream-sink *standard-output*) result)
        ;; TODO clear the updated state of the cells (reload the content?)
        nil))))
