(in-package :cl-gdata)

(declaim #.*compile-decl*)

(defclass spreadsheet-cell (node-dom-mixin)
  ((input-value   :type string
                  :reader cell-input-value
                  :documentation "The value of the <gs:cell inputValue=...> attribute")
   (value         :type string
                  :reader cell-value
                  :documentation "The content of the <gc:cell> node")
   (numeric-value :type (or number null)
                  :reader cell-numeric-value
                  :documentation "The content of the <gs:cell numericValue=...> attribute, or NIL
if the cell does not contain a number"))
  (:documentation "Class that describes the content of a single cell"))

(defmethod initialize-instance :after ((obj spreadsheet-cell) &rest initargs &key node-dom &allow-other-keys)
  (declare (ignore initargs))
  (with-gdata-namespaces
    (with-slots (input-value value numeric-value) obj
      (let ((cell-node (xpath:first-node (xpath:evaluate "gs:cell" node-dom))))
        (setf input-value (dom:get-attribute cell-node "inputValue"))
        (setf value (get-text-from-node cell-node))
        (let ((numeric-value-as-string (dom:get-attribute cell-node "numericValue")))
          (setf numeric-value (if (and numeric-value-as-string
                                       (/= (length numeric-value-as-string) 0))
                                  (parse-number:parse-number numeric-value-as-string)
                                  nil)))))))

(defmethod print-object ((obj spreadsheet-cell) out)
  (print-unreadable-safely (input-value value) obj out
    (format out "~s~a" value (if (string= value input-value)
                                 ""
                                 (format nil " INPUT-VALUE ~s" input-value)))))
