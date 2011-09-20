(in-package :cl-gdata)

(declaim #.*compile-decl*)

(defparameter *gdata-namespaces* '(("atom" "http://www.w3.org/2005/Atom")
                                   ("gd" "http://schemas.google.com/g/2005")
                                   ("docs" "http://schemas.google.com/docs/2007")
                                   ("gs" "http://schemas.google.com/spreadsheets/2006")))

(defmacro with-gdata-namespaces (&body body)
  `(xpath:with-namespaces ,*gdata-namespaces*
     ,@body))

(defun get-text-from-node (node)
  (if (null node)
      nil
      (with-output-to-string (s)
        (dom:do-node-list (child (dom:child-nodes node))
          (when (dom:text-node-p child)
            (princ (dom:node-value child) s))))))

(defun find-namespace-url (name)
  (let ((url (find name *gdata-namespaces* :key #'car :test #'equal)))
    (unless url
      (error "Can't find namespace URL for name: ~s" name))
    (cadr url)))

;;;
;;; Command to create a new worksheet:
;;;
;;; <entry xmlns="http://www.w3.org/2005/Atom"
;;;     xmlns:gs="http://schemas.google.com/spreadsheets/2006">
;;;   <title>Expenses</title>
;;;   <gs:rowCount>50</gs:rowCount>
;;;   <gs:colCount>10</gs:colCount>
;;; </entry>
;;;
;;; Required sexp:
;;;
;;; (("atom" "entry") (("atom" "title") "Expenses")
;;;                   (("gs" "rowCount") "50")
;;;                   (("gs" colCount") "10"))
;;;
;;; In other words:
;;;   An element node consists of a list with two elements: namespace and name
;;;   A text node consists of a simple string
;;;
;;; Another example:
;;;
;;;  <foo>
;;;    <bar>hello</bar>
;;;    <inner>
;;;       <xyz>text here</xyz>
;;;    </inner>
;;;  </foo>
;;;
;;; Resulting sexp:
;;;
;;; (("atom" "foo")
;;;    (("atom" "bar") "hello")
;;;    (("atom" "inner")
;;;        (("atom" "xyz") "text here")))
;;;
(defun build-atom-document (content)
  (let ((doc (cxml-dom:create-document)))
    (labels ((append-subtree (node tree)
               (let ((n
                      (etypecase tree
                        (list (let ((e (car tree)))
                                (let ((new-node (dom:create-element-ns doc
                                                                       (find-namespace-url (car e))
                                                                       (cadr e))))
                                  (dolist (curr-child (cdr tree))
                                    (append-subtree new-node curr-child))
                                  new-node)))
                        (string (dom:create-text-node doc tree))
                        (float (dom:create-text-node doc (format nil "~f" tree)))
                        (number (dom:create-text-node doc (format nil "~a" tree))))))
                 (dom:append-child node n))))
      (check-type content list)
      (append-subtree doc content)
      doc)))

(defun build-atom-xml-stream (content stream)
  (dom:map-document (cxml:make-namespace-normalizer (cxml:make-character-stream-sink stream))
                    (build-atom-document content)))

(defun text-from-xpath (node path)
  (get-text-from-node (xpath:first-node (xpath:evaluate path node))))
