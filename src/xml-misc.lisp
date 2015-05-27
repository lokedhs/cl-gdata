(in-package :cl-gdata-misc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *gdata-namespaces* '(("atom" "http://www.w3.org/2005/Atom")
                                     ("gd" "http://schemas.google.com/g/2005")
                                     ("docs" "http://schemas.google.com/docs/2007")
                                     ("gs" "http://schemas.google.com/spreadsheets/2006")
                                     ("batch" "http://schemas.google.com/gdata/batch")
                                     ("gphoto" "http://schemas.google.com/photos/2007")
                                     ("exif" "http://schemas.google.com/photos/exif/2007")
                                     ("media" "http://search.yahoo.com/mrss/")
                                     ("issues" "http://schemas.google.com/projecthosting/issues/2009")
                                     ("gAcl" "http://schemas.google.com/acl/2007")
				     ("sites" "http://schemas.google.com/sites/2008"))))

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
(defun build-atom-document (content)
  "Given a sexp-based structure of an atom document, return a
DOM tree describing the corresponding XML document.

The data structure is a list of elements. An element can be
either a node definition or a simple string. A node definition
is a list where the two first elements is the namespace name
and the node name respectively. The further list entries are
pairs of strings, the first of which is an attribute name and
the second is the attribute value.

For example, consider the following XML snippet:

<atom:foo>
  <atom:bar>hello</atom:bar>
  <atom:inner>
    <atom:xyz foo=\"bar\">text here</atom:xyz>
  </atom:inner>
</atom:foo>

The resulting sexp would then look like this:

\(\(\"atom\" \"foo\")
 \(\(\"atom\" \"bar\") \"hello\")
 \(\(\"atom\" \"inner\")
  \(\(\"atom\" \"xyz\" \"foo\" \"bar\") \"text here\")))"
  (check-type content list)
  (let ((doc (cxml-dom:create-document)))
    (labels ((append-subtree (node tree)
               (let ((n
                      (etypecase tree
                        (list (let ((e (car tree)))
                                (let ((new-node (dom:create-element-ns doc
                                                                       (find-namespace-url (car e))
                                                                       (concatenate 'string (car e) ":" (cadr e)))))
                                  (loop
                                     for (name val) on (cddr e) by #'cddr
                                     do (dom:set-attribute new-node name val))
                                  (dolist (curr-child (cdr tree))
                                    (append-subtree new-node curr-child))
                                  new-node)))
                        (string (dom:create-text-node doc tree))
                        (float (dom:create-text-node doc (format nil "~f" tree)))
                        (integer (dom:create-text-node doc (format nil "~a" tree))))))
                 (dom:append-child node n))))
      (append-subtree doc content)
      doc)))

(defun build-atom-xml-stream (content stream)
  "Convert the atom structure in CONTENT as per BUILD-ATOM-DOCUMENT and write the
resulting XML data to STREAM."
  (dom:map-document (cxml:make-namespace-normalizer (cxml:make-character-stream-sink stream))
                    (build-atom-document content)))

(defun atom-xml-writer (content)
  "Return a function that when called with a stream STREAM, will apply
BUILD-ATOM-XML-STREAM on CONTENT and STREAM."
  #'(lambda (stream)
      (build-atom-xml-stream content stream)))

(defun debug-print-dom (doc &optional (stream *standard-output*))
  (dom:map-document (cxml:make-namespace-normalizer (cxml:make-character-stream-sink stream)) doc))

(defun value-by-xpath (expression node &key (default-value nil default-value-assigned-p))
  (let ((result (xpath:evaluate expression node)))
    (if (xpath:node-set-empty-p result)
        (if default-value-assigned-p
            default-value
            (error "No value found for expression: ~s" expression))
        (dom:node-value (xpath:first-node result)))))
