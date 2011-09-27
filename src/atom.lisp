(in-package :cl-gdata)

(declaim #.*compile-decl*)

(define-constant +ATOM-XML-MIME-TYPE+ "application/atom+xml")

(defclass node-dom-mixin ()
  ((feeds    :type list
             :initarg :feeds
             :reader document-feeds
             :documentation "A list of links from this document.
Each entry is a list of the three attributes in a \"link\"
node: \"rel\", \"type\", \"href\".")
   (node-dom :initarg :node-dom
             :reader document-node-dom
             :documentation "The DOM node that was used to initialise this document")))

(defmethod initialize-instance :after ((node node-dom-mixin) &key node-dom &allow-other-keys)
  (when node-dom
    (with-slots (feeds) node
      (with-gdata-namespaces
        (setf feeds (xpath:map-node-set->list #'(lambda (n)
                                                  (list (dom:get-attribute n "rel")
                                                        (dom:get-attribute n "type")
                                                        (dom:get-attribute n "href")))
                                              (xpath:evaluate "atom:link" node-dom)))))))

(defclass atom-feed-entry (node-dom-mixin)
  ((title        :type string
		 :reader feed-entry-title
		 :documentation "Content of the <title> node"))
  (:documentation "Common superclass for all Atom feed entries"))

(defmethod initialize-instance :after ((obj atom-feed-entry) &key node-dom &allow-other-keys)
  (with-slots (title) obj
    (with-gdata-namespaces
      (setf title (text-from-xpath node-dom "atom:title")))))

(defun find-document-feed (document rel type)
  (check-type document node-dom-mixin)
  (let ((found-feed (find-if #'(lambda (feed)
                                 (and (equal (car feed) rel) (equal (cadr feed) type)))
                             (document-feeds document))))
    (unless found-feed
      (error "Feed not found. rel=~s type=~s" rel type))
    (caddr found-feed)))

