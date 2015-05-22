(in-package :cl-gdata-sites)

(alexandria:define-constant +VALID-PAGE-KIND+
    '(:ANNOUNCEMENT :ANNOUNCEMENTSPAGE :ATTACHMENT :COMMENT
      :FILECABINET :LISTITEM :LISTPAGE :WEB :WEBPAGE :WEBATTACHMENT
      :TEMPLATE)
    :test 'equal)

(defclass resource (atom-feed-entry)
  ((id-url             :type string
                       :reader webpage-id-url
                       :node "atom:id/text()")
   (title              :type string
                       :reader webpage-title
                       :node "atom:title/text()")
   (updated            :type string
                       :reader webpage-updated
                       :node "atom:updated/text()")
   (content            :type list
                       :reader webpage-content
                       :node ("atom:content" "@type" "@src")
                       :node-collectionp t))
  (:metaclass atom-feed-entry-class))

(defclass web (resource)
  ()
  (:metaclass atom-feed-entry-class))

(defclass listitem (resource)
  ()
  (:metaclass atom-feed-entry-class))

(defclass comment (resource)
  ()
  (:metaclass atom-feed-entry-class))

(defclass webpage (resource)
  ((page-name          :type string
                       :reader webpage-name
                       :node "sites:pageName/text()"))
  (:metaclass atom-feed-entry-class))

(defclass template (webpage)
  ()
  (:metaclass atom-feed-entry-class))

(defclass announcement (webpage)
  ()
  (:metaclass atom-feed-entry-class))

(defclass announcementspage (webpage)
  ()
  (:metaclass atom-feed-entry-class))

(defclass listpage (webpage)
  ()
  (:metaclass atom-feed-entry-class))

(defclass file-cabinet (resource)
  ()
  (:metaclass atom-feed-entry-class))

(defclass attachment (resource)
  ()
  (:metaclass atom-feed-entry-class))

(defclass webattachment (resource)
  ()
  (:metaclass atom-feed-entry-class))

(defgeneric make-page-from-resource (node resource-type)
  (:documentation "Create a page instance based on a specific resource type")
  (:method (node resource-type)
    (warn "Initialisation method for resource type ~s not available" resource-type)
    nil)
  (:method (node (resource-type (eql :webpage)))
    (make-instance 'webpage :node-dom node))
  (:method (node (resource-type (eql :announcement)))
    (make-instance 'announcement :node-dom node))
  (:method (node (resource-type (eql :attachment)))
    (make-instance 'attachment :node-dom node))
  (:method (node (resource-type (eql :comment)))
    (make-instance 'comment :node-dom node))
  (:method (node (resource-type (eql :filecabinet)))
    (make-instance 'filecabinet :node-dom node))
  (:method (node (resource-type (eql :listitem)))
    (make-instance 'listitem :node-dom node))
  (:method (node (resource-type (eql :listpage)))
    (make-instance 'listpage :node-dom node))
  (:method (node (resource-type (eql :web)))
    (make-instance 'web :node-dom node))
  (:method (node (resource-type (eql :webattachment)))
    (make-instance 'webattachment :node-dom node))
  (:method (node (resource-type (eql :template)))
    (make-instance 'template :node-dom node)))

(defun parse-resource-id (resource-id)
  "Given a page, return the id to be used in page URL's. The second
return value is the page type."
  (values-list (reverse (split-sequence:split-sequence #\: resource-id :count 2))))

(defparameter *gdata-page-type-urls*
  '(("http://schemas.google.com/sites/2008#webpage" . :webpage)
    ("http://schemas.google.com/sites/2008#announcement" . :announcementspage)
    ("http://schemas.google.com/sites/2008#announcementspage" . :announcement)
    ("http://schemas.google.com/sites/2008#attachment" . :attachment)
    ("http://schemas.google.com/sites/2008#comment" . :comment)
    ("http://schemas.google.com/sites/2008#filecabinet" . :filecabinet)
    ("http://schemas.google.com/sites/2008#listitem" . :listitem)
    ("http://schemas.google.com/sites/2008#listpage" . :listpage)
    ("http://schemas.google.com/sites/2008#web" . :web)
    ("http://schemas.google.com/sites/2008#webattachment" . :webattachment)
    ("http://schemas.google.com/sites/2008#template" . :template)))

(defun make-page-entry (node)
  (with-gdata-namespaces
    (let* ((resource-id (value-by-xpath (format nil "atom:category[@scheme='~a']/@term" +SCHEME-KIND+) node)))
      (print resource-id)
      (print (assoc resource-id *gdata-page-type-urls* :test #'equal))
      (make-page-from-resource node (cdr (assoc resource-id *gdata-page-type-urls* :test #'equal))))))

(defun list-pages (site &key (domain "site") (session *gdata-session*)
			  include-deleted include-draft kind
			  max-results query-string updated-min)
  "List all the pages that belongs to the authenticated user.

SITE is the site name that appears in the site's landing page URL,
such as in http://sites.google.com/a/domainName/siteName.

:DOMAIN is the domain name, such as \"site\" or a Google Apps domain.

:INCLUDE-DELETED and :INCLUDE-DRAFT specify whether to include deleted
pages and unfinished entries.

:KIND is a list of keywords specifying possible material to return.
Possible values are :ANNOUNCEMENT, :ANNOUNCEMENTSPAGE, :ATTACHMENT,
:COMMENT, :FILECABINET, :LISTITEM, :LISTPAGE, :WEBPAGE, :WEBATTACHMENT,
:TEMPLATE. Some of them might not be supported by this version of the
library.

If :PATH is non-NIL, the query is restricted to a certain page hierarchy.

:MAX-RESULTS can be set to an integer (up to a maximum of 1000) that
limits the number of returned objects.

If :QUERY-STRING is non-NIL, it is used as a search term.

If given, :UPDATED-MIN indicates the oldest documents that should be
included in the output. The value can be either a universal time
value, a local-time instance, or a string in standard ISO format."
  (check-type max-results (or null alexandria:non-negative-integer))
  (check-type query-string (or null string))
  (check-type updated-min (or null cl-gdata-date-value))
  (let* ((site-url (format nil "https://sites.google.com/feeds/content/~A/~A/"
			   domain site))
	 (doc (load-and-parse (print
			       (make-url-search-params
				site-url
				"max-results" max-results
				"include-deleted" (when include-deleted "true")
				"include-draft" (when include-draft "true")
				"kind"
				(when kind (format t "~{~(~A~)~^, ~}" kind))
				"q" query-string
				"updated-min"
				(when updated-min (parse-date-string updated-min))))
			      :version "1.4"
			      :session session)))
    (with-gdata-namespaces
      (remove nil (xpath:map-node-set->list #'make-page-entry (xpath:evaluate "/atom:feed/atom:entry" doc))))))
