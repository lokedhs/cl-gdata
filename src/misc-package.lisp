(defpackage :cl-gdata-misc
  (:use :cl :cl-gdata)
  (:export #:define-constant
           #:print-unreadable-safely
           #:check-range
           #:authenticated-request
           #:http-request-with-stream
           #:load-and-parse
           #:+ATOM-TAG-FEED+
           #:+ATOM-TAG-EDIT+
           #:+ATOM-XML-MIME-TYPE+
           #:node-dom-mixin
           #:document-feeds
           #:document-node-dom
           #:find-document-feed
           #:atom-feed-entry-class
           #:atom-feed-entry
           #:feed-entry-feeds
           #:feed-entry-title
           #:find-feed-from-atom-feed-entry
           #:load-atom-feed
           #:load-atom-feed-url
           #:with-gdata-namespaces
           #:get-text-from-node
           #:find-namespace-url
           #:build-atom-document
           #:build-atom-xml-stream
           #:text-from-xpath
           #:document-feeds
           #:document-node-dom
           #:+HTTP-GDATA-USER-AGENT+))
