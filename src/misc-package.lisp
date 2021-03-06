(defpackage :cl-gdata-misc
  (:use :cl :cl-gdata)
  (:export #:print-unreadable-safely
           #:check-range
           #:authenticated-request
           #:http-request-with-stream
           #:load-and-parse
           #:+ATOM-TAG-FEED+
           #:+ATOM-TAG-EDIT+
           #:+ATOM-XML-MIME-TYPE+
           #:node-dom-mixin
           #:document-feeds
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
           #:+HTTP-GDATA-USER-AGENT+
           #:node-dom
           #:update-from-xpath
           #:update-feed-entry-node
           #:value-by-xpath
           #:display-stream-if-debug
           #:name-from-filename
           #:load-feed
           #:authentication-failed
           #:make-url-search-params
           #:cl-gdata-date-value
           #:parse-date-string
           #:load-and-parse-json
           #:json-instance
           #:init-json-fields
           #:check-api-key
           #:no-api-key-specified
           #:+SCHEME-KIND+
           #:atom-xml-writer)
  (:documentation "Internal package used by the cl-gdata implementation. Not intended for general use."))
