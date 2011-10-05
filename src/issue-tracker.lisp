(in-package :cl-gdata-issue-tracker)

(declaim #.cl-gdata::*compile-decl*)

(defclass issue (atom-feed-entry)
  ((content        :type string
                   :reader issue-content
                   :node "atom:content/text()")
   (author-name    :type string
                   :reader issue-author-name
                   :node "atom:author/atom:name/text()")
   (author-url     :type string
                   :reader issue-author-uri
                   :node "atom:author/atom:name/text()")
   (label          :type string
                   :reader issue-label
                   :node "issues:label/text()"
                   :node-collectionp t)
   (owner-username :type string
                   :reader issue-owner-username
                   :node "issues:owner/issues:username/text()")
   (owner-uri      :type string
                   :reader issue-owner-uri
                   :node "issues:owner/issues:uri/text()")
   (stars          :type number
                   :reader issue-stars
                   :node "issues:stars/text()"
                   :node-type :number)
   (state          :type string
                   :reader issue-state
                   :node "issues:state/text()")
   (status         :type string
                   :reader issue-status
                   :node "issues:status/text()"))
  (:metaclass atom-feed-entry-class))

(defun list-issues (project-name &key (session *gdata-session*))
  (load-atom-feed-url (format nil "https://code.google.com/feeds/issues/p/~a/issues/full"
                              (url-rewrite:url-encode project-name))
                      'issue
                      :session session))
