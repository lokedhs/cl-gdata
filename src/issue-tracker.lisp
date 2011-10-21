(in-package :cl-gdata-issue-tracker)

(declaim #.cl-gdata::*compile-decl*)

(define-constant +COMMENTS-TAG-FEED+ "replies")

(defclass atom-author-mixin ()
  ((author-name    :type string
                   :reader issue-author-name
                   :node "atom:author/atom:name/text()")
   (author-url     :type string
                   :reader issue-author-uri
                   :node "atom:author/atom:uri/text()"))
  (:metaclass atom-feed-entry-class))

(defclass issue (atom-feed-entry atom-author-mixin)
  ((content        :type string
                   :reader issue-content
                   :node "atom:content/text()")
   (label          :type list
                   :reader issue-label
                   :node "issues:label/text()"
                   :node-collectionp t)
   (owner-username :type (or null string)
                   :reader issue-owner-username
                   :node "issues:owner/issues:username/text()")
   (owner-uri      :type (or null string)
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
                   :node "issues:status/text()")
   (comments       :type list
                   :reader issue-comments))
  (:metaclass atom-feed-entry-class))

(defclass comment (atom-feed-entry atom-author-mixin)
  ((content        :type string
                   :reader comment-content
                   :node "atom:content/text()"))
  (:metaclass atom-feed-entry-class))

(defun load-comments (issue &key (session *gdata-session*))
  (check-type issue issue)
  (setf (slot-value issue 'comments)
        (load-atom-feed-url (find-feed-from-atom-feed-entry issue +COMMENTS-TAG-FEED+) 'comment
                            :session session
                            :version "1.0")))

(defun list-issues (project-name &key (session *gdata-session*) (include-comments t))
  (let ((issues (load-atom-feed-url (format nil "https://code.google.com/feeds/issues/p/~a/issues/full"
                                            (url-rewrite:url-encode project-name))
                                    'issue
                                    :session session
                                    :version "1.0")))
    (when include-comments
      (dolist (issue issues)
        (load-comments issue :session session)))
    issues))

(defun add-comment (issue author summary content
                    &key (session *gdata-session*)
                    owner-update status labels cc-update)
  (check-type issue issue)
  (check-type author string)
  (check-type summary string)
  (check-type content string)
  (check-type owner-update (or null string))
  (check-type labels list)
  (check-type cc-update list)

  (flet ((send-output (os)
           (build-atom-xml-stream `(("atom" "entry")
                                    (("atom" "content") ,content)
                                    (("atom" "author")
                                     (("atom" "name") ,author))
                                    (("issues" "updates")
                                     (("issues" "summary") ,summary)
                                     ,@(when status `((("issues" "status") ,status)))
                                     ,@(when owner-update `((("issues" "ownerUpdate") ,owner-update)))
                                     ,@(mapcar #'(lambda (s) `(("issues" "label") ,s)) labels)
                                     ,@(mapcar #'(lambda (s) `(("issues" "ccUpdate") ,s)) cc-update)))
                                  os)))

    (let ((result (load-and-parse (find-feed-from-atom-feed-entry issue +COMMENTS-TAG-FEED+)
                                  :session session
                                  :method :post
                                  :version "1.0"
                                  :content-type +ATOM-XML-MIME-TYPE+
                                  :content #'send-output)))
      result)))
