(in-package :cl-gdata-contacts)

(alexandria:define-constant +EMAIL-TAG-HOME+ "http://schemas.google.com/g/2005#home" :test 'equal)
(alexandria:define-constant +EMAIL-TAG-WORK+ "http://schemas.google.com/g/2005#work" :test 'equal)
(alexandria:define-constant +EMAIL-TAG-OTHER+ "http://schemas.google.com/g/2005#other" :test 'equal)

(alexandria:define-constant +PHONE-TAG-MOBILE+ "http://schemas.google.com/g/2005#mobile" :test 'equal)
(alexandria:define-constant +PHONE-TAG-HOME+ "http://schemas.google.com/g/2005#home" :test 'equal)

(defun clear-email (node)
  (with-gdata-namespaces
    (mapc #'(lambda (child-node)
              (dom:remove-child (dom:parent-node child-node) child-node))
          (xpath:map-node-set->list #'identity (xpath:evaluate "gd:email" node)))))

(defun update-email (node entry slot-descriptor)
  (declare (ignore slot-descriptor))
  (let ((email-node (dom:create-element-ns (dom:owner-document node) (find-namespace-url "gd") "email")))
    (destructuring-bind (rel address primary) entry
      (dom:set-attribute email-node "rel" rel)
      (dom:set-attribute email-node "address" address)
      (when primary
        (dom:set-attribute email-node "primary" "true"))
      (dom:append-child node email-node))))

(defun find-or-create-child-node (node child-ns child-name)
  (let ((result (xpath:evaluate (format nil "~a:~a" child-ns child-name) node)))
    (if (xpath:node-set-empty-p result)
        ;; Node set is empty, create a new node
        (let ((new-node (dom:create-element-ns (dom:owner-document node) (find-namespace-url child-ns) child-name)))
          (dom:append-child node new-node)
          new-node)
        ;; Else, return the first found node
        (xpath:first-node result))))

(defun replace-node-text (node text)
  (map nil #'(lambda (n)
               (when (dom:text-node-p n)
                 (dom:remove-child node n)))
       (dom:child-nodes node))
  (let ((text-node (dom:create-text-node (dom:owner-document node) text)))
    (dom:append-child node text-node)))

(defun update-text-with-create (node text path)
  (loop
     with current = node
     for (entry-ns entry-name) in path
     do (setq current (find-or-create-child-node current entry-ns entry-name))
     finally (replace-node-text current text)))

(defun update-full-name (node entry slot-descriptor)
  (declare (ignore slot-descriptor))
  (update-text-with-create node entry '(("gd" "name")
                                        ("gd" "fullName"))))

(defun update-given-name (node entry slot-descriptor)
  (declare (ignore slot-descriptor))
  (update-text-with-create node entry '(("gd" "name")
                                        ("gd" "givenName"))))

(defun update-family-name (node entry slot-descriptor)
  (declare (ignore slot-descriptor))
  (update-text-with-create node entry '(("gd" "name")
                                        ("gd" "familyName"))))

(defclass contact (atom-feed-entry)
  ((full-name    :type (or null string)
		 :accessor full-name
                 :node "gd:name/gd:fullName/text()"
                 :node-updater-function update-full-name
		 :documentation "Content of the <gd:name><gd:fullName> node")
   (given-name   :type (or null string)
		 :accessor given-name
                 :node "gd:name/gd:givenName/text()"
                 :node-updater-function update-given-name
		 :documentation "Content of the <gd:name><gd:givenName> node")
   (family-name  :type (or null string)
		 :accessor family-name
                 :node "gd:name/gd:familyName/text()"
                 :node-updater-function update-family-name
		 :documentation "Content of the <gd:name><gd:familyName> node")
   (email        :type list
		 :accessor email
                 :node ("gd:email" "@rel" "@address" ("@primary" :true-false))
                 :node-collectionp t
                 :node-clear-function clear-email
                 :node-updater-function update-email
		 :documentation "Alist of email addresses")
   (phone-number :type list
		 :reader phone-number
                 :node ("gd:phoneNumber" "@rel" "text()")
                 :node-collectionp t
		 :documentation "Alist of phone numbers"))
  (:documentation "Class that represents a contact element")
  (:metaclass atom-feed-entry-class))

(defun list-contacts (&key (session *gdata-session*) username updated-min max-results start-index query-string)
  "Return a list of all contacts for the specified user"
  (check-type max-results (or null alexandria:non-negative-integer))
  (check-type start-index (or null alexandria:non-negative-integer))
  (check-type query-string (or null string))
  (check-type updated-min (or null cl-gdata-date-value))
  (load-atom-feed-url (make-url-search-params (format nil "https://www.google.com/m8/feeds/contacts/~a/full"
                                                      (if username (url-rewrite:url-encode username) "default"))
                                              "q" query-string
                                              "updated-min" (when updated-min (parse-date-string updated-min))
                                              "max-results" max-results
                                              "start-index" start-index)
                      'contact
                      :session session))

(defun %write-doc-to-stream (doc stream)
  (dom:map-document (cxml:make-namespace-normalizer (cxml:make-character-stream-sink stream)) doc))

(defun update-contact (contact &key (session *gdata-session*))
  "Update the remote contact list to reflect any local changes to the contact"
  (let ((doc (cxml-dom:create-document)))
    (dom:append-child doc (update-feed-entry-node contact doc))
    (let ((result (load-and-parse (find-feed-from-atom-feed-entry contact "edit")
                                  :session session
                                  :method :put
                                  :additional-headers `(("If-Match" . "*")
                                                        ("Content-Type" . ,+ATOM-XML-MIME-TYPE+))
                                  :content #'(lambda (os) (%write-doc-to-stream doc os)))))
      (with-gdata-namespaces
        (make-instance 'contact :node-dom (xpath:first-node (xpath:evaluate "/atom:entry" result)))))))
