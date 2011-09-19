(in-package :cl-gdata)

(declaim #.*compile-decl*)

(defun create-worksheet (document-id title rows cols &key (session *gdata-session*))
  (with-gdata-namespaces
    (let ((content (with-output-to-string (s)
                     (build-atom-xml-stream `(("atom" "entry")
                                              (("atom" "title") ,title)
                                              (("gs" "rowCount") ,rows)
                                              (("gs" "colCount") ,cols))
                                            s))))
      (authenticated-request (format nil "https://spreadsheets.google.com/feeds/worksheets/~a/private/full"
                                     (url-rewrite:url-encode document-id))
                             session
                             :method :post
                             :content-type "application/atom+xml"
                             :content content))))
