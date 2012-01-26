(defpackage :cl-gdata-system
  (:use :cl :asdf))

(in-package :cl-gdata-system)

(defsystem cl-gdata
  :name "cl-gdata"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "Common Lisp interface to the Google Gdata API's"
  :depends-on (:split-sequence
               :drakma
               :cxml
               :xpath
               :url-rewrite
               :flexi-streams
               :spartns
               :parse-number
               :cl-ppcre
               :gzip-stream
               :closer-mop
               :cl-fad
               :trivial-utf-8
               :alexandria)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     (:file "misc-package")
                                     (:file "clientlogin-package")
                                     (:file "docs-list-package")
                                     (:file "spreadsheets-package")
                                     (:file "picasa-package")
                                     (:file "contacts-package")
                                     (:file "issue-tracker-package")
                                     (:file "cl-gdata-user")
                                     (:file "misc")
                                     (:file "xml-misc")
                                     (:file "cl-gdata")
                                     (:file "auth")
                                     (:file "clientlogin-auth")
                                     (:file "atom")
                                     (:file "docs-list")
                                     (:file "spreadsheets")
                                     (:file "contacts")
                                     (:file "picasa")
                                     (:file "issue-tracker")))))
