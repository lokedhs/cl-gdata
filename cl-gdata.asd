(defpackage :cl-gdata-system
  (:use :cl :asdf))

(in-package :cl-gdata-system)

(defsystem cl-gdata
  :name "cl-gdata"
  :author "Elias Martenson <lokedhs@gmail.com>"
  :license "BSD"
  :description "Common Lisp interface to the Google GData API's"
  :depends-on (:split-sequence
               :drakma
               :cxml
               :xpath
               :url-rewrite
               :flexi-streams
               ;; :spartns
               :parse-number
               :cl-ppcre
               :gzip-stream
               :closer-mop
               :cl-fad
               :trivial-utf-8
               :alexandria
               :string-case
               :local-time
               :cl-json)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     (:file "misc-package")
                                     (:file "clientlogin-package")
                                     (:file "oauth-package")
				     (:file "scope-package")
                                     (:file "device-package")
                                     (:file "app-package")
                                     (:file "docs-list-package")
                                     (:file "spreadsheets-package")
                                     (:file "picasa-package")
                                     (:file "contacts-package")
                                     (:file "issue-tracker-package")
                                     (:file "calendar-package")
                                     (:file "cl-gdata-user")
                                     (:file "cl-gdata")
                                     (:file "misc")
                                     (:file "json-misc")
                                     (:file "xml-misc")
                                     (:file "auth")
                                     (:file "clientlogin-auth")
				     (:file "scope-auth")
				     (:file "device-auth")
				     (:file "app-auth")
                                     (:file "atom")
                                     (:file "docs-list")
                                     (:file "docs-access")
                                     (:file "spreadsheets")
                                     (:file "contacts")
                                     (:file "picasa")
                                     (:file "issue-tracker")
                                     (:file "calendar")))))
