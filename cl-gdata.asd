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
               :string-case
               :flexi-streams)
  :components ((:module src
                        :serial t
                        :components ((:file "package")
                                     (:file "misc")
                                     (:file "xml-misc")
                                     (:file "auth")
                                     (:file "clientlogin-auth")
                                     (:file "docs")
                                     (:file "spreadsheets")))))
