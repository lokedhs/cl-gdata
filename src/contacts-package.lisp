(defpackage :cl-gdata-contacts
  (:use :cl :cl-gdata :cl-gdata-misc)
  (:export #:+EMAIL-TAG-HOME+
           #:+EMAIL-TAG-WORK+
           #:+EMAIL-TAG-OTHER+
           #:+PHONE-TAG-MOBILE+
           #:+PHONE-TAG-HOME+
           #:contact 
           #:phone-number
           #:family-name
           #:given-name
           #:full-name
           #:email
           #:update-contact
           #:list-contacts))
