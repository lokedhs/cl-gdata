(defpackage :cl-gdata-contacts
  (:use :cl :cl-gdata :cl-gdata-misc)
  (:export #:+EMAIL-TAG-HOME+
           #:+EMAIL-TAG-WORK+
           #:+EMAIL-TAG-OTHER+
           #:+PHONE-TAG-MOBILE+
           #:+PHONE-TAG-HOME+
           #:contact
           #:contact-full-name
           #:contact-given-name
           #:contact-family-name
           #:contact-email
           #:contact-phone-number
           #:list-all-contacts))
