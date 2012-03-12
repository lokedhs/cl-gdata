(defpackage :cl-gdata-calendar
  (:use :cl :cl-gdata :cl-gdata-misc)
  (:export #:list-calendars
           #:list-events
           #:calendar
           #:calendar-id
           #:calendar-name
           #:event
           #:event-id
           #:event-description))
