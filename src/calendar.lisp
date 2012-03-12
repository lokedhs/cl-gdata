(in-package :cl-gdata-calendar)

(declaim #.cl-gdata::*compile-decl*)

(defclass calendar ()
  ((id      :type string
            :reader calendar-id
            :initarg :id)
   (summary :type string
            :accessor calendar-name
            :initarg :summary)))

(defmethod print-object ((obj calendar) out)
  (print-unreadable-safely (summary) obj out
    (format out "~s" summary)))

(defun list-calendars (&key (api-key *gdata-api-key*) )
  (unless api-key
    (error "GData API key must be given"))
  (let ((data (load-and-parse-json (format nil "https://www.googleapis.com/calendar/v3/users/me/calendarList?key=~a"
                                           (url-rewrite:url-encode api-key)))))
    (mapcar #'(lambda (v)
                (make-instance 'calendar
                               :id (cdr (assoc :id v))
                               :summary (cdr (assoc :summary v))))
            (cdr (assoc :items data)))))
