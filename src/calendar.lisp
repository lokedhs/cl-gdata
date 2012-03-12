(in-package :cl-gdata-calendar)

(declaim #.cl-gdata::*compile-decl*)

;;;
;;; CALENDAR
;;;

(defclass calendar (json-instance)
  ((id      :type string
            :reader calendar-id
            :initarg :id)
   (summary :type string
            :accessor calendar-name
            :initarg :summary))
  (:documentation "Class that describes a calendar"))

(defmethod initialize-instance :after ((obj calendar) &rest initargs)
  (declare (ignore initargs))
  (init-json-fields obj '((id :id)
                          (summary :summary)))
  )

(defmethod print-object ((obj calendar) out)
  (print-unreadable-safely (summary) obj out
    (format out "~s" summary)))

;;;
;;; EVENT
;;;

(defclass event (json-instance)
  ((id          :type string
                :reader event-id)
   (description :type (or null string)
                :reader event-description))
  (:documentation "Class that describes a calendar event"))

(defmethod initialize-instance :after ((obj event) &rest initargs)
  (declare (ignore initargs))
  (init-json-fields obj '((id :id)
                          (description :description))))

(defmethod print-object ((obj event) out)
  (print-unreadable-safely (description) obj out
    (format out "~s" description)))

(defun list-calendars (&key (api-key *gdata-api-key*) )
  (unless api-key
    (error "GData API key must be given"))
  (let ((data (load-and-parse-json (format nil "https://www.googleapis.com/calendar/v3/users/me/calendarList?key=~a"
                                           (url-rewrite:url-encode api-key)))))
    (mapcar #'(lambda (v)
                (make-instance 'calendar :data v))
            (cdr (assoc :items data)))))

(defun list-events (calendar &key (api-key *gdata-api-key*))
  "List calendar events. CALENDAR is either an instance of calendar, or
a calendar id string."
  (let ((id (etypecase calendar
              (string calendar)
              (calendar (calendar-id calendar)))))
    (mapcar #'(lambda (v)
                (make-instance 'event :data v))
            (cdr (assoc :items
                        (load-and-parse-json (format nil "https://www.googleapis.com/calendar/v3/calendars/~a/events?key=~a"
                                                     (url-rewrite:url-encode id)
                                                     (url-rewrite:url-encode api-key))))))))
