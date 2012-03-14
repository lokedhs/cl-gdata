(in-package :cl-gdata-calendar)

(declaim #.cl-gdata::*compile-decl*)

(defun parse-timestamp (data)
  (unless (and (listp data) (null (cdr data)))
    (error "Unknown timestamp format: ~s" data))
  (let ((inner (car data)))
    (ecase (car inner)
      (:date-time (local-time:parse-timestring (cdr inner)))
      (:date      (local-time:parse-timestring (cdr inner)))
      )))

;;;
;;; CALENDAR
;;;

(defclass calendar (json-instance)
  ((id      :type string
            :reader calendar-id
            :initarg :id)
   (summary :type string
            :accessor calendar-summary
            :initarg :summary))
  (:documentation "Class that describes a calendar"))

(defmethod initialize-instance :after ((obj calendar) &rest initargs)
  (declare (ignore initargs))
  (init-json-fields obj '((id :id)
                          (summary :summary))))

(defmethod print-object ((obj calendar) out)
  (print-unreadable-safely (summary) obj out
    (format out "~s" summary)))

;;;
;;; EVENT
;;;

(defclass event (json-instance)
  ((id          :type string
                :reader event-id)
   (created     :type local-time:timestamp
                :reader event-created)
   (updated     :type local-time:timestamp
                :reader event-updated)
   (summary     :type (or null string)
                :reader event-summary)
   (start       :type local-time:timestamp
                :reader event-start)
   (end         :type local-time:timestamp
                :reader event-end))
  (:documentation "Class that describes a calendar event"))

(defmethod initialize-instance :after ((obj event) &rest initargs)
  (declare (ignore initargs))
  (init-json-fields obj `((id :id)
                          (created :created ,#'local-time:parse-timestring)
                          (updated :updated ,#'local-time:parse-timestring)
                          (summary :summary)
                          (start :start ,#'parse-timestamp)
                          (end :end ,#'parse-timestamp))))

(defmethod print-object ((obj event) out)
  (flet ((format-timestamp (v)
           ;; Safe timestamp printing function that avoids any print-time errors
           ;; if for some reason the slot is unbound or contains unexpected data.
           (if (typep v 'local-time:timestamp)
               (local-time:format-timestring nil v :format local-time:+asctime-format+)
               v)))
    (print-unreadable-safely (summary start end) obj out
      (format out "~s ~a - ~a" summary (format-timestamp start) (format-timestamp end)))))

;;;
;;;  Calendar access functions
;;;

(defun list-calendars (&key (api-key *gdata-api-key*) )
  "List all calendars for the authenticated user."
  (check-api-key api-key)
  (let ((data (load-and-parse-json (format nil "https://www.googleapis.com/calendar/v3/users/me/calendarList?key=~a"
                                           (url-rewrite:url-encode api-key)))))
    (mapcar #'(lambda (v)
                (make-instance 'calendar :data v))
            (cdr (assoc :items data)))))

(defun list-events (calendar &key (api-key *gdata-api-key*))
  "List calendar events. CALENDAR is either an instance of calendar, or
a calendar id string."
  (check-api-key api-key)
  (let ((id (etypecase calendar
              (string calendar)
              (calendar (calendar-id calendar)))))
    (mapcar #'(lambda (v)
                (make-instance 'event :data v))
            (cdr (assoc :items
                        (load-and-parse-json (format nil "https://www.googleapis.com/calendar/v3/calendars/~a/events?key=~a"
                                                     (url-rewrite:url-encode id)
                                                     (url-rewrite:url-encode api-key))))))))
