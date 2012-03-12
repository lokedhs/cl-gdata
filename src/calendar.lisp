(in-package :cl-gdata-calendar)

(declaim #.cl-gdata::*compile-decl*)

(defun parse-timestamp (data)
  (unless (and (listp data) (null (cdr data)) (eq (caar data) :date-time))
    (error "Unexpected timestamp data: ~s" data))
  (local-time:parse-timestring (cdar data)))

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

(defun check-api-key (api-key)
  (unless api-key
    (error "GData API key must be given. Create a key at http://code.google.com/apis/console/ then
either set *GDATA-API-KEY* to the key, or pass it using the API-KEY keyword argument.")))

(defun list-calendars (&key (api-key *gdata-api-key*) )
  (check-api-key api-key)
  (let ((data (load-and-parse-json (format nil "https://www.googleapis.com/calendar/v3/users/me/calendarList?key=~a"
                                           (url-rewrite:url-encode api-key)))))
    (mapcar #'(lambda (v)
                (make-instance 'calendar :data v))
            (cdr (assoc :items data)))))

(defun list-events (calendar &key (api-key *gdata-api-key*))
  (check-api-key api-key)
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
