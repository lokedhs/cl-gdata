;;;
;;; Various utility functions and macros that are not specific to cl-gdata
;;;

(in-package :cl-gdata-misc)

(declaim #.cl-gdata::*compile-decl*)

(alexandria:define-constant +SCHEME-KIND+ "http://schemas.google.com/g/2005#kind" :test 'equal)

(defmacro print-unreadable-safely ((&rest slots) object stream &body body)
  "A version of PRINT-UNREADABLE-OBJECT and WITH-SLOTS that is safe to use with unbound slots"
  (let ((object-copy (gensym "OBJECT"))
        (stream-copy (gensym "STREAM")))
    `(let ((,object-copy ,object)
           (,stream-copy ,stream))
       (symbol-macrolet ,(mapcar #'(lambda (slot-name)
                                     `(,slot-name (if (and (slot-exists-p ,object-copy ',slot-name)
                                                           (slot-boundp ,object-copy ',slot-name))
                                                      (slot-value ,object-copy ',slot-name)
                                                      :not-bound)))
                                 slots)
         (print-unreadable-object (,object-copy ,stream-copy :type t :identity nil)
           ,@body)))))

(defun %perform-check-range (quoted-place value min max)
  (unless (<= min value max)
    (error "The expression ~s is out of range. Value is ~a, should be between ~a and ~a inclusive."
           quoted-place value min max)))

(defmacro check-range (place min max)
  `(%perform-check-range ',place ,place ,min ,max))

(defun name-from-filename (file)
  "Returns the name part of FILE. FILE can be either a string or a pathspec."
  (pathname-name (parse-namestring file)))

(deftype cl-gdata-date-value () '(or alexandria:positive-real string local-time:timestamp))

(defun parse-date-string (value)
  "Parse a date value and return it as an ISO date."
  (check-type value cl-gdata-date-value)
  (flet ((format-local-time (v)
           (local-time:format-timestring nil v :timezone local-time:+utc-zone+)))
    (etypecase value
      (string value)
      (number (format-local-time (local-time:universal-to-timestamp value)))
      (local-time:timestamp (format-local-time value)))))

(defun make-url-search-params (url &rest definitions)
  "Appends a set of candidate parameters to URL and returns the resulting url.
The arguments in DEFINITIONS consists of a set of pairs, the first of which
is a string, and the second is any printable value or NIL. If the value is
NIL, the pair is ignored, otherwise the first is used as a parameter name
and the second is its value.

For example, the following call:

\(make-url-search-params \"http://www.example.com/foo\" \"foo\" nil \"bar\" 10 \"xyz\" \"foo\")

Will result in the following string:

http://www.example.com/foo?bar=10&xyz=foo"
  (with-output-to-string (s)
    (princ url s)
    (loop
       with first = t
       for (tag value) on definitions by #'cddr
       if value
       do (progn
            (format s "~a~a=~a" (if first "?" "&") tag (url-rewrite:url-encode (princ-to-string value)))
            (setq first nil)))))

;;;
;;;  API-KEY handling
;;;

(define-condition no-api-key-specified (error)
  ()
  (:report (lambda (condition out)
             (declare (ignore condition))
             (format out "GData API key must be given. Create a key at http://code.google.com/apis/console/ then
either set *GDATA-API-KEY* to the key, or pass it using the API-KEY keyword argument.")))
  (:documentation "Condition that is signalled if the API key has not been set when accessing a service."))

(defun read-new-api-key ()
  (format t "Enter new API key: ")
  (finish-output)
  (let ((key (read-line)))
    (list key)))

(defun check-or-assign-api-key (api-key)
  (restart-case
      (progn
        (unless api-key
          (error 'no-api-key-specified))
        api-key)
    (specify-api-key (new-api-key)
      :report "Specify API key"
      :interactive read-new-api-key
      (check-or-assign-api-key new-api-key))))

(defmacro check-api-key (v)
  `(setq ,v (check-or-assign-api-key ,v)))
