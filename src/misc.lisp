;;;
;;; Various utility functions and macros that are not specific to cl-gdata
;;;

(in-package :cl-gdata)

(declaim #.*compile-decl*)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro print-unreadable-safely ((&rest slots) object stream &body body)
  "A version of PRINT-UNREADABLE-OBJECT and WITH-SLOTS that is safe to use with unbound slots"
  (let ((object-copy (gensym "OBJECT"))
        (stream-copy (gensym "STREAM")))
    `(let ((,object-copy ,object)
           (,stream-copy ,stream))
       (symbol-macrolet ,(mapcar #'(lambda (slot-name)
                                     `(,slot-name (if (slot-boundp ,object-copy ',slot-name)
                                                      (slot-value ,object-copy ',slot-name)
                                                      :not-bound)))
                                 slots)
         (print-unreadable-object (,object-copy ,stream-copy :type t :identity nil)
           ,@body)))))

(defun perform-check-range (quoted-place value min max)
  (unless (<= min value max)
    (error "The expression ~s is out of range. Value is ~a, should be between ~a and ~a inclusive."
           quoted-place value min max)))

(defmacro check-range (place min max)
  (flet ()
    `(perform-check-range ',place ,place ,min ,max)))
