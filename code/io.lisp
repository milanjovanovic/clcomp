(defstruct stream stream element-type)

(defparameter *standard-input* nil)
(defparameter *standard-output* nil)

(defun open (filespec &key direction element-type if-exists if-does-not-exist external-format)
  "FIXME")

(defun read-char (&optional input-stream eof-error-p eof-value recursive-p)
  "FIXME")

(defun read-byte (stream &optional eof-error-p eof-value)
  "FIXME")

(defun read-sequence (sequence stream &key start end)
  "FIXME")

(defun read-line (&optional input-stream eof-error-p eof-value recursive-p)
  "FIXME")

(defun read-from-string (string &optional eof-error-p eof-value &key start end preserve-whitespace)
  "FIXME")

