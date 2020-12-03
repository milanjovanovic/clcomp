(defstruct stream element-type open type element-type)
(defstruct (fd-stream (:include stream)) fd)
(defstruct (synonym-stream (:include stream)) symbol)

(defparameter *stdin* (make-fd-stream :fd 0 :open t :type :input :element-type 'character))
(defparameter *stdout* (make-fd-stream :fd 1 :open t :type :output :element-type 'character))

(defparameter *standard-input* (make-synonym-stream :symbol '*stdin*))
(defparameter *standard-output* (make-synonym-stream :symbol '*stdout*))

(defun open (filespec &key direction element-type if-exists if-does-not-exist external-format)
  (let* ((lisp-open (load-time-value (%lisp-symbol-address "lisp_open")))
	 (fd (%c-call-one-args lisp-open filespec))
	 (sstream (make-stream :stream fd :element-type t :open t :type :input)))
    sstream))

(defun close (stream &key abort))

(defun read-char (&optional input-stream eof-error-p eof-value recursive-p)
  (let* ((lisp-read-char (load-time-value (%lisp-symbol-address "lisp_read_char")))
	 (stream (fd-stream-fd input-stream))
	 (char (%c-call-one-args lisp-read-char stream)))
    char))

(defun read-byte (stream &optional eof-error-p eof-value)
  "FIXME")

(defun read-sequence (sequence stream &key start end)
  "FIXME")

(defun read-line (&optional input-stream eof-error-p eof-value recursive-p)
  "FIXME")

(defun read-from-string (string &optional eof-error-p eof-value &key start end preserve-whitespace)
  "FIXME")
