;; (defstruct stream stream element-type)

;; (defparameter *standard-input* nil)
;; (defparameter *standard-output* nil)

;; (defun open (filespec &key direction element-type if-exists if-does-not-exist external-format)
;;   (let* ((lisp-open (load-time-value (%lisp-symbol-address "lisp_open")))
;; 	 (lisp-close (load-time-value (%lisp-symbol-address "lisp_close")))
;; 	 (stream (%c-call-one-args lisp-open filespec))
;; 	 (sstream (make-stream :stream stream :element-type t)))
;;     sstream))

;; (defun close (stream &key abort))

;; (defun read-char (&optional input-stream eof-error-p eof-value recursive-p)
;;   (let* ((lisp-read-char (load-time-value (%lisp-symbol-address "lisp_read_char")))
;; 	 (stream (stream-stream input-stream))
;; 	 (char (%c-call-one-args lisp-read-char stream)))
;;     char))

;; (defun read-byte (stream &optional eof-error-p eof-value)
;;   "FIXME")

;; (defun read-sequence (sequence stream &key start end)
;;   "FIXME")

;; (defun read-line (&optional input-stream eof-error-p eof-value recursive-p)
;;   "FIXME")

;; (defun read-from-string (string &optional eof-error-p eof-value &key start end preserve-whitespace)
;;   "FIXME")
