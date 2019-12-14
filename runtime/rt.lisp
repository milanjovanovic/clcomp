(in-package #:clcomp)

(defparameter *runtime-heap-start* #x200000010)
(defparameter *compilation-start-address* *runtime-heap-start*)
(defparameter *rt-funs* (make-hash-table))


(defstruct compilation units)
(defparameter *current-compilation* (make-compilation :units nil))

(defparameter *start-address* *runtime-heap-start*)

(defparameter *clcomp-home* (namestring (ql:where-is-system "clcomp")))

(defun rt-%defun (name compilation-unit)
  ;; code loading will increase heap pointer
  (setf (gethash name *rt-funs*) *compilation-start-address*)
  (setf *compilation-start-address* (+ *compilation-start-address* (get-compilation-unit-code-size compilation-unit))))

;; FIXME
;; for now we are only resolving FUNCTION calls
(defun resolve-fixup (fixup unit-code-buffer)
  (let* ((name (fun-rip-relative-name (rip-location-rip fixup)))
	 (offset (rip-location-byte-offset fixup))
	 (fun-address (rt-get-fun-address name)))
    (unless fun-address
      (error (format nil "Unknown fun ~a" name)))
    (format t "Name: ~a, Address: ~x~%" name fun-address)
    (let ((bytes (little-endian-64bit fun-address))
	  (index offset))
      (dolist (byte bytes)
	(setf (aref unit-code-buffer index) byte)
	(incf index)))))

(defun resolve-fixups (unit)
  (let ((unit-code (get-compilation-unit-code-buffer unit))
	(fixups (compilation-unit-fixups unit)))
    (if (listp fixups )
	(dolist (fixup fixups)
	  (resolve-fixup fixup unit-code))
	(resolve-fixup fixups unit-code))
    unit-code))

(defun print-hex-code (buffers)
  (with-output-to-string (s)
    (dolist (buffer (reverse buffers))
      (format s "~a" (apply #'concatenate 'string (mapcar #'byte-hex (coerce buffer 'list)))))
    s))

(defun rt-dump-binary (file)
  (let (code-buffers)
    (dolist (comp-unit (reverse (compilation-units *current-compilation*)))
      (push (resolve-fixups comp-unit) code-buffers))
    (with-open-file (f file :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (dolist (c (immediate-as-byte-list *start-address* :imm64))
	(write-byte c f))
      (when *debug*
	(format t "~a~%" (print-hex-code code-buffers)))
      (dolist (buffer (reverse code-buffers))
	(write-sequence buffer f)))
    (rt-reset)))

(defun rt-add-to-compilation (compilation-unit)
  (setf (compilation-units *current-compilation*)
	(cons compilation-unit (compilation-units *current-compilation*))))

(defun rt-get-fun-address (name)
  (gethash name *rt-funs*))

(defun set-start-address ()
  (setf *start-address* *compilation-start-address*))

(defun rt-reset ()
  (setf *current-compilation* (make-compilation :units nil))
  (setf *compilation-start-address* *runtime-heap-start*)
  (setf *start-address* *runtime-heap-start*)
  (setf *rt-funs* (make-hash-table)))

(defun compile-and-dump (form)
  (let ((*debug* nil))
    (rt-reset)
    (clcomp-compile-file (format nil "~a/code/cons.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/array.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/arith.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/call.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/symbol.lisp" *clcomp-home*))
    (clcomp-compile-file (format nil "~a/code/char.lisp" *clcomp-home*))
    (set-start-address)
    (clcomp-compile nil form)
    (maphash (lambda (k v)
	       (format t "~a -> ~x~%" k v))
	     *rt-funs*)
    (rt-dump-binary "/tmp/core")))




