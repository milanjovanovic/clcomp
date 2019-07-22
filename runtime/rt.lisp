(in-package #:clcomp)

(defparameter *runtime-heap-start* #x200000010)
(defparameter *compilation-start-address* *runtime-heap-start*)
(defparameter *rt-funs* (make-hash-table))


(defstruct compilation units)
(defparameter *current-compilation* (make-compilation :units nil))

(defparameter *start-address* *runtime-heap-start*)

(defun rt-%defun (name compilation-unit)
  ;; code loading will increase heap pointer
  (setf (gethash name *rt-funs*) *compilation-start-addres*)
  (setf *compilation-start-address* (+ *compilation-start-address* (get-compilation-unit-code-size compilation-unit))))

;; FIXME
;; for now we are only resolving FUNCTION calls
(defun resolve-fixup (fixup unit-code-buffer)
  (let* ((name (fun-rip-relative-name (rip-location-rip fixup)))
	 (offset (rip-location-byte-offset fixup))
	 (fun-address (rt-get-fun-address name)))
    (unless fun-address
      (error (format nil "Unknown fun ~a" name)))
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
    (dolist (buffer (nreverse buffers))
      (format s "~a" (apply #'concatenate 'string (mapcar #'byte-hex (coerce buffer 'list)))))
    s))

(defun rt-dump-binary (file)
  (let (code-buffers)
   (dolist (comp-unit (nreverse (compilation-units *current-compilation*)))
     (push (resolve-fixups comp-unit) code-buffers))
    (with-open-file (f file :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (dolist (c (immediate-as-byte-list *start-address* :imm64))
	(write-byte c f))
      (dolist (buffer (nreverse code-buffers))
	(format t "FOO: ~a~%" (apply #'concatenate 'string (mapcar #'byte-hex (coerce buffer 'list))))
	(loop for b across buffer
	      do
		 (write-byte b f)))
      (rt-reset))))

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